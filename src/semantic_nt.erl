%%
%%   Copyright 2012 - 2014 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   N-triples encode / decode 
%%      http://www.w3.org/TR/rdf11-new/
%%      http://www.w3.org/TR/n-triples/
%%
%%
%% [1]      ntriplesDoc ::=   triple? (EOL triple)* EOL?
%% [2]      triple      ::=   subject predicate object '.'
%% [3]      subject     ::=   IRIREF | BLANK_NODE
%% [4]      predicate   ::=   IRIREF
%% [5]      object      ::=   IRIREF | BLANK_NODE | literal
%% [6]      literal     ::=   STRING ('^^' IRIREF | LANGTAG)?
%%
%%
%% [144s]   LANGTAG     ::=   '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
%% [7]      EOL         ::=   [#xD#xA]+
%% [8]      IRIREF      ::=   '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
%% [9]      STRING      ::=   '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%% [141s]   BLANK_NODE  ::=   '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
%% [10]     UCHAR       ::=   '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
%% [153s]   ECHAR       ::=   '\' [tbnrf"'\]
%% [157s]   PN_CHARS_BASE ::=   [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
%% [158s]   PN_CHARS_U  ::=   PN_CHARS_BASE | '_' | ':'
%% [160s]   PN_CHARS    ::=   PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
%% [162s]   HEX         ::=   [0-9] | [A-F] | [a-f]
%%
%%
-module(semantic_nt).

-include("semantic.hrl").
-include_lib("datum/include/datum.hrl").

-export([
   new/0
  ,decode/1
  ,decode/2
  ,encode/1
  ,encode/2
]).

%%
%% parser state
-record(nt, {
   recbuf = <<>> :: binary() %% internal receive buffer
}).


%% grammar  
-define(WS,  [<<$ >>, <<$\t>>, <<$\n>>]).
-define(EOL, <<$.,$\n>>).

%%
%% create new triple parser
-spec new() -> #nt{}.

new() ->
   #nt{}.

%%
%% stream decoder
-spec decode(datum:stream()) -> datum:stream().

decode(#stream{} = Stream) ->
   istream(Stream, new()).

istream(?stream(),    _State) ->
   stream:new();
istream(Stream, State) ->
   istream(stream:head(Stream), Stream, State).

istream(Head, Stream, State0)
 when is_binary(Head) ->
   {NT, State} = decode(Head, State0),
   istream(NT, Stream, State);

istream([], Stream, State) ->
   istream(stream:tail(Stream), State);
istream([Head|Tail], Stream, State) ->
   stream:new(Head, fun() -> istream(Tail, Stream, State) end).

%%
%% stream encoder
-spec encode(datum:stream()) -> datum:stream().

encode(#stream{} = Stream) ->
   ostream(Stream, new()).

ostream(?stream(),    _State) ->
   stream:new();
ostream(Stream, State) ->
   ostream(stream:head(Stream), Stream, State).

ostream(Head, Stream, State0)
 when is_tuple(Head) ->
   {NT, State} = encode(Head, State0),
   ostream(NT, Stream, State);

ostream([], Stream, State) ->
   ostream(stream:tail(Stream), State);
ostream([Head|Tail], Stream, State) ->
   stream:new(Head, fun() -> ostream(Tail, Stream, State) end).



%%%------------------------------------------------------------------
%%%
%%% decoder
%%%
%%%------------------------------------------------------------------

%%
%% decode stream of triples,
%% returns parsed values and new parser state.
-spec decode(binary(), #nt{}) -> {[semantic:spo()], #nt{}}.

decode(Chunk, #nt{recbuf = <<>>}=State)
 when is_binary(Chunk) ->
   decode(Chunk, [], State);

decode(Chunk, State)
 when is_binary(Chunk) ->
   decode(iolist_to_binary([State#nt.recbuf, Chunk]), [], State#nt{recbuf = <<>>}).

decode(Chunk, Acc, State) ->
   case decode_t(Chunk) of
      %% unable to parse
      undefined   ->
         {lists:reverse(Acc), State#nt{recbuf = Chunk}};
      %% got a triple
      {SPO, Tail} ->
         decode(Tail, [SPO | Acc], State)
   end.

%%
%%
decode_t(<<$#, X0/binary>>) ->
   % skip comment line
   case binary:split(X0, <<$\n>>) of
      [_, X1] ->
         decode_t(X1);
      _       ->
         undefined
   end;

decode_t(X0) ->
   try
      {S, X1} = decode_s(X0),
      {P, X2} = decode_p(X1),
      {O, X3} = decode_o(X2),
      case binary:split(X3, ?EOL) of
         [_, X4] ->
            {{S, P, O}, X4};
         _       ->
            undefined
      end
   catch throw:badarg ->
      undefined
   end.

%%
%%
decode_s(X) ->
   case split(X, ?WS) of
      %% blanked node
      {<<$_, $:, Y/binary>>, Tail} ->
         {{iri, Y}, Tail};

      %% not enough data to parse statement
      {<<>>, _Tail} ->
         throw(badarg);

      %% Node identity is uri
      {Head, Tail} ->
         {Uri,    _} = unquote(Head, <<$<>>, <<$>>>),
         {{iri, Uri}, Tail}
   end.

%%
%%
decode_p(<<$", _/binary>> = X) ->
   {Head, Tail} = unquote(X, <<$">>, <<$">>),
   case binary:split(Tail, ?WS) of
      [Rest] ->
         {Head, Rest};
      [_, Rest] ->
         {Head, Rest}
   end;

decode_p(X) ->
   case split(X, ?WS) of
      {<<$_, $:, Y/binary>>, Tail} ->
         {{iri, Y}, Tail};
      {<<>>,_Tail} ->
         throw(badarg);
      {Head, Tail} -> 
         {Uri,     _} = unquote(Head, <<$<>>, <<$>>>),
         {{iri, Uri},  Tail}
   end.

%%
%%
decode_o(<<$_, $:, Y/binary>>) ->
   {Head, Tail} = split(Y, ?WS),
   case split(Head, [<<$@>>]) of 
      {_,  <<>>} ->
         {{iri, Head}, Tail};
      {Urn, Tag} ->
         {{Tag,  Urn}, Tail}
   end;

decode_o(<<$<, _/binary>>=X) ->
   {Uri, Tail} = unquote(X, <<$<>>, <<$>>>),
   {{iri, Uri}, Tail};

decode_o(<<$", _/binary>>=X) ->
   case unquote(X, <<$">>, <<$">>) of
      {Head, <<$@, Rest/binary>>} ->
         {Lang, Tail} = skip(Rest, ?EOL),
         {{{iri, ?LANG, Lang}, Head}, Tail};

      {Head, <<$^, $^, Rest/binary>>} ->
         case Rest of
            <<$<, _/binary>> ->
               {Type, Tail} = unquote(Rest, <<$<>>, <<$>>>),
               {{{iri, Type}, Head},  Tail};
            _ ->
               {Type, Tail} = split(Rest, <<$ >>),
               {{{iri, Type}, Head},  Tail}
         end;

      {_Head, _Tail} = Result ->
         Result
   end; 

decode_o(_) ->
   throw(badarg).


%%%------------------------------------------------------------------
%%%
%%% encoder
%%%
%%%------------------------------------------------------------------

encode(Stmt, State)
 when is_list(Stmt) ->
   {encode_t(Stmt), State};

encode(Stmt, State) ->
   {encode_t([Stmt]), State}.

encode_t([{{iri, S}, {iri, P}, {iri, O}} | Tail]) ->
   X = <<$<, S/binary, $>, $ , $<, P/binary, $>, $ , $<, O/binary, $>, $ , $., $\n>>,
   [X | encode_t(Tail)];

encode_t([{{iri, S}, {iri, P}, {{iri, ?LANG, Lang}, O}} | Tail]) ->
   X = <<$<, S/binary, $>, $ , $<, P/binary, $>, $ , $", O/binary, $", $@, Lang/binary, $ , $., $\n>>,
   [X | encode_t(Tail)];

encode_t([{{iri, S}, {iri, P}, {{iri, Type}, O}} | Tail]) ->
   X = <<$<, S/binary, $>, $ , $<, P/binary, $>, $ , $", O/binary, $", $^, $^, $<, Type/binary, $>, $ , $., $\n>>,
   [X | encode_t(Tail)];

encode_t([{{iri, S}, {iri, P}, O} | Tail]) when is_binary(O) ->
   X = <<$<, S/binary, $>, $ , $<, P/binary, $>, $ , $", O/binary, $", $ , $., $\n>>,
   [X | encode_t(Tail)];

encode_t([]) ->
   [].  

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% split binary, return head and tail
split(Bin, Pat) ->
   case binary:split(Bin, Pat) of
      [<<>>, X] ->
         split(X, Pat);   
      [X] ->
         {X, <<>>};
      [H, T] ->
         {H, T}
   end.

%%
%% skip binary to pattern
skip(Bin, Pat) ->
   case binary:match(Bin, Pat) of
      {0, _} ->
         {<<>>, Bin};
      {A, _} ->
         {binary:part(Bin, 0, A - 1), binary:part(Bin, A, size(Bin) - A)};
      _      ->
         throw(badarg)
   end.

%%
%% unquote binary 
%% returns error if quote pair do not exists
unquote(Bin, Qa, Qb) ->
   case binary:split(Bin,  Qa) of
      [_, X] ->
         case match(X, Qb) of
            nomatch ->
               throw(badarg);
            At      ->
               H = binary:part(X, 0, At),
               T = binary:part(X, At + 1, byte_size(X) - At - 1),
               {H, T}
         end;
      [X] ->
         {<<>>, X}
   end.

%%
%% match pattern and skip escape
match(Bin, Pat) ->
   match(0, byte_size(Bin), Bin, Pat).   

match(I, L, Bin, Pat) ->
   case binary:match(Bin, Pat, [{scope, {I, L}}]) of
      nomatch ->
         nomatch;
      {I,  _} ->
         I;

      {X,  _} ->
         case is_escaped(Bin, X) of
            true  ->
               X;
            false ->
               match(X + 1, byte_size(Bin) - X - 1, Bin, Pat)
         end
   end.

is_escaped(Bin, X) ->
   is_escaped(binary:at(Bin, X - 1), Bin, X, 2).
is_escaped($\\, Bin, X, I)
 when X >= I ->
   is_escaped(binary:at(Bin, X - I), Bin, X, I + 1);
is_escaped($\\, _, _, I) ->
   I rem 2 == 1;
is_escaped(_, _, _, I) ->
   (I - 1) rem 2 == 1.
