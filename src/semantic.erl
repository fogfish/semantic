%%
%%   Copyright 2012 - 2016 Dmitry Kolesnikov, All Rights Reserved
%%   Copyright 2016 Mario Cardona, All Rights Reserved
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
%%   semantic web toolkit
-module(semantic).

-include("semantic.hrl").
-include_lib("datum/include/datum.hrl").
-compile({parse_transform, category}).

-export([start/0]).
-export([
   compact/1,
   absolute/1,
   typed/1,
   typeof/1,
   native/1,
   to_json/1,
   to_text/1,
   schema/1,
   nt/1,
   jsonld/1,
   fold/1
]).


-export_type([spock/0, spo/0, iri/0]).

%%%------------------------------------------------------------------
%%%
%%% data types
%%%
%%%------------------------------------------------------------------

%% type-safe knowledge statement
%%   s: subject
%%   p: predicate
%%   o: object
%%   c: credibility
%%   k: k-order
-type spock()    :: #{s => s(), p => p(), o => o(), c => float(), k => uid:l(), type => iri()}.
-type s()        :: iri().
-type p()        :: iri().
-type o()        :: iri() | lit().

%% abstract syntax knowledge statement
-type spo()      :: {iri(), iri(), iri() | {iri(), binary()}}.


%% Internationalized Resource Identifiers [RFC3987]
-type iri()      :: absolute() | compact().
-type absolute() :: {iri, uri()}.
-type compact()  :: {iri, prefix(), suffix()}.
-type uri()      :: binary().
-type prefix()   :: binary().
-type suffix()   :: binary().

%% literal data types
-type lit()      :: xsd_string()
                  | xsd_integer()
                  | xsd_decimal()
                  | xsd_boolean()
                  | xsd_datetime()
                  | xsd_date()
                  | georss_point()
                  | georss_hash()
                  | georss_json()
                  .

-type xsd_string()   :: binary().
-type xsd_integer()  :: integer().
-type xsd_decimal()  :: float().
-type xsd_boolean()  :: boolean().
-type xsd_datetime() :: {integer(), integer(), integer()}.
-type xsd_date()     :: {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
-type georss_point() :: {lat(), lng()} | binary().
-type lat()          :: float().
-type lng()          :: float().
-type georss_hash()  :: binary().
-type georss_json()  :: #{}.

%% collections of facts
-type heap(X)    :: X | [X] | _. 

%%%------------------------------------------------------------------
%%%
%%% semantic toolkit interface
%%%
%%%------------------------------------------------------------------

%%
%% start library RnD mode
start() ->
   application:ensure_all_started(?MODULE).


%%
%% encodes IRI to compact format
-spec compact(iri()) -> undefined | iri().

compact({iri, _, _} = IRI) ->
   IRI;
compact({iri,  Uri}) ->
   case semantic_ns:decode(Uri) of
      {undefined, _} ->
         undefined;
      {Prefix, Suffix} ->
         {iri, Prefix, Suffix}
   end;
compact(IRI) ->
   case binary:split(scalar:s(IRI), <<"://">>) of
      [_, _] -> 
         compact({iri, IRI});
      _ ->
         case binary:split(scalar:s(IRI), <<$:>>) of
            [Prefix, Suffix] ->
               {iri, Prefix, Suffix};
            _ ->
               undefined
         end
   end.

%%
%% decodes IRI to absolute format
-spec absolute(iri()) -> undefined | iri().

absolute({iri, _} = IRI) ->
   IRI;
absolute({iri, Prefix, Suffix}) ->
   case semantic_ns:encode(Prefix) of
      undefined ->
         undefined;
      Absolute  ->
         {iri, <<Absolute/binary, Suffix/binary>>}
   end;
absolute(IRI) ->
   {iri, typecast:s(IRI)}.


%%
%% compiles knowledge statement into type-safe format
-spec typed( heap(spo()) ) -> heap(spock()).     

typed(Facts) ->
   semantic_heap:map(fun semantic_typed:compile/1, Facts).

%%
%% deduct semantic type from Erlang native term
-spec typeof(_) -> iri().

typeof(Term) ->
   semantic_typed:typeof(Term).

%%
%% deduct Erlang native type from knowledge statement
-spec native(spock() | spo()) -> iri().

native(Fact) ->
   semantic_typed:native(Fact).

%%
%% maps Erlang native term to json-format
-spec to_json(lit()) -> binary().

to_json(Lit) ->
   semantic_typed:to_json(Lit).

%%
%% maps Erlang native term to text-format
-spec to_text(lit()) -> binary().

to_text(Lit) ->
   semantic_typed:to_text(Lit).

%%
%% deduct schema of knowledge statements using actual knowledge statements
-spec schema( heap(spock()) ) -> heap(_).

schema(Facts) ->
   gb_sets:to_list(
      semantic_heap:fold(fun schema/2, gb_sets:new(), Facts)
   ).

schema(#{p := P, type := Type}, Set) ->
   {iri, _, _} = IRI = semantic:compact(P),
   {iri, _, _} = IsA = semantic:compact(Type),
   gb_sets:union(gb_sets:from_list([{IRI, IsA}]), Set);

schema(#{p := P}, Set) ->
   {iri, _, _} = IRI = semantic:compact(P),
   gb_sets:union(gb_sets:from_list([{IRI, ?XSD_ANYURI}]), Set);

schema({{iri, _}, {iri, _} = P, {iri, _}}, Set) ->
   {iri, _, _} = IRI = semantic:compact(P),
   gb_sets:union(gb_sets:from_list([{IRI, ?XSD_ANYURI}]), Set);

schema({{iri, _}, {iri, _} = P, {Type, _}}, Set) ->
   {iri, _, _} = IRI = semantic:compact(P),
   {iri, _, _} = IsA = semantic:compact(Type),
   gb_sets:union(gb_sets:from_list([{IRI, IsA}]), Set).

%%%------------------------------------------------------------------
%%%
%%% semantic codec
%%%
%%%------------------------------------------------------------------

%%
%% build stream of knowledge statements from n-triples.
-spec nt(datum:stream() | list()) -> datum:stream().

nt(#stream{} = Stream) ->
   semantic_nt:decode(Stream);

nt(File)
 when is_list(File) ->
   case filename:extension(File) of
      ".nt" -> nt(stdio:file(File, [{iobuf, 128 * 1024}]));
      ".gz" -> nt(gz:stream(stdio:file(File, [{iobuf, 128 * 1024}])))
   end;

nt(Blob)
 when is_binary(Blob) ->
   nt(stream:new(Blob)).

%%
%% build list of knowledge statements from json-ld
-spec jsonld(#{}) -> [spock()].

jsonld(JsonLD) ->
   semantic_jsonld:decode(JsonLD).

%%
%% reduces stream of triples by folding object to set
-spec fold(datum:stream()) -> datum:stream().

fold(#stream{} = Stream) ->
   stream:unfold(fun foldp/1, Stream).

foldp(Stream) ->
   foldp(stream:head(Stream), Stream).

foldp(_, ?stream()) ->
   stream:new();

foldp(#{s := S, p := P, type := Type} = Spock, Stream) ->
   {Head, Tail} = stream:splitwhile(
      fun(#{s := Sx, p := Px, type := TypeX}) ->
         S =:= Sx andalso P =:= Px andalso Type =:= TypeX
      end,
      Stream
   ),
   case [X || #{o := X} <- stream:list(Head)] of
      [O] ->
         {Spock#{o =>   O}, Tail};
      Set ->
         {Spock#{o => Set}, Tail}
   end.




