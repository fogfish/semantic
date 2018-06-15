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
   deduct/1,
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
-type lit()      :: canonical() | semantic().

%% canonical data types (Erlang built-in)
-type canonical():: atom() 
                  | binary() 
                  | float() 
                  | integer()
                  | boolean()
                  | byte()
                  | char().

%% semantic data types extension
-type semantic() :: geohash()
                  | datetime().

-type geohash()  :: binary().
-type datetime() :: {integer(), integer(), integer()}.

-type heap(X)    :: X | [X] | _. 

%%%------------------------------------------------------------------
%%%
%%% semantic public interface
%%%
%%%------------------------------------------------------------------

%%
%% start library RnD mode
start() ->
   applib:boot(?MODULE, []).


%%
%% encodes IRI to compact format
-spec compact(iri()) -> undefined | iri().

compact({iri, _, _} = IRI) ->
   IRI;
compact({iri,  Uri}) ->
   case semantic_ns_encode:q(undefined, Uri) of
      Uri ->
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
   case semantic_ns_decode:q(undefined, Prefix) of
      Prefix   ->
         undefined;
      Absolute ->
         {iri, <<Absolute/binary, Suffix/binary>>}
   end;
absolute(IRI) ->
   {iri, scalar:s(IRI)}.


%%
%% compiles knowledge statement into type-safe format
-spec typed( heap(spo()) ) -> heap(spock()).     

typed(Facts) ->
   semantic_typed:c(Facts).


%%
%% return Erlang native type of knowledge statement
-spec typeof(spock()) -> iri().

typeof(Fact) ->
   semantic_typed:native(Fact).


%%
%% deduct schema of knowledge statements using actual knowledge statements
-spec deduct( heap(spock()) ) -> heap(_).

deduct(Facts) ->
   semantic_schema:deduct(Facts).


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
%%
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




