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
-compile({parse_transform, category}).

-export([start/0]).
-export([
   compact/1,
   absolute/1,
   typed/1,
   typeof/1,
   p/1,
   p/2,
   seq/2,
   deduct/1,
   schema/1,
   create/1,
   lookup/1,
   nt/1,
   jsonld/1
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
   case binary:split(scalar:s(IRI), <<$:>>) of
      [Prefix, Suffix] -> 
         {iri, Prefix, Suffix};
      _ ->
         undefined
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
%% define new predicate
-spec p(#rdf_property{}) -> #rdf_property{}.
-spec p(semantic:iri(), semantic:iri()) -> #rdf_property{}. 

p(Spec) ->
   semantic_schema:property(Spec).

p(P, DataType) ->
   semantic_schema:property(P, DataType).


%%
%% define new seq
-spec seq(semantic:iri(), [semantic:iri()]) -> #rdf_seq{}.

seq(IRI, Properties) ->
   {iri, _, _} = Seq = semantic:compact(IRI),
   #rdf_seq{
      id  = Seq,
      seq = [lookup(X) || X <- Properties]
   }.

%%
%% deduct schema of knowledge statements using actual knowledge statements
-spec deduct( heap(spock()) ) -> heap(#rdf_property{}).

deduct(Facts) ->
   semantic_schema:deduct(Facts).

%%
%% define new property
-spec schema(#rdf_property{}) -> [spock()].

schema(P) ->
   semantic_schema:define(P).

%%
%%
-spec create(#rdf_property{} | #rdf_seq{}) -> true | false.

create(#rdf_property{} = Property) ->
   ets:insert_new(semantic, Property);

create(#rdf_seq{} = Seq) ->
   ets:insert_new(semantic, Seq).


-spec lookup(iri()) -> undefined | #rdf_property{}.

lookup({iri, _, _} = IRI) ->
   case ets:lookup(semantic, IRI) of
      [#rdf_property{} = Property] ->
         Property;
      [#rdf_seq{} = Seq] ->
         Seq;
      _ ->
         undefined
   end;

lookup(IRI) ->
   [$? || compact(IRI), lookup(_)]. 


%%
%% build stream of knowledge statements from n-triples.
-spec nt(datum:stream() | list()) -> datum:stream().

nt({s, _, _} = Stream) ->
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

