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
%%   semantic web toolkit
-module(semantic).

-export([
   prefixes/0,
   prefixes/1,
   prefix/1,
   define/2
]).
-export([
   typed/1,
   typed/2,
   typeof/1,
   schema/1,
   nt/1,
   jsonld/1
]).

-export_type([spo/0]).

%%
%% data types
-type spo()  :: #{s => s(), p => p(), o => o(), type => lang() | type()}.
-type s()    :: uri().
-type p()    :: uri().
-type o()    :: uri() | lit().
-type uri()  :: {uri, binary()}.
-type lit()  :: binary() | integer() | float() | boolean() | {_, _, _}.
-type lang() :: binary().
-type type() :: integer | float | boolean | datetime | binary | geohash | uri.

%%%------------------------------------------------------------------
%%%
%%% schema interface
%%%
%%%------------------------------------------------------------------

%%
%% configure built-in namespace prefixes 
-spec prefixes() -> ok.

prefixes() ->
   prefixes( filename:join([code:priv_dir(?MODULE), "prefixes.nt"]) ).

prefixes(File) ->
   prefixes(semantic_ns_encode, semantic_ns_decode, 
      semantic_nt:decode(
         stdio:file(File)
      )
   ).

prefixes(Enc, Dec, {s, _, _} = Kns) ->
   prefixes(Enc, Dec, stream:list(Kns));

prefixes(Enc, Dec, Kns)
 when is_list(Kns) ->
   {module, Enc} = semantic_ns:encoder(Enc, Kns),
   {module, Dec} = semantic_ns:decoder(Dec, Kns),
   ok.

%%
%% encode uri prefix
prefix({uri, Uri}) ->
   Uri;
prefix(Uri) ->
   semantic_ns_encode:q(undefined, Uri).

%%
%% define predicate meta-data
-spec define(binary(), type() | lang()) -> [spo()].

define(Predicate, uri) ->
   [
      #{s => {uri, prefix(Predicate)}, p => {uri, <<"rdf:type">>},  o => {uri, <<"rdf:Property">>}}
   ];
define(Predicate, Type)
 when is_atom(Type) ->
   [
      #{s => {uri, prefix(Predicate)}, p => {uri, <<"rdf:type">>},  o => {uri, <<"rdf:Property">>}},
      #{s => {uri, prefix(Predicate)}, p => {uri, <<"rdf:range">>}, o => semantic_typed:typeof(Type)}
   ];
define(Predicate, Lang)
 when is_binary(Lang) ->
   [
      #{s => {uri, prefix(Predicate)}, p => {uri, <<"rdf:type">>},  o => {uri, <<"rdf:Property">>}},
      #{s => {uri, prefix(Predicate)}, p => {uri, <<"rdf:range">>}, o => {uri, <<"rdf:langString">>}},
      #{s => {uri, prefix(Predicate)}, p => {uri, <<"rdf:range">>}, o => Lang}
   ].



%%%------------------------------------------------------------------
%%%
%%% type-safe triple interface
%%%
%%%------------------------------------------------------------------
   
%%
%% map abstract knowledge statement to Erlang native representation 
-spec typed(datum:stream() | _) -> spo().
-spec typed(atom(), _) -> spo().

typed({s, _, _} = Stream) ->
   stream:map(
      fun(X) -> 
         semantic_typed:c(semantic_ns_encode, X) 
      end, 
      Stream
   );

typed([_|_] = List) ->
   lists:map(
      fun(X) ->
         semantic_typed:c(semantic_ns_encode, X) 
      end,
      List
   );

typed(Fact) ->
   semantic_typed:c(semantic_ns_encode, Fact).

typed(Prefix, Fact) ->
   semantic_typed:c(Prefix, Fact).


%%
%% return type of knowledge statement
-spec typeof(spo()) -> lang() | type().

typeof(Fact) ->
   semantic_typed:typeof(Fact).

%%%------------------------------------------------------------------
%%%
%%% intake interface
%%%
%%%------------------------------------------------------------------

%%
%% build schema of knowledge statements
-spec schema(datum:stream()) -> [spo()].

schema({s, _, _} = Stream) ->
   gb_sets:to_list(
      stream:fold(fun schema/2, gb_sets:new(), Stream)
   ).

schema(#{p := P} = Fact, Set) ->
   Type = semantic:typeof(Fact),
   gb_sets:union(gb_sets:from_list(define(P, Type)), Set).

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
-spec jsonld(#{}) -> [spo()].

jsonld(JsonLD) ->
   semantic_jsonld:decode(JsonLD).

