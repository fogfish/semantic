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

-export([start/0]).
-export([
   compact/1,
   absolute/1,
   typed/1
]).



-export([
   prefix/1,
   define/2
]).
-export([
   % typed/2,
   typeof/1,
   schema/1,
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
-spec compact(iri()) -> iri().

compact({iri, _, _} = IRI) ->
   IRI;
compact({iri,  Uri} = IRI) ->
   case semantic_ns_encode:q(undefined, Uri) of
      Uri ->
         IRI;
      {Prefix, Suffix} ->
         {iri, Prefix, Suffix}
   end.


%%
%% decodes IRI to absolute format
-spec absolute(iri()) -> iri().

absolute({iri, _} = IRI) ->
   IRI;
absolute({iri, Prefix, Suffix} = IRI) ->
   case semantic_ns_decode:q(undefined, Prefix) of
      Prefix   ->
         IRI;
      Absolute ->
         {iri, <<Absolute/binary, Suffix/binary>>}
   end.


%%
%% compiles knowledge statement into type-safe format
-spec typed( heap(spo()) ) -> heap(spock()).     

typed(Fact) ->
   semantic_typed:c(Fact).


%%
%% map abstract knowledge statement to Erlang native representation 
% -spec typed(datum:stream() | _) -> spock().
% -spec typed(atom(), _) -> spock().

% typed({s, _, _} = Stream) ->
%    stream:map(
%       fun(X) -> 
%          semantic_typed:c(semantic_ns_encode, X) 
%       end, 
%       Stream
%    );

% typed([_|_] = List) ->
%    lists:map(
%       fun(X) ->
%          semantic_typed:c(semantic_ns_encode, X) 
%       end,
%       List
%    );

% typed(Fact) ->
%    semantic_typed:c(semantic_ns_encode, Fact).

% typed(Prefix, Fact) ->
%    semantic_typed:c(Prefix, Fact).





%%%------------------------------------------------------------------
%%%
%%% schema interface
%%%
%%%------------------------------------------------------------------

%%
%% encode uri prefix
-spec prefix(binary() | uri()) -> uri().

prefix({uri, Uri}) ->
   Uri;
prefix(Uri) ->
   semantic_ns_encode:q(undefined, Uri).

%%
%% define predicate meta-data
% -spec define(binary(), type() | lang()) -> [spo()].

define(Predicate, rel) ->
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
%% return type of knowledge statement
% -spec typeof(spo()) -> lang() | type().

typeof(Fact) ->
   semantic_typed:typeof(Fact).

%%%------------------------------------------------------------------
%%%
%%% intake interface
%%%
%%%------------------------------------------------------------------

%%
%% build schema of knowledge statements
-spec schema(datum:stream()) -> [spock()].

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
-spec jsonld(#{}) -> [spock()].

jsonld(JsonLD) ->
   semantic_jsonld:decode(JsonLD).

