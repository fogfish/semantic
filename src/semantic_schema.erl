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
-module(semantic_schema).
-include("semantic.hrl").

-export([
   deduct/1,
   define/1,
   define/2
]).


%%
%% deduct schema of knowledge statements
deduct({s, _, _} = Stream) ->
   gb_sets:to_list(
      stream:fold(fun deduct/2, gb_sets:new(), Stream)
   );

deduct([_|_] = List) ->
   gb_sets:to_list(
      lists:foldl(fun deduct/2, gb_sets:new(), List)
   ).

deduct(#{p := P, type := Type}, Set) ->
   gb_sets:union(gb_sets:from_list(define(P, Type)), Set);

deduct(#{p := P}, Set) ->
   gb_sets:union(gb_sets:from_list(define(P)), Set).



%%
%% define predicate meta-data
define(Predicate) ->
   [property(Predicate)].

define(Predicate, {iri, ?LANG, _} = Type) ->
   [property(Predicate), lang_string(Predicate), range(Predicate, Type)];

define(Predicate, Type) -> 
   [property(Predicate), range(Predicate, Type)].


%%
%%
property(Predicate) ->
   #{
      s => semantic:compact(Predicate),
      p => ?RDF_TYPE,
      o => ?RDF_PROPERTY,
      c => 1.0,
      k => uid:l()
   }.

%%
%%
lang_string(Predicate) ->
   #{
      s => semantic:compact(Predicate),
      p => ?RDF_RANGE,
      o => ?RDF_LANG_STRING,
      c => 1.0,
      k => uid:l()
   }.

%%
%%
range(Predicate, Type) ->
   #{
      s => semantic:compact(Predicate),
      p => ?RDF_RANGE,
      o => semantic:compact(Type),
      c => 1.0,
      k => uid:l()
   }.
