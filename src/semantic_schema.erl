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
-include_lib("datum/include/datum.hrl").

-export([
   deduct/1
]).


%%
%% deduct schema of knowledge statements
-spec deduct(semantic:heap(semantic:spock())) -> semantic:heap(_).

deduct(#stream{} = Stream) ->
   gb_sets:to_list(
      stream:fold(fun deduct/2, gb_sets:new(), Stream)
   );

deduct([_|_] = List) ->
   gb_sets:to_list(
      lists:foldl(fun deduct/2, gb_sets:new(), List)
   ).

deduct(#{p := P, type := Type}, Set) ->
   {iri, _, _} = IRI = semantic:compact(P),
   {iri, _, _} = IsA = semantic:compact(Type),
   gb_sets:union(gb_sets:from_list([{IRI, IsA}]), Set);

deduct(#{p := P}, Set) ->
   {iri, _, _} = IRI = semantic:compact(P),
   gb_sets:union(gb_sets:from_list([{IRI, ?XSD_ANYURI}]), Set).

