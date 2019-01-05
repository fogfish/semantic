%%
%%   Copyright 2018 Dmitry Kolesnikov, All Rights Reserved
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
%%   in-memory heap of knowledge statements
-module(semantic_heap).

-include_lib("datum/include/datum.hrl").

-export([map/2, fold/3]).

%%
%%
map(Fun, #{s := _, p := _, o := _, type := _} = Spock) ->
   Fun(Spock);

map(Fun, {{iri, _}, {iri, _}, _} = Spo) ->
   Fun(Spo);

map(Fun, #stream{} = Stream) ->
   stream:map(Fun, Stream);

map(Fun, [_|_] = List) ->
   lists:map(Fun, List).


%%
%%
fold(Fun, Acc, #{s := _, p := _, o := _, type := _} = Spock) ->
   Fun(Spock, Acc);

fold(Fun, Acc, {{iri, _}, {iri, _}, _} = Spo) ->
   Fun(Spo, Acc);

fold(Fun, Acc, {s, _, _} = Stream) ->
   stream:fold(Fun, Acc, Stream);

fold(Fun, Acc, [_|_] = List) ->
   lists:foldl(Fun, Acc, List).
