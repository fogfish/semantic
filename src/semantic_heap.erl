%% @doc
%%   in-memory heap of knowledge statements
-module(semantic_heap).

-export([map/2, fold/3]).

%%
%%
map(Fun, #{s := _, p := _, o := _, type := _} = Spock) ->
   Fun(Spock);

map(Fun, {{iri, _}, {iri, _}, _} = Spo) ->
   Fun(Spo);

map(Fun, {s, _, _} = Stream) ->
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
   lists:fold(Fun, Acc, List).
