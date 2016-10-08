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
-module(semantic_jsonld).
-include("semantic.hrl").

-export([
   decode/1
]).

%%
%% 
decode(#{<<"@context">> := Context, <<"@id">> := Node} = JsonLD) -> 
   decode(Context, Node, maps:without([<<"@context">>, <<"@id">>], JsonLD)).

decode(Context, Node, JsonLD) ->
   lists:flatten(
      maps:fold(
         fun(Key, Val, Acc) ->
            Uri  = context(Key, Context),
            Type = typeof(Key, Val, Context),
            [spo(Node, Uri, Type, Val) | Acc]
         end,
         [],
         JsonLD
      )
   ).

spo(S, P, Type, O)
 when is_list(Type), is_list(O) ->
   lists:map(
      fun({Tx, Ox}) -> spo(S, P, Tx, Ox) end,
      lists:zip(Type, O) 
   );

spo(S, P, uri, O) ->
   {{iri, S}, {iri, P}, {iri, O}};

spo(S, P, undefined, O) ->
   {{iri, S}, {iri, P}, O};

spo(S, P, Type, O) ->
   {{iri, S}, {iri, P}, {Type, O}}.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%%
%% @todo: prefix expansion
context(Key, Context) ->
   case Context of
      % context term definition 
      #{Key := Uri} when is_binary(Uri) ->
         Uri;

      % expanded term definition
      #{Key := #{<<"@id">> := Uri}} ->
         Uri;

      % key is not defined at context
      _ ->
         Key
   end.

typeof(Key, Val, Context)
 when is_list(Val) ->
   lists:map(
      fun(X) -> typeof(Key, X, Context) end,
      Val
   );

typeof(Key, Val, Context) ->
   case Context of
      % expanded term definition
      #{Key := #{<<"@type">> := <<"@id">>}} ->
         undefined;

      #{Key := #{<<"@type">> := Type}} ->
         Type;

      % key is not defined at context
      _ ->
         typeof(Val)
   end.


typeof(Val)
 when is_binary(Val) -> 
   {iri, <<"http://www.w3.org/2001/XMLSchema#string">>};

typeof(Val)
 when is_integer(Val) -> 
   {iri, <<"http://www.w3.org/2001/XMLSchema#integer">>};

typeof(Val)
 when is_float(Val) -> 
   {iri, <<"http://www.w3.org/2001/XMLSchema#double">>};

typeof(true) ->
   {iri, <<"http://www.w3.org/2001/XMLSchema#boolean">>};

typeof(false)  -> 
   {iri, <<"http://www.w3.org/2001/XMLSchema#boolean">>}.
