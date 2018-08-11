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
decode(#{<<"@context">> := Context} = JsonLD) -> 
   decode(Context, semantic:absolute(id(JsonLD)), 
      maps:without([<<"@context">>, <<"@id">>, <<"rdf:id">>, <<"id">>], JsonLD)
   );

decode(#{} = JsonLD) ->
   decode(#{}, semantic:absolute(id(JsonLD)), 
      maps:without([<<"@context">>, <<"@id">>, <<"rdf:id">>, <<"id">>], JsonLD)
   ).

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

id(#{<<"id">> := Id}) -> Id;
id(#{<<"@id">> := Id}) -> Id;
id(#{<<"rdf:id">> := Id}) -> Id.

spo(S, P, Type, O)
 when is_list(Type), is_list(O) ->
   lists:map(
      fun({Tx, Ox}) -> spo(S, P, Tx, Ox) end,
      lists:zip(Type, O) 
   );

spo(S, P, Type, O) ->
   {S, P, {Type, O}}.

% spo(S, P, rel, O) ->
%    {{iri, S}, {iri, P}, {iri, O}};

% spo(S, P, undefined, O) ->
%    {{iri, S}, {iri, P}, O};

% spo(S, P, Type, O) ->
%    {{iri, S}, {iri, P}, {Type, O}}.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%%
context(Key, Context) ->
   case Context of
      % context term definition 
      #{Key := Uri} when is_binary(Uri) ->
         semantic:absolute(Uri);

      % expanded term definition
      #{Key := #{<<"@id">> := Uri}} ->
         semantic:absolute(Uri);

      % key is not defined at context
      _ ->
         semantic:absolute(Key)
   end.

%%
%%
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
         ?XSD_ANYURI;

      #{Key := #{<<"@type">> := Type}} ->
         case binary:split(Type, <<"://">>) of
            [_, _] ->
               semantic:absolute(Type);
            [_] ->
               semantic:compact(Type)
         end;

      % key is not defined at context
      _ ->
         typeof(Val)
   end.


typeof(Val) when is_binary(Val) -> ?XSD_STRING;
typeof(Val) when is_integer(Val) -> ?XSD_INTEGER; 
typeof(Val) when is_float(Val) -> ?XSD_DOUBLE;
typeof(true) -> ?XSD_BOOLEAN;
typeof(false) -> ?XSD_BOOLEAN. 
