-module(semantic_jsonld).

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
   {{uri, S}, {uri, P}, {uri, O}};

spo(S, P, undefined, O) ->
   {{uri, S}, {uri, P}, O};

spo(S, P, Type, O) ->
   {{uri, S}, {uri, P}, {Type, O}}.

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
         uri;

      #{Key := #{<<"@type">> := Type}} ->
         Type;

      % key is not defined at context
      _ ->
         typeof(Val)
   end.


typeof(Val) when is_binary(Val) -> <<"http://www.w3.org/2001/XMLSchema#string">>;
typeof(Val) when is_integer(Val) -> <<"http://www.w3.org/2001/XMLSchema#integer">>;
typeof(Val) when is_float(Val) -> <<"http://www.w3.org/2001/XMLSchema#double">>;
typeof(true)  -> <<"http://www.w3.org/2001/XMLSchema#boolean">>;
typeof(false)  -> <<"http://www.w3.org/2001/XMLSchema#boolean">>.

