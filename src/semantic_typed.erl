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
%%   compile abstract syntax triple to type-safe representation
-module(semantic_typed).
-include("semantic.hrl").

-export([
   % typeof/1,
   c/1
]).


%%
%%
typeof(#{type := geohash}) -> geohash;
typeof(#{type := Type})  -> Type;

typeof(#{o := {uri, _}}) -> rel;
typeof(#{o := X}) when is_integer(X) -> integer;
typeof(#{o := X}) when is_float(X)   -> float;
typeof(#{o := X}) when is_boolean(X) -> boolean;
typeof(#{o := {_, _, _}}) -> datetime;
typeof(#{o := X}) when is_binary(X) -> binary;

typeof(binary)   -> {uri, <<"xsd:string">>};
typeof(integer)  -> {uri, <<"xsd:integer">>};
typeof(float)    -> {uri, <<"xsd:double">>};
typeof(boolean)  -> {uri, <<"xsd:boolean">>};
typeof(datetime) -> {uri, <<"xsd:dateTime">>};
typeof(geohash)  -> {uri, <<"georss:point">>}.


%%
%% compile abstract syntax triple to type-safe
c({{iri, _} = S, {iri, _} = P, {iri, _} = O}) ->
   #{
      s => semantic:compact(S),
      p => semantic:compact(P),
      o => semantic:compact(O),
      c => 1.0,
      k => uid:l()
   };

c({{iri, _} = S, {iri, _} = P, {{iri, ?LANG, _} = Type, O}}) ->
   #{
      s => semantic:compact(S),
      p => semantic:compact(P),
      o => O,
      c => 1.0,
      k => uid:l(),
      type => Type
   };

c({{iri, _} = S, {iri, _} = P, {{iri, _} = Type, O}}) ->
   decode(semantic:compact(Type), O,
      #{
         s => semantic:compact(S),
         p => semantic:compact(P),
         c => 1.0,
         k => uid:l()
      }
   );

c({{iri, _} = S, {iri, _} = P, O}) ->
   decode(O,
      #{
         s => semantic:compact(S),
         p => semantic:compact(P),
         c => 1.0,
         k => uid:l()
      }
   ).


%%
%% type-safe decode
decode(?STRING = Type, O, Fact) ->
   Fact#{o => O, type => Type};

%%
decode(?INTEGER = Type, O, Fact) -> 
   Fact#{o => scalar:i(O), type => Type};

%%
decode(?BYTE = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?SHORT = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?INT = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?LONG = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};


%%
decode(?DECIMAL = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

decode(?FLOAT = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

decode(?DOUBLE = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

%%
decode(?BOOLEAN = Type, <<"true">>, Fact) ->
   Fact#{o => true, type => Type};

decode(?BOOLEAN = Type, <<"false">>, Fact) ->
   Fact#{o => false, type => Type};

decode(?BOOLEAN = Type, <<"1">>, Fact) ->
   Fact#{o => true, type => Type};

decode(?BOOLEAN = Type, <<"0">>, Fact) ->
   Fact#{o => false, type => Type};

%%
decode(?DATETIME = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?DATE = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?TIME = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?YEARMONTH = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?YEAR = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?MONTHDAY = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?MONTH = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?DAY = Type, O, Fact) ->
   Fact#{o => O, type => Type};

%%
decode(?GEOHASH = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?GEOPOINT = Type, O, Fact) ->
   [Lat, Lng] = binary:split(O, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => Type}. 


%%
%% relax decode
decode(<<"true">>, Fact) ->
   Fact#{o => true,  type => ?BOOLEAN};

decode(<<"false">>, Fact) ->
   Fact#{o => false, type => ?BOOLEAN};

decode(LatLng, #{p := {iri, <<"georss">>, <<"point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => ?GEOPOINT}; 

decode(LatLng, #{p := {iri, <<"http://www.georss.org/georss/point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => ?GEOPOINT};

decode(O, Fact) ->
   case scalar:decode(O) of
      X when is_integer(X) ->
         Fact#{o => X, type => ?INTEGER};
      X when is_float(X) ->
         Fact#{o => X, type => ?DOUBLE};
      X when is_boolean(X) ->
         Fact#{o => X, type => ?BOOLEAN};
      X when is_binary(X) ->
         Fact#{o => X, type => ?STRING}
   end.
