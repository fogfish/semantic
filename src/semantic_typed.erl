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

-export([
   typeof/1,
   c/2
]).

%%
%%
typeof(#{type := geohash}) -> geohash;
typeof(#{type := Type})  -> Type;

typeof(#{o := {uri, _}}) -> uri;
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
%%
c(Prefix, {{uri, S}, {uri, P}, {uri, O}}) ->
   #{
      s => {uri, prefix(Prefix, S)},
      p => {uri, prefix(Prefix, P)},
      o => {uri, prefix(Prefix, O)}
   };

c(Prefix, {{uri, S}, {uri, P}, O}) ->
   decode(O, #{
      s => {uri, prefix(Prefix, S)},
      p => {uri, prefix(Prefix, P)}
   }).

%%
%%
decode({<<"http://www.w3.org/2001/XMLSchema#string">>, O}, Fact) ->
   Fact#{o => O};

%%
decode({<<"http://www.w3.org/2001/XMLSchema#integer">>, O}, Fact) ->
   Fact#{o => scalar:i(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#long">>, O}, Fact) ->
   Fact#{o => scalar:i(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#int">>, O}, Fact) ->
   Fact#{o => scalar:i(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#short">>, O}, Fact) ->
   Fact#{o => scalar:i(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#byte">>, O}, Fact) ->
   Fact#{o => scalar:i(O)};

%%
decode({<<"http://www.w3.org/2001/XMLSchema#decimal">>, O}, Fact) ->
   Fact#{o => scalar:f(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#float">>, O}, Fact) ->
   Fact#{o => scalar:f(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#double">>, O}, Fact) ->
   Fact#{o => scalar:f(O)};

%%
decode({<<"http://www.w3.org/2001/XMLSchema#boolean">>, <<"true">>}, Fact) ->
   Fact#{o => true};

decode({<<"http://www.w3.org/2001/XMLSchema#boolean">>, <<"false">>}, Fact) ->
   Fact#{o => false};

decode({<<"http://www.w3.org/2001/XMLSchema#boolean">>, <<"1">>}, Fact) ->
   Fact#{o => true};

decode({<<"http://www.w3.org/2001/XMLSchema#boolean">>, <<"0">>}, Fact) ->
   Fact#{o => false};

%%
decode({<<"http://www.w3.org/2001/XMLSchema#dateTime">>, O}, Fact) ->
   Fact#{o => tempus:iso8601(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#date">>, O}, Fact) ->
   Fact#{o => tempus:iso8601(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#time">>, O}, Fact) ->
   Fact#{o => tempus:iso8601(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#gYearMonth">>, O}, Fact) ->
   Fact#{o => tempus:iso8601(O)};

decode({<<"http://www.w3.org/2001/XMLSchema#gYear">>, O}, Fact) ->
   Fact#{o => tempus:iso8601(O)};

%%
decode({<<_:16>> = Lang, O}, Fact) ->
   Fact#{o => O, type => Lang};

decode({<<_:16, $-, _:16>> = Lang, O}, Fact) ->
   Fact#{o => O, type => Lang};

%%
decode(LatLng, #{p := {uri, <<"georss:point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => geohash}; 

decode(LatLng, #{p := {uri, <<"http://www.georss.org/georss/point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => geohash}.


%%
%%
prefix(Encoder, Uri) ->
   Encoder:q(undefined, Uri).
