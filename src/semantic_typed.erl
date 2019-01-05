%%
%%   Copyright 2012 - 2014 Dmitry Kolesnikov, All Rights Reserved
%%   Copyright 2016 Mario Cardona, All Rights Reserved
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
   typeof/1
,  native/1
,  compile/1
]).

%%
%% deduct semantic type from Erlang native term
typeof({iri, _}) -> ?XSD_ANYURI;
typeof({iri, _, _}) -> ?XSD_ANYURI;
typeof(X) when is_binary(X) -> ?XSD_STRING;
typeof(X) when is_integer(X) -> ?XSD_INTEGER;
typeof(X) when is_float(X) -> ?XSD_DECIMAL;
typeof(true) -> ?XSD_BOOLEAN;
typeof(false) -> ?XSD_BOOLEAN;
typeof({A, B, C}) when is_integer(A), is_integer(B), is_integer(C) -> ?XSD_DATETIME;
typeof({{Y, 0, 0}, {0, 0, 0}}) when is_integer(Y), Y > 0 -> ?XSD_YEAR;
typeof({{0, M, 0}, {0, 0, 0}}) when is_integer(M), M > 0 -> ?XSD_MONTH;
typeof({{0, 0, D}, {0, 0, 0}}) when is_integer(D), D > 0 -> ?XSD_DAY;
typeof({{Y, M, 0}, {0, 0, 0}}) when is_integer(Y), Y > 0, is_integer(M), M > 0 -> ?XSD_YEARMONTH;
typeof({{0, M, D}, {0, 0, 0}}) when is_integer(M), M > 0, is_integer(D), D > 0 -> ?XSD_MONTHDAY;
typeof({{Y, M, D}, {0, 0, 0}}) when is_integer(Y), Y > 0, is_integer(M), M > 0, is_integer(D), D > 0 -> ?XSD_DATE;
typeof({{0, 0, 0}, {T, M, S}}) when is_integer(T), is_integer(M), is_integer(S) -> ?XSD_TIME;
typeof({A, B}) when is_float(A), is_float(B) -> ?GEORSS_POINT;
typeof(#{<<"type">> := _, <<"coordinates">> := _}) -> ?GEORSS_JSON;
typeof(#{}) -> ?RDF_MAP;
typeof(X) when is_list(X) -> ?RDF_LIST;
typeof(X) when is_tuple(X) -> ?RDF_SEQ.


%%
%% Translate semantic type to Erlang native
native({{iri, _}, {iri, _}, {iri, _}}) -> native_type(?XSD_ANYURI);
native({{iri, _}, {iri, _}, {Type, O}}) -> native_type(Type);
native(#{s := _, p := _, o := _, type := Type}) -> native_type(Type).

native_type(?XSD_ANYURI) -> iri;

native_type(?XSD_STRING) -> binary;

native_type(?XSD_INTEGER) -> integer;
native_type(?XSD_BYTE) -> integer;
native_type(?XSD_SHORT) -> integer;
native_type(?XSD_INT) -> integer;
native_type(?XSD_LONG) -> integer;

native_type(?XSD_DECIMAL) -> float;
native_type(?XSD_FLOAT) -> float;
native_type(?XSD_DOUBLE) -> float;

native_type(?XSD_BOOLEAN) -> boolean;

native_type(?XSD_DATETIME) -> timestamp;
native_type(?XSD_DATE) -> datetime;
native_type(?XSD_TIME) -> datetime;
native_type(?XSD_YEARMONTH) -> datetime;
native_type(?XSD_YEAR) -> datetime;
native_type(?XSD_MONTHDAY) -> datetime;
native_type(?XSD_MONTH) -> datetime;
native_type(?XSD_DAY) -> datetime;

native_type(?GEORSS_POINT) -> geopoint;
native_type(?GEORSS_HASH) -> geohash;
native_type(?GEORSS_JSON) -> geojson;

native_type(?RDF_MAP) -> map;
native_type(?RDF_LIST) -> list;
native_type(?RDF_SEQ) -> tuple;

native_type({iri, ?LANG, Lang}) -> Lang.


%%
%% compile abstract syntax knowledge statement to type-safe once
compile({{iri, _} = S, {iri, _} = P, {iri, _} = O}) ->
   #{
      s => get_or_else(semantic:compact(S), S),
      p => get_or_else(semantic:compact(P), P),
      o => get_or_else(semantic:compact(O), O),
      c => 1.0,
      k => uid:encode64( uid:l() ),
      type => ?XSD_ANYURI
   };

compile({{iri, _} = S, {iri, _} = P, {{iri, ?LANG, _} = Type, O}}) ->
   #{
      s => get_or_else(semantic:compact(S), S),
      p => get_or_else(semantic:compact(P), P),
      o => O,
      c => 1.0,
      k => uid:encode64( uid:l() ),
      type => Type
   };

compile({{iri, _} = S, {iri, _} = P, {{iri, _} = Type, O}}) ->
   decode(semantic:compact(Type), O,
      #{
         s => get_or_else(semantic:compact(S), S),
         p => get_or_else(semantic:compact(P), P),
         c => 1.0,
         k => uid:encode64( uid:l() )
      }
   );

compile({{iri, _} = S, {iri, _} = P, {{iri, _, _} = Type, O}}) ->
   decode(Type, O,
      #{
         s => get_or_else(semantic:compact(S), S),
         p => get_or_else(semantic:compact(P), P),
         c => 1.0,
         k => uid:encode64( uid:l() )
      }
   );

compile({{iri, _} = S, {iri, _} = P, O}) ->
   decode(O,
      #{
         s => get_or_else(semantic:compact(S), S),
         p => get_or_else(semantic:compact(P), P),
         c => 1.0,
         k => uid:encode64( uid:l() )
      }
   ).


%%
%% type-safe decode
decode(?XSD_STRING = Type, O, Fact) ->
   Fact#{o => O, type => Type};

%%
decode(?XSD_INTEGER = Type, O, Fact) -> 
   Fact#{o => scalar:i(O), type => Type};

%%
decode(?XSD_BYTE = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_SHORT = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_INT = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_LONG = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};


%%
decode(?XSD_DECIMAL = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

decode(?XSD_FLOAT = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

decode(?XSD_DOUBLE = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

%%
decode(?XSD_BOOLEAN = Type, <<"true">>, Fact) ->
   Fact#{o => true, type => Type};

decode(?XSD_BOOLEAN = Type, <<"false">>, Fact) ->
   Fact#{o => false, type => Type};

decode(?XSD_BOOLEAN = Type, <<"1">>, Fact) ->
   Fact#{o => true, type => Type};

decode(?XSD_BOOLEAN = Type, <<"0">>, Fact) ->
   Fact#{o => false, type => Type};

%%
decode(?XSD_DATETIME = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_DATE = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_TIME = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_YEARMONTH = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_YEAR = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_MONTHDAY = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?XSD_MONTH = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_DAY = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

%%
decode(?GEORSS_HASH = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?GEORSS_POINT = Type, O, Fact) ->
   [Lat, Lng] = binary:split(O, [<<$ >>, <<$,>>]), 
   Fact#{o => {scalar:f(Lat), scalar:f(Lng)}, type => Type};

decode(?GEORSS_JSON = Type, #{<<"type">> := _, <<"coordinates">> := _} = GeoJson, Fact) ->
   Fact#{o => GeoJson, type => Type}.


%%
%% relax decode
decode(<<"true">>, Fact) ->
   Fact#{o => true,  type => ?XSD_BOOLEAN};

decode(<<"false">>, Fact) ->
   Fact#{o => false, type => ?XSD_BOOLEAN};

decode(LatLng, #{p := {iri, <<"georss">>, <<"point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => {scalar:f(Lat), scalar:f(Lng)}, type => ?GEORSS_POINT}; 

decode(LatLng, #{p := {iri, <<"http://www.georss.org/georss/point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => {scalar:f(Lat), scalar:f(Lng)}, type => ?GEORSS_POINT};

decode(O, Fact) ->
   case scalar:decode(O) of
      X when is_integer(X) ->
         Fact#{o => X, type => ?XSD_INTEGER};
      X when is_float(X) ->
         Fact#{o => X, type => ?XSD_DOUBLE};
      X when is_boolean(X) ->
         Fact#{o => X, type => ?XSD_BOOLEAN};
      X when is_binary(X) ->
         Fact#{o => X, type => ?XSD_STRING}
   end.


get_or_else(undefined, X) ->
   X;
get_or_else(X, _) ->
   X.
