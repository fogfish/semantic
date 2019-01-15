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
,  to_json/1
,  to_text/1
,  compile/1
]).

-define(UNX_EPOCH, 62167219200).
-define(BASE,          1000000).

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
native({{iri, _}, {iri, _}, {Type, _}}) -> native_type(Type);
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
%% maps Erlang native term to json-format
to_json({iri, IRI}) -> 
   IRI;
to_json({iri, Prefix, Suffix}) -> 
   <<Prefix/binary, $:, Suffix/binary>>;
to_json(Lit)
 when is_binary(Lit) -> 
   Lit;
to_json(Lit)
 when is_integer(Lit) ->
   Lit;
to_json(Lit)
 when is_float(Lit) ->
   Lit;
to_json(true) ->
   true;
to_json(false) ->
   false;
to_json({A, B, C} = T)
 when is_integer(A), is_integer(B), is_integer(C) ->
   datetime_to_binary(T);
to_json({{Y, M, D}, {T, N, S}} = A)
 when is_integer(Y), is_integer(M), is_integer(D), is_integer(T), is_integer(N), is_integer(S) ->
   datetime_to_binary(A);
to_json({A, B})
 when is_float(A), is_float(B) ->
   <<(typecast:s(A))/binary, $,, (typecast:s(B))/binary>>;
to_json(#{<<"type">> := _, <<"coordinates">> := _} = GeoJson) ->
   GeoJson;
to_json(#{} = Lit) ->
   maps:map(fun(_, X) -> to_json(X) end, Lit);
to_json(Lit)
 when is_list(Lit) ->
   [to_json(X) || X <- Lit];
to_json(Lit) when is_tuple(Lit) -> 
   [to_json(X) || X <- tuple_to_list(Lit)].


%%
%% maps Erlang native term to textual-format
to_text({iri, IRI}) -> 
   <<$<, IRI/binary, $>>>;
to_text({iri, Prefix, Suffix}) -> 
   <<Prefix/binary, $:, Suffix/binary>>;
to_text(Lit)
 when is_binary(Lit) -> 
   <<$", Lit/binary, $">>;
to_text(Lit)
 when is_integer(Lit) ->
   typecast:s(Lit);
to_text(Lit)
 when is_float(Lit) ->
   typecast:s(Lit);
to_text(true) ->
   <<"true">>;
to_text(false) ->
   <<"false">>;
to_text({A, B, C} = T)
 when is_integer(A), is_integer(B), is_integer(C) ->
   datetime_to_binary(T);
to_text({{Y, M, D}, {T, N, S}} = A)
 when is_integer(Y), is_integer(M), is_integer(D), is_integer(T), is_integer(N), is_integer(S) ->
   datetime_to_binary(A);
to_text({A, B})
 when is_float(A), is_float(B) ->
   <<(typecast:s(A))/binary, $,, (typecast:s(B))/binary>>;
to_text(#{<<"type">> := Type, <<"coordinates">> := Coordinates}) ->
   typecast:s([${,
      <<"type:">>, to_text(Type), $,,
      <<"coordinates:">>, to_text(Coordinates),
   $}]);
to_text(#{} = Lit) ->
   typecast:s([${,
      lists:join($,,
         [ [typecast:s(Key), $:, to_text(Val)] || {Key, Val} <- maps:to_list(Lit) ]
      ),
   $}]);
to_text(Lit)
 when is_list(Lit) ->
   typecast:s([$[,
      lists:join($,,
         [ to_text(Val) || Val <- Lit ]
      ),
   $]]);
to_text(Lit) when is_tuple(Lit) ->
   typecast:s([$(,
      lists:join($,,
         [ to_text(Val) || Val <- tuple_to_list(Lit) ]
      ),
   $)]).


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
   Fact#{o => typecast:s(O), type => Type};

%%
decode(?XSD_INTEGER = Type, O, Fact) -> 
   Fact#{o => typecast:i(O), type => Type};

%%
decode(?XSD_BYTE = Type, O, Fact) ->
   Fact#{o => typecast:i(O), type => Type};

decode(?XSD_SHORT = Type, O, Fact) ->
   Fact#{o => typecast:i(O), type => Type};

decode(?XSD_INT = Type, O, Fact) ->
   Fact#{o => typecast:i(O), type => Type};

decode(?XSD_LONG = Type, O, Fact) ->
   Fact#{o => typecast:i(O), type => Type};


%%
decode(?XSD_DECIMAL = Type, O, Fact) ->
   Fact#{o => typecast:f(O), type => Type};

decode(?XSD_FLOAT = Type, O, Fact) ->
   Fact#{o => typecast:f(O), type => Type};

decode(?XSD_DOUBLE = Type, O, Fact) ->
   Fact#{o => typecast:f(O), type => Type};

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
   Fact#{o => binary_to_datetime(typecast:s(O)), type => Type};

decode(?XSD_DATE = Type, O, Fact) ->
   Fact#{o => binary_to_date(typecast:s(O)), type => Type};

decode(?XSD_TIME = Type, O, Fact) ->
   Fact#{o => binary_to_time(typecast:s(O)), type => Type};

decode(?XSD_YEARMONTH = Type, O, Fact) ->
   Fact#{o => binary_to_yearmonth(typecast:s(O)), type => Type};

decode(?XSD_YEAR = Type, O, Fact) ->
   Fact#{o => binary_to_year(typecast:s(O)), type => Type};

decode(?XSD_MONTHDAY = Type, O, Fact) ->
   Fact#{o => binary_to_monthday(typecast:s(O)), type => Type};

decode(?XSD_MONTH = Type, O, Fact) ->
   Fact#{o => binary_to_month(typecast:s(O)), type => Type};

decode(?XSD_DAY = Type, O, Fact) ->
   Fact#{o => binary_to_day(typecast:s(O)), type => Type};

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
   [Lat, Lng] = binary:split(LatLng, [<<$ >>, <<$,>>]), 
   Fact#{o => {scalar:f(Lat), scalar:f(Lng)}, type => ?GEORSS_POINT}; 

decode(LatLng, #{p := {iri, <<"http://www.georss.org/georss/point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, [<<$ >>, <<$,>>]), 
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

%%
%% encode datetime to string
datetime_to_binary({A, B, C})
 when is_integer(A), is_integer(B), is_integer(C) ->
   Seconds = A * ?BASE + B,
   {{Y, M, D}, {T, N, S}} = calendar:gregorian_seconds_to_datetime(Seconds + ?UNX_EPOCH),
   <<
      (typecast:s(Y))/binary, $-, (prefixz(M))/binary, $-, (prefixz(D))/binary, $T, 
      (prefixz(T))/binary, $:, (prefixz(N))/binary, $:, (prefixz(S))/binary, $Z
   >>;

datetime_to_binary({{Y, 0, 0}, {0, 0, 0}})
 when is_integer(Y), Y > 0 ->
   typecast:s(Y);

datetime_to_binary({{0, M, 0}, {0, 0, 0}})
 when is_integer(M), M > 0 ->
   prefixz(M);

datetime_to_binary({{0, 0, D}, {0, 0, 0}})
 when is_integer(D), D > 0 ->
   prefixz(D);

datetime_to_binary({{Y, M, 0}, {0, 0, 0}})
 when is_integer(Y), Y > 0, is_integer(M), M > 0 ->
   <<(typecast:s(Y))/binary, $-, (prefixz(M))/binary>>;

datetime_to_binary({{0, M, D}, {0, 0, 0}})
 when is_integer(M), M > 0, is_integer(D), D > 0 ->
   <<$-, $-, (prefixz(M))/binary, $-, (prefixz(D))/binary>>;

datetime_to_binary({{Y, M, D}, {0, 0, 0}})
 when is_integer(Y), Y > 0, is_integer(M), M > 0, is_integer(D), D > 0 ->
 <<(typecast:s(Y))/binary, $-, (prefixz(M))/binary, $-, (prefixz(D))/binary>>;

datetime_to_binary({{0, 0, 0}, {T, M, S}})
 when is_integer(T), is_integer(M), is_integer(S) ->
   <<(prefixz(T))/binary, $:, (prefixz(M))/binary, $:, (prefixz(S))/binary, $Z>>;

datetime_to_binary({{Y, M, D}, {T, N, S}})
 when is_integer(Y), is_integer(M), is_integer(D), is_integer(T), is_integer(N), is_integer(S) ->
   <<
      (typecast:s(Y))/binary, $-, (prefixz(M))/binary, $-, (prefixz(D))/binary, $T, 
      (prefixz(T))/binary, $:, (prefixz(N))/binary, $:, (prefixz(S))/binary, $Z
   >>.

prefixz(X)
 when X > 9 -> 
   typecast:s(X);
prefixz(X) ->
   <<$0, (typecast:s(X))/binary>>.

%%
%%
binary_to_datetime(<<$-, Y:4/binary, $-, M:2/binary, $-, D:2/binary, $T, T:2/binary, $:, N:2/binary, $:, S:2/binary, $Z>>) ->
   Sec = -1 * calendar:datetime_to_gregorian_seconds({
      {-1 * typecast:i(Y), typecast:i(M), typecast:i(D)},
      {typecast:i(T), typecast:i(N), typecast:i(S)}
   }) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0};

binary_to_datetime(<<$-, Y:4/binary, M:2/binary, D:2/binary, $T, T:2/binary, N:2/binary, S:2/binary, $Z>>) ->
   Sec = -1 * calendar:datetime_to_gregorian_seconds({
      {-1 * typecast:i(Y), typecast:i(M), typecast:i(D)},
      {typecast:i(T), typecast:i(N), typecast:i(S)}
   }) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0};

binary_to_datetime(<<Y:4/binary, $-, M:2/binary, $-, D:2/binary, $T, T:2/binary, $:, N:2/binary, $:, S:2/binary, $Z>>) ->
   Sec = calendar:datetime_to_gregorian_seconds({
      {typecast:i(Y), typecast:i(M), typecast:i(D)},
      {typecast:i(T), typecast:i(N), typecast:i(S)}
   }) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0};

binary_to_datetime(<<Y:4/binary, M:2/binary, D:2/binary, $T, T:2/binary, N:2/binary, S:2/binary, $Z>>) ->
   Sec = calendar:datetime_to_gregorian_seconds({
      {typecast:i(Y), typecast:i(M), typecast:i(D)},
      {typecast:i(T), typecast:i(N), typecast:i(S)}
   }) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0}.

%%
%%
binary_to_date(<<$-, Y:4/binary, $-, M:2/binary, $-, D:2/binary>>) ->
   {{-1 * typecast:i(Y), typecast:i(M), typecast:i(D)}, {0, 0, 0}};

binary_to_date(<<$-, Y:4/binary, M:2/binary, D:2/binary>>) ->
   {{-1 * typecast:i(Y), typecast:i(M), typecast:i(D)}, {0, 0, 0}};

binary_to_date(<<Y:4/binary, $-, M:2/binary, $-, D:2/binary>>) ->
   {{typecast:i(Y), typecast:i(M), typecast:i(D)}, {0, 0, 0}};

binary_to_date(<<Y:4/binary, M:2/binary, D:2/binary>>) ->
   {{typecast:i(Y), typecast:i(M), typecast:i(D)}, {0, 0, 0}}.

%%
%%
binary_to_time(<<T:2/binary, $:,  N:2/binary, $:, S:2/binary, $Z>>) ->
   {{0, 0, 0}, {typecast:i(T), typecast:i(N), typecast:i(S)}};

binary_to_time(<<T:2/binary, N:2/binary, S:2/binary, $Z>>) ->
   {{0, 0, 0}, {typecast:i(T), typecast:i(N), typecast:i(S)}}.

%%
%%
binary_to_yearmonth(<<$-, Y:4/binary, $-, M:2/binary>>) ->
   {{-1 * typecast:i(Y), typecast:i(M), 0}, {0, 0, 0}};

binary_to_yearmonth(<<Y:4/binary, $-, M:2/binary>>) ->
   {{typecast:i(Y), typecast:i(M), 0}, {0, 0, 0}}.

%%
%%
binary_to_year(<<$-, Y:4/binary>>) ->
   {{-1 * typecast:i(Y), 0, 0}, {0, 0, 0}};

binary_to_year(<<Y:4/binary>>) ->
   {{typecast:i(Y), 0, 0}, {0, 0, 0}}.

%%
%%
binary_to_monthday(<<$-, $-, M:4/binary, $-, D:2/binary>>) ->
   {{0, typecast:i(M), typecast:i(D)}, {0, 0, 0}}.

%%
%%
binary_to_month(<<M:2/binary>>) ->
   {{0, typecast:i(M), 0}, {0, 0, 0}}.

%%
%%
binary_to_day(<<D:2/binary>>) ->
   {{0, 0, typecast:i(D)}, {0, 0, 0}}.

