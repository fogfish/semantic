-module(semantic_codec).
-include("semantic.hrl").

-export([
   decode/2
,  decode_xsd_anyuri/1
,  decode_xsd_string/1
,  decode_xsd_integer/1
,  decode_xsd_decimal/1
,  decode_xsd_boolean/1
,  decode_xsd_datetime/1
,  decode_xsd_date/1
,  decode_xsd_time/1
,  decode_xsd_yearmonth/1
,  decode_xsd_monthday/1
,  decode_xsd_year/1
,  decode_xsd_month/1
,  decode_xsd_day/1
,  decode_georss_point/1
,  decode_georss_hash/1
,  decode_georss_json/1

,  encode_json/1
,  encode_text/1
]).

-define(UNX_EPOCH, 62167219200).
-define(BASE,          1000000).

%%
%%
decode(Type, List)
 when is_list(List) ->
   [decode(Type, X) || X <- List];

decode(?XSD_ANYURI, X) -> decode_xsd_anyuri(X);
decode(?XSD_STRING, X) -> decode_xsd_string(X);
decode(?XSD_INTEGER, X) -> decode_xsd_integer(X); 
decode(?XSD_BYTE, X) -> decode_xsd_integer(X); 
decode(?XSD_SHORT, X) -> decode_xsd_integer(X); 
decode(?XSD_INT, X) -> decode_xsd_integer(X);
decode(?XSD_LONG, X) -> decode_xsd_integer(X);
decode(?XSD_DECIMAL, X) -> decode_xsd_decimal(X);
decode(?XSD_FLOAT, X) -> decode_xsd_decimal(X);
decode(?XSD_DOUBLE, X) -> decode_xsd_decimal(X);
decode(?XSD_BOOLEAN, X) -> decode_xsd_boolean(X);
decode(?XSD_DATETIME, X) -> decode_xsd_datetime(X);
decode(?XSD_DATE, X) -> decode_xsd_date(X);
decode(?XSD_TIME, X) -> decode_xsd_time(X);
decode(?XSD_YEARMONTH, X) -> decode_xsd_yearmonth(X);
decode(?XSD_MONTHDAY, X) -> decode_xsd_monthday(X);
decode(?XSD_YEAR, X) -> decode_xsd_year(X);
decode(?XSD_MONTH, X) -> decode_xsd_month(X);
decode(?XSD_DAY, X) -> decode_xsd_day(X);

decode(?GEORSS_POINT, X) -> decode_georss_point(X);
decode(?GEORSS_HASH, X) -> decode_georss_hash(X);
decode(?GEORSS_JSON, X) -> decode_georss_json(X).


%%
%%
decode_xsd_anyuri(Iri) ->
   case semantic:compact(Iri) of
      undefined ->
         semantic:absolute(Iri);
      Compact ->
         Compact
   end.

decode_xsd_string(Val) ->
   typecast:s(Val).

decode_xsd_integer(Val) ->
   typecast:i(Val).

decode_xsd_decimal(Val) ->
   typecast:f(Val).

%%
decode_xsd_boolean(<<"true">>) -> true;
decode_xsd_boolean(<<"1">>) -> true;
decode_xsd_boolean(<<"false">>) -> false;
decode_xsd_boolean(<<"0">>) -> false;
decode_xsd_boolean(true) -> true;
decode_xsd_boolean(false) -> false.

%%
decode_xsd_datetime(<<
   Y:4/binary, $-, M:2/binary, $-, D:2/binary, $T, 
   T:2/binary, $:, N:2/binary, $:, S:2/binary, $Z
>>) ->
   decode_xsd_datetime(Y, M, D, T, N, S);
decode_xsd_datetime(<<
   Y:4/binary, M:2/binary, D:2/binary, $T, 
   T:2/binary, N:2/binary, S:2/binary, $Z
>>) ->
   decode_xsd_datetime(Y, M, D, T, N, S).

decode_xsd_datetime(Y, M, D, T, N, S) ->
   Sec = calendar:datetime_to_gregorian_seconds({
      {typecast:i(Y), typecast:i(M), typecast:i(D)},
      {typecast:i(T), typecast:i(N), typecast:i(S)}
   }) - ?UNX_EPOCH,
   {Sec div ?BASE, Sec rem ?BASE, 0}.

%%
decode_xsd_date(<<Y:4/binary, $-, M:2/binary, $-, D:2/binary>>) ->
   {{typecast:i(Y), typecast:i(M), typecast:i(D)}, {0, 0, 0}};

decode_xsd_date(<<Y:4/binary, M:2/binary, D:2/binary>>) ->
   {{typecast:i(Y), typecast:i(M), typecast:i(D)}, {0, 0, 0}}.

%%
decode_xsd_time(<<T:2/binary, $:,  N:2/binary, $:, S:2/binary, $Z>>) ->
   {{0, 0, 0}, {typecast:i(T), typecast:i(N), typecast:i(S)}};

decode_xsd_time(<<T:2/binary, N:2/binary, S:2/binary, $Z>>) ->
   {{0, 0, 0}, {typecast:i(T), typecast:i(N), typecast:i(S)}}.

%%
decode_xsd_yearmonth(<<Y:4/binary, $-, M:2/binary>>) ->
   {{typecast:i(Y), typecast:i(M), 0}, {0, 0, 0}}.

%%
decode_xsd_monthday(<<$-, $-, M:2/binary, $-, D:2/binary>>) ->
   {{0, typecast:i(M), typecast:i(D)}, {0, 0, 0}}.


%%
decode_xsd_year(<<Y:4/binary>>) ->
   {{typecast:i(Y), 0, 0}, {0, 0, 0}}.

%%
decode_xsd_month(<<M:2/binary>>) ->
   {{0, typecast:i(M), 0}, {0, 0, 0}}.

%%
decode_xsd_day(<<D:2/binary>>) ->
   {{0, 0, typecast:i(D)}, {0, 0, 0}}.

%%
decode_georss_point(Val) ->
   case binary:split(Val, [<<$ >>, <<$,>>]) of
      [Lat, Lng] ->
         {typecast:f(Lat), typecast:f(Lng)};
      _ ->
         Val
   end.

%%
decode_georss_hash(Val) ->
   typecast:s(Val).

%% fake codec, validate data input 
decode_georss_json(#{<<"type">> := _, <<"coordinates">> := _} = Json) ->
   Json.


%%
%%
encode_json({iri, IRI}) -> IRI;
encode_json({iri, _, _} = IRI) -> encode_xsd_anyuri(IRI);
encode_json(Lit) when is_binary(Lit) -> Lit;
encode_json(Lit) when is_integer(Lit) -> Lit;
encode_json(Lit) when is_float(Lit) -> Lit;
encode_json(true) -> true;
encode_json(false) -> false;
encode_json({A, B, C}) when is_integer(A), is_integer(B), is_integer(C) -> encode_xsd_datetime(A, B, C);
encode_json({{Y, 0, 0}, {0, 0, 0}}) when is_integer(Y), Y > 0 -> encode_xsd_year(Y);
encode_json({{0, M, 0}, {0, 0, 0}}) when is_integer(M), M > 0 -> encode_xsd_month(M);
encode_json({{0, 0, D}, {0, 0, 0}}) when is_integer(D), D > 0 -> encode_xsd_day(D);
encode_json({{Y, M, 0}, {0, 0, 0}}) when is_integer(Y), Y > 0, is_integer(M), M > 0 -> encode_xsd_yearmonth(Y, M);
encode_json({{0, M, D}, {0, 0, 0}}) when is_integer(M), M > 0, is_integer(D), D > 0 -> encode_xsd_monthday(M, D);
encode_json({{Y, M, D}, {0, 0, 0}}) when is_integer(Y), Y > 0, is_integer(M), M > 0, is_integer(D), D > 0 -> encode_xsd_date(Y, M, D);
encode_json({{0, 0, 0}, {T, N, S}}) when is_integer(T), is_integer(N), is_integer(S) -> encode_xsd_time(T, N, S);
encode_json({A, B}) when is_float(A), is_float(B) -> encode_georss_point(A, B);
encode_json(#{<<"type">> := _, <<"coordinates">> := _} = GeoJson) ->
   GeoJson;
encode_json(#{} = Lit) ->
   maps:map(fun(_, X) -> encode_json(X) end, Lit);
encode_json(Lit)
 when is_list(Lit) ->
   [encode_json(X) || X <- Lit];
encode_json(Lit) when is_tuple(Lit) -> 
   [encode_json(X) || X <- tuple_to_list(Lit)].


%%
%% maps Erlang native term to textual-format
encode_text({iri, IRI}) -> <<$<, IRI/binary, $>>>;
encode_text({iri, _, _} = IRI) -> encode_xsd_anyuri(IRI); 
encode_text(Lit) when is_binary(Lit) -> <<$", Lit/binary, $">>;
encode_text(Lit) when is_integer(Lit) -> typecast:s(Lit);
encode_text(Lit) when is_float(Lit) -> typecast:s(Lit);
encode_text(Lit) when is_atom(Lit) -> typecast:s(Lit);
encode_text({A, B, C}) when is_integer(A), is_integer(B), is_integer(C) -> encode_xsd_datetime(A, B, C);
encode_text({{Y, 0, 0}, {0, 0, 0}}) when is_integer(Y), Y > 0 -> encode_xsd_year(Y);
encode_text({{0, M, 0}, {0, 0, 0}}) when is_integer(M), M > 0 -> encode_xsd_month(M);
encode_text({{0, 0, D}, {0, 0, 0}}) when is_integer(D), D > 0 -> encode_xsd_day(D);
encode_text({{Y, M, 0}, {0, 0, 0}}) when is_integer(Y), Y > 0, is_integer(M), M > 0 -> encode_xsd_yearmonth(Y, M);
encode_text({{0, M, D}, {0, 0, 0}}) when is_integer(M), M > 0, is_integer(D), D > 0 -> encode_xsd_monthday(M, D);
encode_text({{Y, M, D}, {0, 0, 0}}) when is_integer(Y), Y > 0, is_integer(M), M > 0, is_integer(D), D > 0 -> encode_xsd_date(Y, M, D);
encode_text({{0, 0, 0}, {T, N, S}}) when is_integer(T), is_integer(N), is_integer(S) -> encode_xsd_time(T, N, S);
encode_text({A, B}) when is_float(A), is_float(B) -> encode_georss_point(A, B);
% encode_text(#{<<"type">> := Type, <<"coordinates">> := Coordinates}) ->
encode_text(#{} = Lit) ->
   typecast:s([${,
      lists:join($,,
         [ [typecast:s(Key), $:, encode_text(Val)] || {Key, Val} <- maps:to_list(Lit) ]
      ),
   $}]);
encode_text(Lit)
 when is_list(Lit) ->
   typecast:s([$[,
      lists:join($,,
         [ encode_text(Val) || Val <- Lit ]
      ),
   $]]);
encode_text(Lit) when is_tuple(Lit) ->
   typecast:s([$(,
      lists:join($,,
         [ encode_text(Val) || Val <- tuple_to_list(Lit) ]
      ),
   $)]).


%%
encode_xsd_anyuri({iri, Prefix, Suffix}) -> 
   <<Prefix/binary, $:, Suffix/binary>>.

%%
encode_xsd_datetime(A, B, C) ->
   Seconds = A * ?BASE + B,
   {{Y, M, D}, {T, N, S}} = calendar:gregorian_seconds_to_datetime(Seconds + ?UNX_EPOCH),
   <<(encode_xsd_date(Y, M, D))/binary, $T, (encode_xsd_time(T, N, S))/binary>>.

encode_xsd_date(Y, M, D) ->
   <<(typecast:s(Y))/binary, $-, (encode_xsd_0digit(M))/binary, $-, (encode_xsd_0digit(D))/binary>>.

encode_xsd_time(T, N, S) ->
   <<(encode_xsd_0digit(T))/binary, $:, (encode_xsd_0digit(N))/binary, $:, (encode_xsd_0digit(S))/binary, $Z>>.

%%
encode_xsd_yearmonth(Y, M) ->
   <<(typecast:s(Y))/binary, $-, (encode_xsd_0digit(M))/binary>>.

%%
encode_xsd_monthday(M, D) ->
   <<$-, $-, (encode_xsd_0digit(M))/binary, $-, (encode_xsd_0digit(D))/binary>>.

%%
encode_xsd_year(Y) -> typecast:s(Y).
encode_xsd_month(M) -> encode_xsd_0digit(M).
encode_xsd_day(D) -> encode_xsd_0digit(D).

encode_xsd_0digit(X) when X > 9 -> typecast:s(X);
encode_xsd_0digit(X) -> <<$0, (typecast:s(X))/binary>>.

%%
encode_georss_point(Lat, Lng) -> 
   <<(typecast:s(Lat))/binary, $,, (typecast:s(Lng))/binary>>.
