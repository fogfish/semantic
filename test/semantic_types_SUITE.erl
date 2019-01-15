-module(semantic_types_SUITE).
-include_lib("semantic/include/semantic.hrl").

-export([all/0]).
-export([
   xsd_anyuri_absolute/1
,  xsd_anyuri_compact/1
,  xsd_string/1
,  xsd_integer/1
,  xsd_decimal/1
,  xsd_boolean/1
,  xsd_datetime/1
,  xsd_date/1
,  xsd_time/1
,  xsd_yearmonth/1
,  xsd_monthday/1
,  xsd_year/1
,  xsd_month/1
,  xsd_day/1
,  georss_point/1
,  georss_hash/1
,  georss_json/1
,  rdf_map/1
,  rdf_list/1
,  rdf_seq/1
]).

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

-define(fact(X), {{iri, <<"a">>}, {iri, <<"b">>}, X}).

xsd_anyuri_absolute(_) ->
   ?XSD_ANYURI = semantic:typeof({iri, <<"http://example.com">>}),
   iri = semantic:native( ?fact({iri, <<"c">>}) ).

xsd_anyuri_compact(_) ->
   ?XSD_ANYURI = semantic:typeof({iri, <<"example">>, <<"a">>}),
   iri = semantic:native( ?fact({iri, <<"c">>}) ).

xsd_string(_) ->
   ?XSD_STRING = semantic:typeof(<<"string">>),
   binary = semantic:native( ?fact({?XSD_STRING, <<"string">>}) ).

xsd_integer(_) ->
   ?XSD_INTEGER = semantic:typeof(1),
   integer = semantic:native( ?fact({?XSD_INTEGER, 1}) ),
   integer = semantic:native( ?fact({?XSD_BYTE, 1}) ),
   integer = semantic:native( ?fact({?XSD_SHORT, 1}) ),
   integer = semantic:native( ?fact({?XSD_INT, 1}) ),
   integer = semantic:native( ?fact({?XSD_LONG, 1}) ).

xsd_decimal(_) ->
   ?XSD_DECIMAL = semantic:typeof(1.0),
   float = semantic:native( ?fact({?XSD_DECIMAL, 1.0}) ),
   float = semantic:native( ?fact({?XSD_FLOAT, 1.0}) ),
   float = semantic:native( ?fact({?XSD_DOUBLE, 1.0}) ).

xsd_boolean(_) ->
   ?XSD_BOOLEAN = semantic:typeof(true),
   ?XSD_BOOLEAN = semantic:typeof(false),
   boolean = semantic:native( ?fact({?XSD_BOOLEAN, true}) ).

xsd_datetime(_) ->
   T = os:timestamp(),
   ?XSD_DATETIME = semantic:typeof(T),
   timestamp = semantic:native( ?fact({?XSD_DATETIME, T}) ).

xsd_date(_) ->
   T = {{1970, 1, 1}, {0, 0, 0}},
   ?XSD_DATE = semantic:typeof(T),
   datetime = semantic:native( ?fact({?XSD_DATE, T}) ).

xsd_time(_) ->
   T = {{0, 0, 0}, {12, 40, 20}},
   ?XSD_TIME = semantic:typeof(T),
   datetime = semantic:native( ?fact({?XSD_TIME, T}) ).

xsd_yearmonth(_) ->
   T = {{1970, 1, 0}, {0, 0, 0}},
   ?XSD_YEARMONTH = semantic:typeof(T),
   datetime = semantic:native( ?fact({?XSD_YEARMONTH, T}) ).

xsd_monthday(_) ->
   T = {{0, 1, 12}, {0, 0, 0}},
   ?XSD_MONTHDAY = semantic:typeof(T),
   datetime = semantic:native( ?fact({?XSD_MONTHDAY, T}) ).

xsd_year(_) ->
   T = {{1970, 0, 0}, {0, 0, 0}},
   ?XSD_YEAR = semantic:typeof(T),
   datetime = semantic:native( ?fact({?XSD_YEAR, T}) ).

xsd_month(_) ->
   T = {{0, 10, 0}, {0, 0, 0}},
   ?XSD_MONTH = semantic:typeof(T),
   datetime = semantic:native( ?fact({?XSD_MONTH, T}) ).

xsd_day(_) ->
   T = {{0, 0, 10}, {0, 0, 0}},
   ?XSD_DAY = semantic:typeof(T),
   datetime = semantic:native( ?fact({?XSD_DAY, T}) ).

georss_point(_) ->
   ?GEORSS_POINT = semantic:typeof({1.0, 1.0}),
   geopoint = semantic:native( ?fact({?GEORSS_POINT, {0.0, 0.0}}) ).

georss_hash(_) ->
   geohash = semantic:native( ?fact({?GEORSS_HASH, <<"aaa">>}) ).

georss_json(_) ->
   ?GEORSS_JSON = semantic:typeof(#{<<"type">> => <<"Point">>, <<"coordinates">> => [25.7, 62.1]}),
   geojson = semantic:native( ?fact({?GEORSS_JSON, #{<<"type">> => <<"Point">>, <<"coordinates">> => [25.7, 62.1]}}) ).

rdf_map(_) ->
   ?RDF_MAP = semantic:typeof(#{a => 1}),
   map = semantic:native( ?fact({?RDF_MAP, #{a => 1}}) ).

rdf_list(_) ->
   ?RDF_LIST = semantic:typeof([1]),
   list = semantic:native( ?fact({?RDF_LIST, [1]}) ).

rdf_seq(_) ->
   ?RDF_SEQ = semantic:typeof({1}),
   tuple = semantic:native( ?fact({?RDF_SEQ, {1}}) ).
