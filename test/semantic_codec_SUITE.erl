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
-module(semantic_codec_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   to_json_xsd_anyuri_absolute/1
,  to_json_xsd_anyuri_compact/1
,  to_json_xsd_string/1
,  to_json_xsd_integer/1
,  to_json_xsd_decimal/1
,  to_json_xsd_boolean/1
,  to_json_xsd_datetime/1
,  to_json_xsd_gyear/1
,  to_json_xsd_gmonth/1
,  to_json_xsd_gday/1
,  to_json_xsd_yearmonth/1
,  to_json_xsd_monthday/1
,  to_json_xsd_time/1
,  to_json_xsd_date/1
,  to_json_georss_point/1
,  to_json_georss_json/1
,  to_json_rdf_map/1
,  to_json_rdf_list/1
,  to_json_rdf_tuple/1

,  to_text_xsd_anyuri_absolute/1
,  to_text_xsd_anyuri_compact/1
,  to_text_xsd_string/1
,  to_text_xsd_integer/1
,  to_text_xsd_decimal/1
,  to_text_xsd_boolean/1
,  to_text_xsd_datetime/1
,  to_text_xsd_gyear/1
,  to_text_xsd_gmonth/1
,  to_text_xsd_gday/1
,  to_text_xsd_yearmonth/1
,  to_text_xsd_monthday/1
,  to_text_xsd_time/1
,  to_text_xsd_date/1
,  to_text_georss_point/1
,  to_text_georss_json/1
,  to_text_rdf_map/1
,  to_text_rdf_list/1
,  to_text_rdf_tuple/1
]).

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%
%%
to_json_xsd_anyuri_absolute(_) ->
   <<"http://example.com/a">> = semantic:to_json({iri, <<"http://example.com/a">>}).

to_json_xsd_anyuri_compact(_) ->
   <<"example:a">> = semantic:to_json({iri, <<"example">>, <<"a">>}).

to_json_xsd_string(_) ->
   <<"example">> = semantic:to_json(<<"example">>).

to_json_xsd_integer(_) ->
   123 = semantic:to_json(123).

to_json_xsd_decimal(_) ->
   12.3 = semantic:to_json(12.3).

to_json_xsd_boolean(_) ->
   true = semantic:to_json(true),
   false = semantic:to_json(false).

to_json_xsd_datetime(_) ->
   <<"1970-01-01T00:00:00Z">> = semantic:to_json({0, 0, 0}).

to_json_xsd_gyear(_) ->
   <<"2010">> = semantic:to_json({{2010, 0, 0}, {0, 0, 0}}).

to_json_xsd_gmonth(_) ->
   <<"08">> = semantic:to_json({{0, 8, 0}, {0, 0, 0}}).

to_json_xsd_gday(_) ->
   <<"04">> = semantic:to_json({{0, 0, 4}, {0, 0, 0}}).

to_json_xsd_yearmonth(_) ->
   <<"2010-08">> = semantic:to_json({{2010, 8, 0}, {0, 0, 0}}).

to_json_xsd_monthday(_) ->
   <<"--08-08">> = semantic:to_json({{0, 8, 8}, {0, 0, 0}}).

to_json_xsd_time(_) ->
   <<"10:01:30Z">> = semantic:to_json({{0, 0, 0}, {10, 1, 30}}).

to_json_xsd_date(_) ->
   <<"1970-01-01">> = semantic:to_json({{1970, 1, 1}, {0, 0, 0}}).

to_json_georss_point(_) ->
   <<"60.100000000,20.300000000">> = semantic:to_json({60.1,20.3}).

to_json_georss_json(_) ->
   #{
      <<"type">> := <<"Polygon">>, 
      <<"coordinates">> := [[ [20.3,60.1] ]]
   } = semantic:to_json(
      #{
         <<"type">> => <<"Polygon">>, 
         <<"coordinates">> => [[ [20.3,60.1] ]]
      }
   ).

to_json_rdf_map(_) ->
   #{
      <<"example">> := <<"text">>,
      <<"a">> := 1 
   } = semantic:to_json(
      #{
         <<"example">> => <<"text">>,
         <<"a">> => 1
      }
   ).

to_json_rdf_list(_) ->
   [1, 2, 3, 4] = semantic:to_json([1, 2, 3, 4]).

to_json_rdf_tuple(_) ->
   [1, 2, 3, 4] = semantic:to_json({1, 2, 3, 4}).

%%
%%
to_text_xsd_anyuri_absolute(_) ->
   <<"<http://example.com/a>">> = semantic:to_text({iri, <<"http://example.com/a">>}).

to_text_xsd_anyuri_compact(_) ->
   <<"example:a">> = semantic:to_text({iri, <<"example">>, <<"a">>}).

to_text_xsd_string(_) ->
   <<"\"example\"">> = semantic:to_text(<<"example">>).

to_text_xsd_integer(_) ->
   <<"123">> = semantic:to_text(123).

to_text_xsd_decimal(_) ->
   <<"12.300000000">> = semantic:to_text(12.3).

to_text_xsd_boolean(_) ->
   <<"true">> = semantic:to_text(true),
   <<"false">> = semantic:to_text(false).

to_text_xsd_datetime(_) ->
   <<"1970-01-01T00:00:00Z">> = semantic:to_text({0, 0, 0}).

to_text_xsd_gyear(_) ->
   <<"2010">> = semantic:to_text({{2010, 0, 0}, {0, 0, 0}}).

to_text_xsd_gmonth(_) ->
   <<"08">> = semantic:to_text({{0, 8, 0}, {0, 0, 0}}).

to_text_xsd_gday(_) ->
   <<"04">> = semantic:to_text({{0, 0, 4}, {0, 0, 0}}).

to_text_xsd_yearmonth(_) ->
   <<"2010-08">> = semantic:to_text({{2010, 8, 0}, {0, 0, 0}}).

to_text_xsd_monthday(_) ->
   <<"--08-08">> = semantic:to_text({{0, 8, 8}, {0, 0, 0}}).

to_text_xsd_time(_) ->
   <<"10:01:30Z">> = semantic:to_text({{0, 0, 0}, {10, 1, 30}}).

to_text_xsd_date(_) ->
   <<"1970-01-01">> = semantic:to_text({{1970, 1, 1}, {0, 0, 0}}).

to_text_georss_point(_) ->
   <<"60.100000000,20.300000000">> = semantic:to_text({60.1,20.3}).

to_text_georss_json(_) ->
   <<"{coordinates:[[[20.300000000,60.100000000]]],type:\"Polygon\"}">> = semantic:to_text(
      #{
         <<"type">> => <<"Polygon">>, 
         <<"coordinates">> => [[ [20.3,60.1] ]]
      }
   ).

to_text_rdf_map(_) ->
   <<"{a:1,example:\"text\"}">> = semantic:to_text(
      #{
         <<"example">> => <<"text">>,
         <<"a">> => 1
      }
   ).

to_text_rdf_list(_) ->
   <<"[1,2,3,4]">> = semantic:to_text([1, 2, 3, 4]).

to_text_rdf_tuple(_) ->
   <<"(1,2,3,4)">> = semantic:to_text({1, 2, 3, 4}).

