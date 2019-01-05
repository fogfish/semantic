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
-module(semantic_typed_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
   spo_with_uri/1,
   spo_with_string/1,
   spo_with_lang_string/1,
   spo_with_integer/1,
   spo_with_float/1,
   spo_with_bool/1,
   spo_with_datetime/1,
   spo_with_geopoint/1
]).

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

init_per_suite(Config) ->
   {ok, _} = semantic:start(),
   Config.

end_per_suite(_Config) ->
   ok.


spo_with_uri(_Config) ->
   A = <<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> <http://example.org/c> .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"foaf">>, <<"name">>},
      o := {iri, <<"http://example.org/c">>},
      type := {iri, <<"xsd">>, <<"anyURI">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   iri = semantic:native(T).


spo_with_string(_Config) ->
   A = <<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> \"c\"^^<http://www.w3.org/2001/XMLSchema#string> .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"foaf">>, <<"name">>},
      o := <<"c">>,
      type := {iri, <<"xsd">>, <<"string">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   binary = semantic:native(T).

spo_with_lang_string(_Config) ->
   A = <<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> \"text\"@en .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"foaf">>, <<"name">>},
      o := <<"text">>,
      type := {iri, <<"langString">>, <<"en">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   <<"en">> = semantic:native(T).


spo_with_integer(_Config) ->
   A = <<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> \"1\"^^<http://www.w3.org/2001/XMLSchema#integer> .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"foaf">>, <<"name">>},
      o := 1,
      type := {iri, <<"xsd">>, <<"integer">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   integer = semantic:native(T).


spo_with_float(_Config) ->
   A = <<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> \"1.0\"^^<http://www.w3.org/2001/XMLSchema#float> .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"foaf">>, <<"name">>},
      o := 1.0,
      type := {iri, <<"xsd">>, <<"float">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   float = semantic:native(T).

spo_with_bool(_Config) ->
   A = <<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> \"1\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"foaf">>, <<"name">>},
      o := true,
      type := {iri, <<"xsd">>, <<"boolean">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   boolean = semantic:native(T).

spo_with_datetime(_Config) ->
   A = <<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> \"19700101T000000Z\"^^<http://www.w3.org/2001/XMLSchema#dateTime> .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"foaf">>, <<"name">>},
      o := {0, 0, 0},
      type := {iri, <<"xsd">>, <<"dateTime">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   timestamp = semantic:native(T).

spo_with_geopoint(_Config) ->
   A = <<"<http://example.org/a> <http://www.georss.org/georss/point> \"64.0 -150.0\" .\n">>,
   T = #{
      s := {iri, <<"http://example.org/a">>},
      p := {iri, <<"georss">>, <<"point">>},
      o := {64.0, -150.0},
      type := {iri, <<"georss">>, <<"point">>}
   } = maps:with([s,p,o,type], semantic:typed( decode(A) )),
   geopoint = semantic:native(T).



%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

decode(X) ->
   hd(
      erlang:element(1, 
         semantic_nt:decode(X, semantic_nt:new())
      )
   ).
