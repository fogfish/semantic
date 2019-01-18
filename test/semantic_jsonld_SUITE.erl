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
-module(semantic_jsonld_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("semantic/include/semantic.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
   json_string/1,
   json_integer/1,
   json_double/1,
   json_true/1,
   json_false/1,

   json_id/1,
   jsonld_id/1,
   rdf_id/1,
   jsonld_anyuri/1,
   jsonld_string/1,
   jsonld_strings/1,
   jsonld_subst_compact_predicate/1,
   jsonld_subst_absolute_predicate/1,
   jsonld_subst_typed_predicate/1
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


json_string(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"value">>}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"value">>
   }).

json_integer(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_INTEGER, 1}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>,
      <<"key">> => 1
   }).

json_double(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_DECIMAL, 1.0}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>,
      <<"key">> => 1.0
   }).

json_true(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_BOOLEAN, true}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>,
      <<"key">> => true
   }).

json_false(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_BOOLEAN, false}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>,
      <<"key">> => false
   }).


json_id(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"value">>}
   }] = semantic:jsonld(#{
      <<"id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"value">>
   }).

jsonld_id(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"value">>}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"value">>
   }).

rdf_id(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"value">>}
   }] = semantic:jsonld(#{
      <<"rdf:id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"value">>
   }).

jsonld_anyuri(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_ANYURI, <<"http://dbpedia.org/resource/b">>}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"http://dbpedia.org/resource/b">>,
      <<"@context">> => #{
         <<"key">> => #{<<"@type">> => <<"@id">>}
      }
   }).

jsonld_string(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"test">>}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"test">>,
      <<"@context">> => #{
         <<"key">> => #{<<"@type">> => <<"xsd:string">>}
      }
   }).

jsonld_strings(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"a">>}
   },
   {
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"b">>}
   },
   {
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"key">>},
      {?XSD_STRING, <<"c">>}
   }
   ] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => [<<"a">>, <<"b">>, <<"c">>],
      <<"@context">> => #{
         <<"key">> => #{<<"@type">> => <<"xsd:string">>}
      }
   }).

jsonld_subst_compact_predicate(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"foaf:name">>},
      {?XSD_STRING, <<"test">>}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"test">>,
      <<"@context">> => #{
         <<"key">> => <<"foaf:name">>
      }
   }).

jsonld_subst_absolute_predicate(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"name">>},
      {?XSD_STRING, <<"test">>}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"test">>,
      <<"@context">> => #{
         <<"key">> => <<"name">>
      }
   }).

jsonld_subst_typed_predicate(_) ->
   [{
      {iri, <<"http://dbpedia.org/resource/a">>},
      {iri, <<"foaf:name">>},
      {?XSD_STRING, <<"test">>}
   }] = semantic:jsonld(#{
      <<"@id">> => <<"http://dbpedia.org/resource/a">>, 
      <<"key">> => <<"test">>,
      <<"@context">> => #{
         <<"key">> => #{<<"@type">> => <<"xsd:string">>, <<"@id">> => <<"foaf:name">>}
      }
   }).
