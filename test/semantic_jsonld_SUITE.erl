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

%%
%% common test
-export([
   all/0,
   groups/0,
   init_per_suite/1,
   end_per_suite/1,
   init_per_group/2,
   end_per_group/2
]).

%%
%% unit tests
-export([
   json_string/1,
   json_integer/1,
   json_double/1,
   json_true/1,
   json_false/1,

   jsonld_anyuri/1,
   jsonld_string/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, compile}
   ].

groups() ->
   [
      {compile, [parallel], [
         json_string,
         json_integer,
         json_double,
         json_true,
         json_false,

         jsonld_anyuri,
         jsonld_string
      ]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   ok = semantic:start(),
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   

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
      {?XSD_DOUBLE, 1.0}
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
