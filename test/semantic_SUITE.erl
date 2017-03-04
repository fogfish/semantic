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
-module(semantic_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

%%
%% unit tests
-export([
   define_uri/1, define_string/1, define_lang/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, interface}
   ].

groups() ->
   [
      {interface, [parallel], 
         [define_uri, define_string, define_lang]}
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
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   
-define(FOAF_NAME,    {iri, <<"foaf">>, <<"name">>}).
-define(RDF_TYPE,     {iri, <<"rdf">>, <<"type">>}).
-define(RDF_DATATYPE, {iri, <<"rdf">>, <<"datatype">>}).
-define(RDF_RANGE,    {iri, <<"rdf">>, <<"range">>}).
-define(RDF_PROPERTY, {iri, <<"rdf">>, <<"Property">>}).
-define(RDF_LANG_STRING, {iri, <<"rdf">>, <<"langString">>}).
-define(XSD_STRING,   {iri, <<"xsd">>, <<"string">>}).
-define(LANG_EN,      {iri, <<"lang">>, <<"en">>}).


define_uri(_Config) ->
   [
      #{s := ?FOAF_NAME, p := ?RDF_TYPE, o := ?RDF_PROPERTY}
   ] = semantic:schema(semantic:p({iri, <<"http://xmlns.com/foaf/0.1/name">>})).

define_string(_Config) ->
   [
      #{s := ?FOAF_NAME, p := ?RDF_TYPE,     o := ?RDF_PROPERTY},
      #{s := ?FOAF_NAME, p := ?RDF_DATATYPE, o := ?XSD_STRING}
   ] = semantic:schema(semantic:p({iri, <<"http://xmlns.com/foaf/0.1/name">>}, ?XSD_STRING)).

define_lang(_Config) ->
   [
      #{s := ?FOAF_NAME, p := ?RDF_TYPE,     o := ?RDF_PROPERTY},
      #{s := ?FOAF_NAME, p := ?RDF_DATATYPE, o := ?RDF_LANG_STRING},
      #{s := ?FOAF_NAME, p := ?RDF_DATATYPE, o := ?LANG_EN}
   ] = semantic:schema(semantic:p({iri, <<"http://xmlns.com/foaf/0.1/name">>}, ?LANG_EN)).

