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
   ok = semantic:prefixes(),
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

define_uri(_Config) ->
   [
      #{s := {uri, <<"foaf:name">>}, p := {uri, <<"rdf:type">>}, o := {uri, <<"rdf:Property">>}}
   ] = semantic:define(<<"http://xmlns.com/foaf/0.1/name">>, uri).

define_string(_Config) ->
   [
      #{s := {uri, <<"foaf:name">>}, p := {uri, <<"rdf:type">>},  o := {uri, <<"rdf:Property">>}},
      #{s := {uri, <<"foaf:name">>}, p := {uri, <<"rdf:range">>}, o := {uri, <<"xsd:string">>}}
   ] = semantic:define(<<"http://xmlns.com/foaf/0.1/name">>, binary).

define_lang(_Config) ->
   [
      #{s := {uri, <<"foaf:name">>}, p := {uri, <<"rdf:type">>},  o := {uri, <<"rdf:Property">>}},
      #{s := {uri, <<"foaf:name">>}, p := {uri, <<"rdf:range">>}, o := {uri, <<"rdf:langString">>}},
      #{s := {uri, <<"foaf:name">>}, p := {uri, <<"rdf:range">>}, o := <<"en">>}
   ] = semantic:define(<<"http://xmlns.com/foaf/0.1/name">>, <<"en">>).

