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
-module(semantic_nt_SUITE).
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
   spo_with_uri/1,
   spo_with_blanknodes/1,
   spo_with_lit/1,
   spo_with_lang/1,
   spo_with_type/1,
   spo_chunked_input/1,
   spo_with_comment/1,
   spo_stream/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, decode}
   ].

groups() ->
   [
      {decode, [parallel], 
         [spo_with_uri, spo_with_blanknodes, spo_with_lit, spo_with_lang, spo_with_type,
          spo_chunked_input, spo_with_comment, spo_stream]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
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

spo_with_uri(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> <http://example.org/c> .\n">>,
   {
      [{
         {uri, <<"http://example.org/a">>}, 
         {uri, <<"http://example.org/b">>}, 
         {uri, <<"http://example.org/c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  

spo_with_blanknodes(_) ->
   A = <<"_:a _:b _:c .\n">>,
   {
      [{
         {uri, <<"a">>}, 
         {uri, <<"b">>}, 
         {uri, <<"c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  


spo_with_lit(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> \"c\" .\n">>,
   {
      [{
         {uri, <<"http://example.org/a">>}, 
         {uri, <<"http://example.org/b">>}, 
         <<"c">>
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  



spo_with_lang(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> \"c\"@en .\n">>,
   {
      [{
         {uri, <<"http://example.org/a">>}, 
         {uri, <<"http://example.org/b">>}, 
         {<<"en">>, <<"c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  



spo_with_type(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> \"c\"^^<http://www.w3.org/2001/XMLSchema#string> .\n">>,
   {
      [{
         {uri, <<"http://example.org/a">>}, 
         {uri, <<"http://example.org/b">>}, 
         {<<"http://www.w3.org/2001/XMLSchema#string">>, <<"c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  


spo_chunked_input(_) ->
   A = <<"<http://example.org/a> ">>,
   B = <<"<http://example.org/b> ">>,
   C = <<"<http://example.org/c> .\n">>,
   {[], Nt1} = semantic_nt:decode(A, semantic_nt:new()),
   {[], Nt2} = semantic_nt:decode(B, Nt1),
   {
      [{
         {uri, <<"http://example.org/a">>},
         {uri, <<"http://example.org/b">>}, 
         {uri, <<"http://example.org/c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(C, Nt2).  
   
spo_with_comment(_) ->
   A = <<"# comment\n<http://example.org/a> <http://example.org/b> <http://example.org/c> .\n">>,
   {
      [{
         {uri, <<"http://example.org/a">>},
         {uri, <<"http://example.org/b">>},
         {uri, <<"http://example.org/c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  


spo_stream(_) ->
   A = <<"_:a _:b _:c .\n_:d _:e _:f .\n_:q _:a _:z .\n">>,
   [
      {{uri, <<"a">>}, {uri, <<"b">>}, {uri, <<"c">>}},
      {{uri, <<"d">>}, {uri, <<"e">>}, {uri, <<"f">>}},
      {{uri, <<"q">>}, {uri, <<"a">>}, {uri, <<"z">>}}

   ] = stdio:list( semantic_nt:stream( stdio:new(A) ) ).  
