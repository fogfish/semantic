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

-export([all/0]).
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

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

spo_with_uri(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> <http://example.org/c> .\n">>,
   {
      [{
         {iri, <<"http://example.org/a">>}, 
         {iri, <<"http://example.org/b">>}, 
         {iri, <<"http://example.org/c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  

spo_with_blanknodes(_) ->
   A = <<"_:a _:b _:c .\n">>,
   {
      [{
         {iri, <<"a">>}, 
         {iri, <<"b">>}, 
         {iri, <<"c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  


spo_with_lit(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> \"c\" .\n">>,
   {
      [{
         {iri, <<"http://example.org/a">>}, 
         {iri, <<"http://example.org/b">>}, 
         <<"c">>
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  



spo_with_lang(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> \"c\"@en .\n">>,
   {
      [{
         {iri, <<"http://example.org/a">>}, 
         {iri, <<"http://example.org/b">>}, 
         {{iri, <<"langString">>, <<"en">>}, <<"c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  



spo_with_type(_) ->
   A = <<"<http://example.org/a> <http://example.org/b> \"c\"^^<http://www.w3.org/2001/XMLSchema#string> .\n">>,
   {
      [{
         {iri, <<"http://example.org/a">>}, 
         {iri, <<"http://example.org/b">>}, 
         {{iri, <<"http://www.w3.org/2001/XMLSchema#string">>}, <<"c">>}
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
         {iri, <<"http://example.org/a">>},
         {iri, <<"http://example.org/b">>}, 
         {iri, <<"http://example.org/c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(C, Nt2).  
   
spo_with_comment(_) ->
   A = <<"# comment\n<http://example.org/a> <http://example.org/b> <http://example.org/c> .\n">>,
   {
      [{
         {iri, <<"http://example.org/a">>},
         {iri, <<"http://example.org/b">>},
         {iri, <<"http://example.org/c">>}
      }],
      {nt, <<>>}
   } = semantic_nt:decode(A, semantic_nt:new()).  


spo_stream(_) ->
   A = <<"_:a _:b _:c .\n_:d _:e _:f .\n_:q _:a _:z .\n">>,
   [
      {{iri, <<"a">>}, {iri, <<"b">>}, {iri, <<"c">>}},
      {{iri, <<"d">>}, {iri, <<"e">>}, {iri, <<"f">>}},
      {{iri, <<"q">>}, {iri, <<"a">>}, {iri, <<"z">>}}

   ] = stdio:list( semantic_nt:decode( stdio:new(A) ) ).  
