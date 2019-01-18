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
-module(semantic_ns_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([
   encoder/1,
   decoder/1
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

encoder(_Config) -> 
   {<<"xsd">>, <<"int">>} = semantic_ns:decode(<<"http://www.w3.org/2001/XMLSchema#int">>),
   {<<"xsd">>, <<>>} = semantic_ns:decode(<<"http://www.w3.org/2001/XMLSchema#">>),
   {undefined, <<"http://www.w3.org">>} = semantic_ns:decode(<<"http://www.w3.org">>).

decoder(_Config) ->
   <<"http://www.w3.org/2001/XMLSchema#">> = semantic_ns:encode(<<"xsd">>),
   undefined = semantic_ns:encode(<<"foaf:name">>).


