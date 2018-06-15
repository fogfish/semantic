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
   [
      encoder,
      decoder
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

encoder(_Config) -> 
   A  = [{{iri, <<"xsd">>}, {iri, <<"rdfs:domain">>}, {iri, <<"http://www.w3.org/2001/XMLSchema#">>}}],
   {module, test_encoder} = semantic_ns:encoder(test_encoder, A),
   {<<"xsd">>, <<"int">>} = test_encoder:q(undefined, <<"http://www.w3.org/2001/XMLSchema#int">>),
   {<<"xsd">>, <<>>} = test_encoder:q(undefined, <<"http://www.w3.org/2001/XMLSchema#">>),
   <<"http://www.w3.org">> = test_encoder:q(undefined, <<"http://www.w3.org">>).

decoder(_Config) ->
   A  = [{{iri, <<"xsd">>}, {iri, <<"rdfs:domain">>}, {iri, <<"http://www.w3.org/2001/XMLSchema#">>}}],
   {module, test_decoder} = semantic_ns:decoder(test_decoder, A),
   <<"http://www.w3.org/2001/XMLSchema#">> = test_decoder:q(undefined, <<"xsd">>),
   <<"foaf:name">> = test_decoder:q(undefined, <<"foaf:name">>).


