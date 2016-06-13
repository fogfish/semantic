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
%% @doc
%%   uri namespace prefixes compiler
-module(semantic_ns).

-export([
   encoder/2, 
   encode/3, 
   decoder/2, 
   decode/3
]).

%%
%% build prefix encoder
%%   http://www.w3.org/2001/XMLSchema#integer -> xsd:integer
encoder(Id, Kns)
 when is_atom(Id), is_list(Kns) ->
   hornlog:c(Id, [encoder(X) || X <- extend(Kns)]).

encoder({{uri, Ns}, {uri,<<"rdfs:domain">>}, {uri, Uri}}) ->
   hornlog:rule(hornlog:like(fun semantic_ns:encode/3, [Ns]), Uri).

%%
%%
encode(<<>>, _X, Suffix) ->
   Suffix;
encode(Ns, _X, <<>>) ->
   Ns;
encode(Ns, _X, Suffix) ->
   <<Ns/binary, $:, Suffix/binary>>.


%%
%% build prefix decoder
%%   xsd:integer -> http://www.w3.org/2001/XMLSchema#integer
decoder(Id, Kns)
 when is_atom(Id), is_list(Kns) ->
   hornlog:c(Id, [decoder(X) || X <- extend(Kns)]).

decoder({{uri, <<>>}, {uri,<<"rdfs:domain">>}, {uri, Uri}}) ->
   hornlog:rule(hornlog:like(fun semantic_ns:decode/3, [Uri]), <<>>);
decoder({{uri, Ns}, {uri,<<"rdfs:domain">>}, {uri, Uri}}) ->
   hornlog:rule(hornlog:like(fun semantic_ns:decode/3, [Uri]), <<Ns/binary, $:>>).

decode(<<>>, _X, Suffix) ->
   Suffix;
decode(Uri, _X, <<>>) ->
   Uri;
decode(Uri, _X, Suffix) ->
   <<Uri/binary, Suffix/binary>>.


%%
%% extends list of knowledge name-space with empty statement to fall-back prefix match
extend(Kns) -> 
   Kns ++ [{{uri, <<>>}, {uri,<<"rdfs:domain">>}, {uri, <<>>}}].
