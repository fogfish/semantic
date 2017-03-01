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
%%   semantic web toolkit
-module(semantic_app).
-behaviour(application).

-include("semantic.hrl").

-export([
   start/2
  ,stop/1
]).

%%
%%
start(_Type, _Args) ->
   schema(),
   config(),
   {ok, self()}. 

%%
%%
stop(_State) ->
   ok.

%%
%%
config() ->
   Default = filename:join([code:priv_dir(semantic), "prefixes.nt"]),
   prefixes( opts:val(prefixes, Default, semantic) ).   

prefixes(File) ->
   prefixes(semantic_ns_encode, semantic_ns_decode, 
      semantic_nt:decode(
         stdio:file(File)
      )
   ).

prefixes(Enc, Dec, {s, _, _} = Kns) ->
   prefixes(Enc, Dec, stream:list(Kns));

prefixes(Enc, Dec, Kns)
 when is_list(Kns) ->
   {module, Enc} = semantic_ns:encoder(Enc, Kns),
   {module, Dec} = semantic_ns:decoder(Dec, Kns),
   ok.

%%
%% schema table
schema() ->
   _  = ets:new(semantic, [
      public
     ,named_table
     ,set
     ,{keypos, #rdf_property.id}
     ,{read_concurrency,  true}
   ]).


