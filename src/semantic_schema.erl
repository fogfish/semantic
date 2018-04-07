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
-module(semantic_schema).

-include("semantic.hrl").
-include_lib("datum/include/datum.hrl").


-export([
   deduct/1,
   property/1,
   property/2,
   define/1
]).


%%
%% deduct schema of knowledge statements
-spec deduct(semantic:heap(semantic:spock())) -> semantic:heap(#rdf_property{}).

deduct(#stream{} = Stream) ->
   gb_sets:to_list(
      stream:fold(fun deduct/2, gb_sets:new(), Stream)
   );

deduct([_|_] = List) ->
   gb_sets:to_list(
      lists:foldl(fun deduct/2, gb_sets:new(), List)
   ).

deduct(#{p := P, type := Type}, Set) ->
   gb_sets:union(gb_sets:from_list(property(P, Type)), Set);

deduct(#{p := P}, Set) ->
   gb_sets:union(gb_sets:from_list(property(P)), Set).



%%
%% define new predicate into global schema
-spec property(#rdf_property{}) -> #rdf_property{}. %[semantic:spock()].

property(#rdf_property{id = IRI, datatype = DataType} = Property) ->
   {iri, _, _} = Uid = semantic:compact(IRI),
   {iri, _, _} = IsA = semantic:compact(DataType),
   Property#rdf_property{id = Uid, datatype = IsA};

property({iri, _, _} = IRI) ->
   #rdf_property{id = IRI};

property(IRI) ->
   {iri, _, _} = Uid = semantic:compact(IRI),
   #rdf_property{id = Uid}.

%%
%%
-spec property(semantic:iri(), semantic:iri()) -> #rdf_property{}.

property({iri, _, _} = IRI, DataType) ->
   {iri, _, _} = IsA = semantic:compact(DataType),
   #rdf_property{id = IRI, datatype = IsA};

property(IRI, DataType) ->
   {iri, _, _} = Uid = semantic:compact(IRI),
   {iri, _, _} = IsA = semantic:compact(DataType),
   #rdf_property{id = Uid, datatype = IsA}.
   
%%
%%
-spec define(#rdf_property{}) -> [semantic:spock()].

define(#rdf_property{} = Property) ->
   rdf_property(Property).

%%
rdf_property(#rdf_property{id = IRI} = Property) ->
   [#{
      s => IRI,
      p => ?RDF_TYPE,
      o => ?RDF_PROPERTY,
      c => 1.0,
      k => uid:l()
   } | rdf_datatype(Property)].

%%
rdf_datatype(#rdf_property{datatype = undefined} = Property) ->
   rdf_functional(Property);

rdf_datatype(#rdf_property{id = IRI, datatype = {iri, ?LANG, _} = DataType} = Property) ->
   [#{
      s => IRI,
      p => ?RDF_DATATYPE,
      o => ?RDF_LANG_STRING,
      c => 1.0,
      k => uid:l()
   },
   #{
      s => IRI,
      p => ?RDF_DATATYPE,
      o => DataType,
      c => 1.0,
      k => uid:l()
   } | rdf_functional(Property)];

rdf_datatype(#rdf_property{id = IRI, datatype = DataType} = Property) ->
   [#{
      s => IRI,
      p => ?RDF_DATATYPE,
      o => DataType,
      c => 1.0,
      k => uid:l()
   } | rdf_functional(Property)].   

%%
rdf_functional(#rdf_property{single = false} = Property) ->
   rdf_inverse(Property);
rdf_functional(#rdf_property{id = IRI} = Property) ->
   [#{
      s => IRI,
      p => ?RDF_TYPE,
      o => ?OWL_SINGLE,
      c => 1.0,
      k => uid:l()
   } | rdf_inverse(Property)].

%%
rdf_inverse(#rdf_property{unique = false}) ->
   [];
rdf_inverse(#rdf_property{id = IRI} = Property) ->
   [#{
      s => IRI,
      p => ?RDF_TYPE,
      o => ?OWL_UNIQUE,
      c => 1.0,
      k => uid:l()
   } | rdf_inverse(Property)].

