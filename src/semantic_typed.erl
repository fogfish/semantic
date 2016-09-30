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
%%   compile abstract syntax triple to type-safe representation
-module(semantic_typed).
-include("semantic.hrl").

-export([
   c/1
]).


%%
%% compile abstract syntax knowledge statement to type-safe once
c({{iri, _}, {iri, _}, _} = Fact) ->
   compile(Fact);

c({s, _, _} = Stream) ->
   stream:map(fun compile/1, Stream);

c([_|_] = List) ->
   lists:map(fun compile/1, List).


compile({{iri, _} = S, {iri, _} = P, {iri, _} = O}) ->
   #{
      s => semantic:compact(S),
      p => semantic:compact(P),
      o => semantic:compact(O),
      c => 1.0,
      k => uid:l()
   };

compile({{iri, _} = S, {iri, _} = P, {{iri, ?LANG, _} = Type, O}}) ->
   #{
      s => semantic:compact(S),
      p => semantic:compact(P),
      o => O,
      c => 1.0,
      k => uid:l(),
      type => Type
   };

compile({{iri, _} = S, {iri, _} = P, {{iri, _} = Type, O}}) ->
   decode(semantic:compact(Type), O,
      #{
         s => semantic:compact(S),
         p => semantic:compact(P),
         c => 1.0,
         k => uid:l()
      }
   );

compile({{iri, _} = S, {iri, _} = P, O}) ->
   decode(O,
      #{
         s => semantic:compact(S),
         p => semantic:compact(P),
         c => 1.0,
         k => uid:l()
      }
   ).


%%
%% type-safe decode
decode(?XSD_STRING = Type, O, Fact) ->
   Fact#{o => O, type => Type};

%%
decode(?XSD_INTEGER = Type, O, Fact) -> 
   Fact#{o => scalar:i(O), type => Type};

%%
decode(?XSD_BYTE = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_SHORT = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_INT = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};

decode(?XSD_LONG = Type, O, Fact) ->
   Fact#{o => scalar:i(O), type => Type};


%%
decode(?XSD_DECIMAL = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

decode(?XSD_FLOAT = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

decode(?XSD_DOUBLE = Type, O, Fact) ->
   Fact#{o => scalar:f(O), type => Type};

%%
decode(?XSD_BOOLEAN = Type, <<"true">>, Fact) ->
   Fact#{o => true, type => Type};

decode(?XSD_BOOLEAN = Type, <<"false">>, Fact) ->
   Fact#{o => false, type => Type};

decode(?XSD_BOOLEAN = Type, <<"1">>, Fact) ->
   Fact#{o => true, type => Type};

decode(?XSD_BOOLEAN = Type, <<"0">>, Fact) ->
   Fact#{o => false, type => Type};

%%
decode(?XSD_DATETIME = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_DATE = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_TIME = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_YEARMONTH = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_YEAR = Type, O, Fact) ->
   Fact#{o => tempus:iso8601(O), type => Type};

decode(?XSD_MONTHDAY = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?XSD_MONTH = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?XSD_DAY = Type, O, Fact) ->
   Fact#{o => O, type => Type};

%%
decode(?GEORSS_HASH = Type, O, Fact) ->
   Fact#{o => O, type => Type};

decode(?GEORSS_POINT = Type, O, Fact) ->
   [Lat, Lng] = binary:split(O, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => Type}. 


%%
%% relax decode
decode(<<"true">>, Fact) ->
   Fact#{o => true,  type => ?XSD_BOOLEAN};

decode(<<"false">>, Fact) ->
   Fact#{o => false, type => ?XSD_BOOLEAN};

decode(LatLng, #{p := {iri, <<"georss">>, <<"point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => ?GEORSS_POINT}; 

decode(LatLng, #{p := {iri, <<"http://www.georss.org/georss/point">>}} = Fact) ->
   [Lat, Lng] = binary:split(LatLng, <<$ >>), 
   Fact#{o => hash:geo(scalar:f(Lat), scalar:f(Lng)), type => ?GEORSS_POINT};

decode(O, Fact) ->
   case scalar:decode(O) of
      X when is_integer(X) ->
         Fact#{o => X, type => ?XSD_INTEGER};
      X when is_float(X) ->
         Fact#{o => X, type => ?XSD_DOUBLE};
      X when is_boolean(X) ->
         Fact#{o => X, type => ?XSD_BOOLEAN};
      X when is_binary(X) ->
         Fact#{o => X, type => ?XSD_STRING}
   end.
