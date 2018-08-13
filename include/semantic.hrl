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

%%%------------------------------------------------------------------
%%%
%%% schema definition
%%%
%%%------------------------------------------------------------------

%% built in prefixes
-define(RDF,  <<"rdf">>).
-define(OWL,  <<"owl">>).
-define(XSD,  <<"xsd">>).

-define(LANG, <<"langString">>).

%% rdfs schema
-define(RDF_TYPE,        {iri, ?RDF, <<"type">>}).
-define(RDF_DATATYPE,    {iri, ?RDF, <<"datatype">>}).
-define(RDF_PROPERTY,    {iri, ?RDF, <<"Property">>}).
-define(RDF_SEQ,         {iri, ?RDF, <<"Seq">>}).
-define(RDF_RANGE,       {iri, ?RDF, <<"range">>}).
-define(RDF_LANG_STRING, {iri, ?RDF, <<"langString">>}).

-define(OWL_SINGLE,      {iri, ?OWL, <<"FunctionalProperty">>}).
-define(OWL_UNIQUE,      {iri, ?OWL, <<"InverseFunctionalProperty">>}).

%% built-int predicates
-define(RDF_ID,          {iri, ?RDF, <<"id">>}).

-define(IRI_LANG(X),     {iri, ?LANG, X}).
%%%------------------------------------------------------------------
%%%
%%% semantic data types (built in)
%%%
%%%------------------------------------------------------------------

%% reference
-define(XSD_ANYURI,      {iri, ?XSD,  <<"anyURI">>}).

%% string
-define(XSD_STRING,      {iri, ?XSD, <<"string">>}).

%% integer
-define(XSD_INTEGER,     {iri, ?XSD, <<"integer">>}).

-define(XSD_BYTE,        {iri, ?XSD, <<"byte">>}).
-define(XSD_SHORT,       {iri, ?XSD, <<"short">>}).
-define(XSD_INT,         {iri, ?XSD, <<"int">>}).
-define(XSD_LONG,        {iri, ?XSD, <<"long">>}).

%% float
-define(XSD_DECIMAL,     {iri, ?XSD, <<"decimal">>}).
-define(XSD_FLOAT,       {iri, ?XSD, <<"float">>}).
-define(XSD_DOUBLE,      {iri, ?XSD, <<"double">>}).

%% boolean
-define(XSD_BOOLEAN,     {iri, ?XSD, <<"boolean">>}).

%% date
-define(XSD_DATETIME,    {iri, ?XSD, <<"dateTime">>}).
-define(XSD_DATE,        {iri, ?XSD, <<"date">>}).
-define(XSD_TIME,        {iri, ?XSD, <<"time">>}).
-define(XSD_YEARMONTH,   {iri, ?XSD, <<"gYearMonth">>}).
-define(XSD_YEAR,        {iri, ?XSD, <<"gYear">>}).
-define(XSD_MONTHDAY,    {iri, ?XSD, <<"gMonthDay">>}).
-define(XSD_MONTH,       {iri, ?XSD, <<"gMonth">>}).
-define(XSD_DAY,         {iri, ?XSD, <<"gDay">>}).

%% geo hash
-define(GEORSS_POINT,    {iri, <<"georss">>, <<"point">>}).
-define(GEORSS_HASH,     {iri, <<"georss">>, <<"hash">>}).
-define(GEORSS_JSON,     {iri, <<"georss">>, <<"json">>}).

