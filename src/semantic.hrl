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

%%
-define(LANG,    <<"lang">>).

-define(RDF_TYPE,        {iri, <<"rdf">>, <<"type">>}).
-define(RDF_PROPERTY,    {iri, <<"rdf">>, <<"Property">>}).
-define(RDF_RANGE,       {iri, <<"rdf">>, <<"range">>}).
-define(RDF_LANG_STRING, {iri, <<"rdf">>, <<"langString">>}).

%%
-define(XSD_STRING,      {iri, <<"xsd">>, <<"string">>}).

-define(XSD_INTEGER,     {iri, <<"xsd">>, <<"integer">>}).

-define(XSD_BYTE,        {iri, <<"xsd">>, <<"byte">>}).
-define(XSD_SHORT,       {iri, <<"xsd">>, <<"short">>}).
-define(XSD_INT,         {iri, <<"xsd">>, <<"int">>}).
-define(XSD_LONG,        {iri, <<"xsd">>, <<"long">>}).

-define(XSD_DECIMAL,     {iri, <<"xsd">>, <<"decimal">>}).
-define(XSD_FLOAT,       {iri, <<"xsd">>, <<"float">>}).
-define(XSD_DOUBLE,      {iri, <<"xsd">>, <<"double">>}).

-define(XSD_BOOLEAN,     {iri, <<"xsd">>, <<"boolean">>}).

-define(XSD_DATETIME,    {iri, <<"xsd">>, <<"dateTime">>}).
-define(XSD_DATE,        {iri, <<"xsd">>, <<"date">>}).
-define(XSD_TIME,        {iri, <<"xsd">>, <<"time">>}).
-define(XSD_YEARMONTH,   {iri, <<"xsd">>, <<"gYearMonth">>}).
-define(XSD_YEAR,        {iri, <<"xsd">>, <<"gYear">>}).
-define(XSD_MONTHDAY,    {iri, <<"xsd">>, <<"gMonthDay">>}).
-define(XSD_MONTH,       {iri, <<"xsd">>, <<"gMonth">>}).
-define(XSD_DAY,         {iri, <<"xsd">>, <<"gDay">>}).

-define(GEORSS_POINT,    {iri, <<"georss">>, <<"point">>}).
-define(GEORSS_HASH,     {iri, <<"georss">>, <<"hash">>}).


