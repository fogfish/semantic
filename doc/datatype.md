# Data Types

Knowledge statements contain scalar objects -- literals. Literals are either language-tagged string rdf:langString or type-safe values containing a reference to data-type (e.g. xsd:string). This section defines rules how semantic data types are serialized to Erlang native formats.

## Primitive Data Types

Knowledge statements contain scalar objects -- literals. Literals are either language-tagged string `rdf:langString` or _type-safe_ values containing a reference to data-type (e.g. `xsd:string`). This section defines data-types supported by the library, its mapping to Erlang native types and relation to existed schema(s) and ontologies.  

### URI / IRI

The data type represents a Uniform Resource Identifier / Internationalized Resource Identifier. Used to uniquely identify concept, objects, etc.

Lang | Data type
---  | ---
Erlang | `-type {iri, uri()} | {iri, prefix(), suffix()}.`
Semantic | `?XSD_ANYURI`
Text | `<uri>`
|| `prefix:suffix`
RDF | xmlns:xsd="http://www.w3.org/2001/XMLSchema#string"


### binary

The binary data-type represents character strings in knowledge statements. The language strings are annotated with corresponding language tag.

Lang | Data type
---  | ---
Erlang | `-type binary().`
Semantic | `?XSD_STRING`
Text | `"string"`
|| `"string"^^xsd:string`
|| `"string"@en`
RDF | xmlns:xsd="http://www.w3.org/2001/XMLSchema#string"


### integer

The library derives an arbitrary-precision Integers from Erlang (only limited by available memory on the machine).

Lang | Data type
---  | ---
Erlang | `-type integer().`
Semantic | `?XSD_INTEGER`
|| `?XSD_LONG`
|| `?XSD_INT`
|| `?XSD_SHORT`
|| `?XSD_BYTE`
Text | `10`
|| `"10"^^xsd:integer`
RDF | xmlns:xsd="http://www.w3.org/2001/XMLSchema#integer"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#long"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#int"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#short"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#byte"


### float

The value is the IEEE 754 double-precision 64-bit floating point type.

Lang | Data type
---  | ---
Erlang | `-type float().`
Semantic | `?XSD_DECIMAL`
|| `?XSD_FLOAT`
|| `?XSD_DOUBLE`
Text | `10.1`
|| `"10.1"^^xsd:decimal`
RDF | xmlns:xsd="http://www.w3.org/2001/XMLSchema#decimal"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#float"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#double"


### boolean

The value is either true or false, representing a logic values

Lang | Data type
---  | ---
Erlang | `-type true \| false.`
Semantic | `?XSD_BOOLEAN`
Text | `true | false`
RDF | xmlns:xsd="http://www.w3.org/2001/XMLSchema#boolean"


### datetime

The date-time value is closely related to calendar dates and times described by ISO 8601, covering AD and BC eras. Erlang native date format is triple of integers giving micro-seconds precision. The external representation is ISO 8601.

Lang | Data type
---  | ---
Erlang | `-type {integer(), integer(), integer()}.`
Semantic | `?XSD_DATETIME`
Text | ISO8601
|| `2007-04-05T14:30:00Z`
RDF | xmlns:xsd="http://www.w3.org/2001/XMLSchema#dateTime"

### recursive date/times

 XSD defines set of recursive data-types. These types are also translated to native datetime format.  

Lang | Data type
---  | ---
Erlang | `-type {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.`
Semantic | `?XSD_DATE`
|| `?XSD_TIME`
|| `?XSD_YEARMONTH`
|| `?XSD_YEAR`
|| `?XSD_MONTHDAY`
|| `?XSD_MONTH`
|| `?XSD_DAY`
Text | ISO8601
|| `2007-04-05`
|| `14:30:00Z`
|| `2007-04`
|| `--04-05`
|| `2007`
|| `04`
|| `05`
RDF | xmlns:xsd="http://www.w3.org/2001/XMLSchema#date"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#time"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#gYearMonth"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#gYear"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#gMonthDay"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#gMonth"
|| xmlns:xsd="http://www.w3.org/2001/XMLSchema#gDay"


### Geo Point

The geographical coordinates are encoded using either GeoHash or latitude/longitude pair. GeoHash is encoded using `georss:hash` that ensures precision about 3.7cm x 1.8cm. The latitude/longitude pair is encoded `georss:point` as tuple of floats.

Lang | Data type
---  | ---
Erlang | `-type binary() | {lat(), lng()}.`
Semantic | `?GEORSS_HASH`
|| `?GEORSS_POINT`
Text | ueh6xc
|| 25.7,62.1
RDF | xmlns:georss="http://www.georss.org/georss/point"
|| xmlns:georss="http://www.georss.org/georss/hash"

### GeoJson

The data type facilitates operations with geospatial data as defined by https://tools.ietf.org/html/rfc7946

Lang | Data type
---  | ---
Erlang | `-type #{<<"type">> => _, <<"coordinates">> => _}.`
Semantic | `?GEORSS_JSON`
Text | (type: ..., coordinates: ...)
RDF | xmlns:georss="http://www.georss.org/georss/json"


### not supported 

* xmlns:xsd="http://www.w3.org/2001/XMLSchema#duration"
* xmlns:xsd="http://www.w3.org/2001/XMLSchema#hexBinary"
* xmlns:xsd="http://www.w3.org/2001/XMLSchema#base64Binary"


## Reference
1. http://www.w3.org/TR/xmlschema-2/
2. https://en.wikipedia.org/wiki/Geohash

