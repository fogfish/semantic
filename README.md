# semantic

Semantic Web ToolKit for Erlang applications

[![Build Status](https://secure.travis-ci.org/fogfish/semantic.svg?branch=master)](http://travis-ci.org/fogfish/semantic)

## Inspiration

Linked-Data are used widely by Semantic Web to publish structured data so that it can be interlinked by application. The library provides toolkit for Erlang application and defines an data model (abstract syntax) to interpret and manipulate Linked-Data as a collection of ground facts / knowledge statement / triples. You can use it to build inter-operable Web services or store Linked Data in storage engines. The library solve a following aspects:
* define Erlang native format for knowledge statement; its serialization to various external formats
* provide common rules for data type mapping between Semantic Web and Erlang application
* resolve complexity of IRI identity through compact prefixes and urn-base schema



## Getting started

The latest version of the library is available at its master branch. All development, including new features and bug fixes, take place on the master branch using forking and pull requests as described in contribution guidelines.

### Installation

If you use `rebar` you can include `semantic` library in your project with
```
{semantic, ".*",
   {git, "https://github.com/fogfish/semantic", {branch, master}}
}
```

### Usage

The library exposes _public_ interface through exports of [semantic.erl](src/semantic.erl) module. Just call required function with required arguments, check out _Supported features_ and _More Information_ chapter for details. 

Build library and run the development console
```
make
make run
```

Let's take a short tour to the library
```erlang
%% configure built-in prefixes for compact IRI/URI 
semantic:prefixes().

%% intake knowledge fact from N-triple data source
A = stream:head(semantic:nt(<<"<http://example.org/a> <http://xmlns.com/foaf/0.1/name> \"text\"@en .\n">>)).

%% check the type of statement
semantic:typeof(A).

%% intake knowledge fact from JSON-LD data source
JsonLD = #{
   <<"@context">> => #{<<"price">> => <<"http://schema.org/price">>},
   <<"@id">> => <<"http://example.org/a">>,
   <<"price">> => 20
}.
B = hd(semantic:jsonld(JsonLD))

%% check the type of statement
semantic:typeof(B).
``` 


## Supported features


### Native format

The library expresses knowledge fact(s) using map as 'type-safe' container type (the statement type is expressed using Erlang native [data types](doc/datatype.md) mapping). This statement is compiled using `semantic:typed/1` function, that takes an abstract triple format and produces a native one. 

```erlang
-type spo()  :: #{s => s(), p => p(), o => o(), type => lang() | type()}.
```


### Compact identifiers

Compact identifiers is an approach to reduce IRI / URI overhead using `<prefix> : <suffix>` notation. The library implements compaction automatically using [built-in dictionary of prefixes](priv/prefixes.nt). For example the library applies following reduction to uri:

```
http://xmlns.com/foaf/0.1/name  ->  foaf:name
```


### Knowledge intake

The library supports multiple external serialization formats for knowledge intake:
* N-triples
* JSON-LD


### More Information

* [RDF 1.1 -- Concepts and Abstract Syntax](http://www.w3.org/TR/2014/PR-rdf11-concepts-20140109/)
* [JSON-LD 1.0 -- A JSON-based Serialization for Linked Data](http://www.w3.org/TR/json-ld/)
* study [native interface](src/semantic.erl)
* supported [data type](doc/datatype.md)
* look into [how-to examples](doc/howto.md)


## How to Contribute

`semantic` is Apache 2.0 licensed and accepts contributions via GitHub pull requests.

### getting started

* Fork the repository on GitHub
* Read the README.md for build instructions
* Make pull request

### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>

## Bugs

If you detect a bug, please bring it to our attention via GitHub issues. Please make your report detailed and accurate so that we can identify and replicate the issues you experience:
- specify the configuration of your environment, including which operating system you're using and the versions of your runtime environments
- attach logs, screen shots and/or exceptions if possible
- briefly summarize the steps you took to resolve or reproduce the problem

## References

1. http://www.w3.org/TR/2014/PR-rdf11-concepts-20140109/


## License

Copyright 2014 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.



