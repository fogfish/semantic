%%
%%! -smp -sbt ts +A10 +K true -pa _build/default/lib/semantic/ebin -pa _build/default/lib/feta/ebin -pa _build/default/lib/datum/ebin
-module(prefixes).

-export([main/1]).

main(_) ->
   Prefixes = prefixes(),
   Decoder  = lists:map(fun(X) -> decoder(X) end, Prefixes) ++ [decoder()],
   Encoder  = lists:map(fun(X) -> encoder(X) end, Prefixes) ++ [encoder()],
   {ok, FD} = file:open("./src/semantic_ns.erl", [write, binary, raw]),
   file:write(FD, <<"-module(semantic_ns).\n\n">>),
   file:write(FD, <<"-export([decode/1, encode/1]).\n\n">>),
   file:write(FD, Decoder),
   file:write(FD, Encoder),
   file:close(FD).

%%
prefixes() ->
   stream:list(
      semantic_nt:decode(
         stdio:file(
            filename:join([code:priv_dir(semantic), "prefixes.nt"])
         )
      )
   ).

%%
decoder({{iri, Prefix}, {iri, <<"rdfs:domain">>}, {iri, Uri}}) ->
   <<"decode(<<\"", Uri/binary, "\", Suffix/binary>>) -> {<<\"", Prefix/binary, "\">>, Suffix};\n">>.

decoder() ->
   <<"decode(Suffix) -> {undefined, Suffix}.\n\n">>.

%%
encoder({{iri, Prefix}, {iri, <<"rdfs:domain">>}, {iri, Uri}}) ->
   <<"encode(<<\"", Prefix/binary, "\">>) -> <<\"", Uri/binary, "\">>;\n">>.

encoder() ->
   <<"encode(_) -> undefined.\n\n">>.
