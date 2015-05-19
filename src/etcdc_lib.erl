-module(etcdc_lib).

-export([call/4, call/5, ensure_first_slash/1,
         async_connect/2, parse_response/1]).

-define(DEFAULT_TIMEOUT, timer:seconds(10)).

%% ----------------------------------------------------------------------------

call(Method, PortType, Path, Opts) ->
    call(Method, PortType, Path, Opts, <<>>).

call(Method, PortType, Path, Opts, Value) ->
    {Host, Port} = get_server_info(PortType),
    Timeout = get_timeout(Opts),
    Url = url(Host, Port, Path, proplists:unfold(Opts)),
    case lhttpc:request(Url, Method, [], Value, Timeout) of
        {ok, {{Code, _}, _, Body}} when Code >= 200, Code =< 205 ->
            parse_response(Body);
        {ok, {{404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, _}, _, Body}} ->
            {error, parse_response(Body)};
        {error, Error} ->
            {error, Error}
    end.

ensure_first_slash([$/|_] = Path) -> Path;
ensure_first_slash(Path) -> [$/|Path].

async_connect(Path, Timeout) ->
    {Host, Port} = get_server_info(etcd_client_port),
    Opts = [binary, {active, once}, {packet, 0}],
    {ok, Sock} = gen_tcp:connect(Host, Port, Opts),
    P = case is_binary(Path) of true -> Path; false -> list_to_binary(Path) end,
    Pl = <<"GET ", P/binary, " HTTP/1.1\r\n\r\n">>,
    ok = gen_tcp:send(Sock, Pl),
    receive
        {tcp, Sock, Headers} ->
            {ok, Headers, Sock}
    after Timeout ->
        {error, timeout}
    end.

parse_response(Body) ->
    case lejson:decode(Body) of
        {error, not_json} ->
            Body;
        Json ->
            simplify_json(Json)
    end.

%% ----------------------------------------------------------------------------

get_server_info(PortType) ->
    {ok, Host} = application:get_env(etcdc, etcd_host),
    {ok, Port} = application:get_env(etcdc, PortType),
    {Host, Port}.

get_timeout(Opts) ->
    case proplists:get_bool(wait, Opts) of
        true ->
            infinity;
        false ->
            proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT)
    end.

url(Host, Port, Path, Query) when is_integer(Port) ->
    Base  = ["http://", Host, ":", integer_to_list(Port)],
    NewPath = ensure_first_slash(Path),
    case parse_query(Query) of
        [] ->
            lists:flatten([Base, NewPath]);
        Qs ->
            lists:flatten([Base, NewPath, "?", Qs])
    end.

parse_query(Qs) ->
    string:join([ to_str(K) ++ "=" ++ to_str(V) || {K, V} <- Qs ], "&").

to_str(T) when is_atom(T) -> url_encode(atom_to_list(T));
to_str(T) when is_integer(T) -> url_encode(integer_to_list(T));
to_str(T) when is_float(T) -> url_encode(float_to_list(T));
to_str(T) when is_binary(T) -> url_encode(binary_to_list(T));
to_str(T) -> url_encode(T).

simplify_json(Map) when is_map(Map) ->
    Ls = maps:to_list(Map),
    maps:from_list([ {to_underscore(K), simplify_json(V)} || {K, V} <- Ls ]);
simplify_json(List) when is_list(List) ->
    [ simplify_json(E) || E <- List ];
simplify_json(Value) ->
    Value.

to_underscore(Word) when is_binary(Word) ->
    to_underscore(binary_to_list(Word));
to_underscore(Word) ->
    list_to_atom(string:join(to_underscore(Word, [], []), "_")).

to_underscore([C,D|Word], Stack, Res) when C >= $A, C =< $Z, D >= $a, D =< $z ->
    to_underscore([D|Word], [C], [lists:reverse(Stack)|Res]);
to_underscore([C,D|Word], Stack, Res) when C >= $a, C =< $z, D >= $A, D =< $Z ->
    to_underscore([D|Word], [], [lists:reverse([C|Stack])|Res]);
to_underscore([C|Word], Stack, Res) ->
    to_underscore(Word, [C|Stack], Res);
to_underscore([], Stack, Res) ->
    [ string:to_lower(P) || P <- lists:reverse([lists:reverse(Stack)|Res]),
                            length(P) > 0 ].

url_encode(Value) ->
    process_chars(Value, safe_chars(), []).

process_chars([$\s|Value], Safe, Acc) ->
    process_chars(Value, Safe, [$+|Acc]);
process_chars([C|Value], Safe, Acc) ->
    case lists:member(C, Safe) of
        true ->
            process_chars(Value, Safe, [C|Acc]);
        false ->
           process_chars(Value, Safe, [encode_char(C)|Acc])
    end;
process_chars([], _, Acc) ->
    lists:reverse(Acc).

encode_char(Char) ->
    <<Hi:4, Lo:4>> = <<Char:8>>,
    [$%, hex(Hi), hex(Lo)].

hex(10) -> $a;
hex(11) -> $b;
hex(12) -> $c;
hex(13) -> $d;
hex(14) -> $e;
hex(15) -> $f;
hex(N) when N < 10 -> $0 + N.

safe_chars() ->
    lists:append([lists:seq($a, $z),
                  lists:seq($A, $Z),
                  lists:seq($0, $9),
                  [$., $-, $~, $_]]).
