-module(etcdc_lib).

-export([call/4, call/5, ensure_first_slash/1, parse_response/1]).

-export([url_encode/1, url_decode/1]).

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

%% Url encode/decode ----------------------------------------------------------

url_encode(Value) ->
    encode_chars(Value, safe_chars(), []).

encode_chars([$\s|Value], Safe, Acc) ->
    encode_chars(Value, Safe, [$+|Acc]);
encode_chars([C|Value], Safe, Acc) ->
    case lists:member(C, Safe) of
        true ->
            encode_chars(Value, Safe, [C|Acc]);
        false ->
           encode_chars(Value, Safe, [encode_char(C)|Acc])
    end;
encode_chars([], _, Acc) ->
    lists:reverse(Acc).

encode_char(Char) ->
    <<Hi:4, Lo:4>> = <<Char:8>>,
    [$%, to_hex(Hi), to_hex(Lo)].

url_decode(Value) ->
    decode_chars(Value, []).

decode_chars([$%, Hi, Lo|Rest], Res) ->
    <<Char:8>> = <<(from_hex(Hi)):4, (from_hex(Lo)):4>>,
    decode_chars(Rest, [Char|Res]);
decode_chars([C|Rest], Res) ->
    decode_chars(Rest, [C|Res]);
decode_chars([], Res) ->
    lists:reverse(Res).

to_hex(10) -> $a;
to_hex(11) -> $b;
to_hex(12) -> $c;
to_hex(13) -> $d;
to_hex(14) -> $e;
to_hex(15) -> $f;
to_hex(N) when N < 10 -> $0 + N.

from_hex($a) -> 10;
from_hex($b) -> 11;
from_hex($c) -> 12;
from_hex($d) -> 13;
from_hex($e) -> 14;
from_hex($f) -> 15;
from_hex(N) -> N-$0.

safe_chars() ->
    lists:append([lists:seq($a, $z),
                  lists:seq($A, $Z),
                  lists:seq($0, $9),
                  [$., $-, $~, $_]]).
