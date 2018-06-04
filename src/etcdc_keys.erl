-module(etcdc_keys).

-export([get/1, get/2, set/2, set/3, del/1, del/2]).

%% ----------------------------------------------------------------------------

-type get_opt() :: recursive
                 | consistent
                 | sorted
                 | stream
                 | wait
                 | {waitIndex, integer()}.

-type set_opt() :: dir
                 | prevExist
                 | sequence
                 | {prevValue, term()}
                 | {prevIndex, term()}
                 | {ttl, term()}
                 | {ttl_renew, term()}.

-type del_opt() :: dir
                 | prevExist
                 | {prevValue, term()}
                 | {prevIndex, term()}.

%% ----------------------------------------------------------------------------

-spec get(string()) -> {ok, map()} | {error, any()}.
get(Path) ->
    get(Path, []).

-spec get(string(), [get_opt()]) -> {ok, map()} | {error, any()}.
get(Path, Opts) when is_binary(Path) ->
    get(binary_to_list(Path), Opts);
get(Path, Opts) ->
    FullPath = "/v2/keys" ++ etcdc_lib:ensure_first_slash(Path),
    etcdc_lib:call(get, etcd_client_port, FullPath, Opts).

-spec set(string(), iolist()) -> {ok, map()} | {error, any()}.
set(Path, Value) ->
    set(Path, Value, []).

-spec set(string(), iolist(), [set_opt()]) -> {ok, map()} | {error, any()}.
set(Path, Value, Opts) when is_binary(Path) ->
    set(binary_to_list(Path), Value, Opts);
set(Path, Value, Opts) ->
    FullPath = "/v2/keys" ++ etcdc_lib:ensure_first_slash(Path),
    {FullOpts, Renew, TTL} = check_for_ttl([{value, Value} | Opts]),
    IsSequence = proplists:get_bool(sequence, FullOpts),
    Method = case IsSequence of true -> post; false -> put end,
    Res = etcdc_lib:call(Method, etcd_client_port, FullPath, FullOpts),
    case Renew of
        true ->
            etcdc_ttl:new(Path, TTL),
            Res;
        false ->
            Res
    end.

-spec del(string()) -> {ok, map()} | {error, any()}.
del(Path) ->
    del(Path, []).

-spec del(string(), [del_opt()]) -> {ok, map()} | {error, any()}.
del(Path, Opts) ->
    NewPath = "/v2/keys" ++ etcdc_lib:ensure_first_slash(Path),
    etcdc_lib:call(delete, etcd_client_port, NewPath, Opts).

%% Internal -------------------------------------------------------------------

check_for_ttl(Opts) ->
    case lists:keyfind(ttl_renew, 1, Opts) of
        {ttl_renew, TTL} ->
            {lists:keyreplace(ttl_renew, 1, Opts, {ttl, TTL}), true, TTL};
        false ->
            {Opts, false, infinity}
    end.
