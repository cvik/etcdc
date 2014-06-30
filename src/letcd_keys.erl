-module(letcd_keys).

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
                 | {ttl, term()}.

-type del_opt() :: dir
                 | prevExist
                 | {prevValue, term()}
                 | {prevIndex, term()}.

%% ----------------------------------------------------------------------------

-spec get(string()) -> {ok, #{}} | {error, #{}}.
get(Path) ->
    get(Path, []).

-spec get(string(), [get_opt()]) -> {ok, #{}} | {error, #{}}.
get(Path, Opts) when is_binary(Path) ->
    get(binary_to_list(Path), Opts);
get(Path, Opts) ->
    FullPath = "/v2/keys" ++ letcd_lib:ensure_first_slash(Path),
    letcd_lib:call(get, etcd_client_port, FullPath, Opts).

-spec set(string(), iolist()) -> {ok, #{}} | {error, #{}}.
set(Path, Value) ->
    set(Path, Value, []).

-spec set(string(), iolist(), [set_opt()]) -> {ok, #{}} | {error, #{}}.
set(Path, Value, Opts) when is_binary(Path) ->
    set(binary_to_list(Path), Value, Opts);
set(Path, Value, Opts) ->
    FullPath = "/v2/keys" ++ letcd_lib:ensure_first_slash(Path),
    FullOpts = [{value, Value} | Opts],
    case proplists:get_bool(sequence, Opts) of
        true ->
            letcd_lib:call(post, etcd_client_port, FullPath, FullOpts);
        false ->
            letcd_lib:call(put, etcd_client_port, FullPath, FullOpts)
    end.

-spec del(string()) -> {ok, #{}} | {error, #{}}.
del(Path) ->
    del(Path, []).

-spec del(string(), [del_opt()]) -> {ok, #{}} | {error, #{}}.
del(Path, Opts) ->
    NewPath = "/v2/keys" ++ letcd_lib:ensure_first_slash(Path),
    letcd_lib:call(delete, etcd_client_port, NewPath, Opts).
