-module(letcd).

-export([start/0, stop/0]).
-export([get/1, get/2, set/2, set/3, del/1, del/2]).
-export([watch/2, watch_continous/1, stop_watch/1]).
-export([stats_leader/0, stats_self/0, stats_store/0]).
-export([get_config/0, set_config/3, list_machines/0, del_machine/1]).
-export([leader/0, peers/0]).

%% Management -----------------------------------------------------------------

start() ->
    Deps = [crypto, asn1, public_key, ssl, lhttpc, lejson],
    [ application:start(A) || A <- Deps ],
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% Keys -----------------------------------------------------------------------

get(Path) ->
    letcd_keys:get(Path).

get(Path, Opts) ->
    case verify_opts(get, Opts) of
        true ->
            letcd_keys:get(Path, Opts);
        false ->
            {error, bad_arg}
    end.

set(Path, Value) ->
    letcd_keys:set(Path, Value).

set(Path, Value, Opts) ->
    case verify_opts(set, Opts) of
        true ->
            letcd_keys:set(Path, Value, Opts);
        false ->
            {error, bad_arg}
    end.

del(Path) ->
    letcd_keys:del(Path).

del(Path, Opts) ->
    case verify_opts(set, Opts) of
        true ->
            letcd_keys:del(Path, Opts);
        false ->
            {error, bad_arg}
    end.

%% Watch ----------------------------------------------------------------------

watch(Key, false) ->
    letcd_watch:new(Key, []);
watch(Key, true) ->
    letcd_watch:new(Key, [recursive]).

%% TODO: Add recursive streaming watches
watch_continous(Key) ->
    letcd_stream:new(Key).

stop_watch(Pid) ->
    letcd_stream:stop(Pid).

%% Stats ----------------------------------------------------------------------

stats_leader() ->
    letcd_stats:leader().

stats_self() ->
    letcd_stats:self().

stats_store() ->
    letcd_stats:store().

%% Admin ----------------------------------------------------------------------

get_config() ->
    letcd_admin:get_config().

set_config(ActiveSize, RemoveDelay, SyncInterval) ->
    letcd_admin:set_config(ActiveSize, RemoveDelay, SyncInterval).

list_machines() ->
    letcd_admin:list_machines().

del_machine(MachineId) ->
    letcd_admin:del_machine(MachineId).

leader() ->
    letcd_lib:call(get, etcd_client_port, "/v2/leader", []).

peers() ->
    letcd_lib:call(get, etcd_client_port, "/v2/peers", []).

%% Internal -------------------------------------------------------------------

verify_opts(Cmd, Opts) ->
    Allowed = get_allowed_opts(Cmd),
    F = fun(Key) -> lists:member(Key, Allowed) end,
    lists:all(F, proplists:get_keys(Opts)).

get_allowed_opts(get) ->
    [recursive, consistent, sorted, stream, wait, waitIndex];
get_allowed_opts(set) ->
    [dir, prevExist, sequence, prevValue, prevIndex, ttl];
get_allowed_opts(del) ->
    [recursive, sorted, stream, wait, waitIndex];
get_allowed_opts(_) ->
    [].
