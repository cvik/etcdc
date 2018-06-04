-module(etcdc).

-export([start/0, stop/0]).
-export([get/1, get/2, set/2, set/3, del/1, del/2]).
-export([watch/1, watch/2, cancel_watch/1]).
-export([stats_leader/0, stats_self/0, stats_store/0]).
-export([get_config/0, set_config/3, list_machines/0, del_machine/1]).
-export([leader/0, peers/0]).

%% Types ----------------------------------------------------------------------

-type get_opt() :: recursive
                 | consistent
                 | sorted
                 | stream
                 | wait
                 | {waitIndex, integer()}.
-type set_opt() :: dir
                 | prevExist
                 | sequence
                 | {ttl, term()}
                 | {ttl_renew, term()}
                 | {prevIndex, term()}
                 | {prevValue, term()}.

-type del_opt() :: dir
                 | prevExist
                 | {prevIndex, term()}
                 | {prevValue, term()}.

%% Management -----------------------------------------------------------------

%% @doc Start the etcdc application and it's dependencies
-spec start() -> ok | {error, any()}.
start() ->
    application:ensure_all_started(?MODULE).

%% @doc Stop the etcdc application
-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

%% Keys -----------------------------------------------------------------------

-spec get(string()) -> {ok, map()} | {error, any()}.
get(Path) ->
    etcdc_keys:get(Path).

-spec get(string(), [get_opt()]) -> {ok, map()} | {error, any()}.
get(Path, Opts) ->
    case verify_opts(get, Opts) of
        true ->
            etcdc_keys:get(Path, Opts);
        false ->
            {error, bad_arg}
    end.

-spec set(string(), iodata()) -> {ok, map()} | {error, any()}.
set(Path, Value) ->
    etcdc_keys:set(Path, Value).

-spec set(string(), iodata(), [set_opt()]) -> {ok, map()} | {error, any()}.
set(Path, Value, Opts) ->
    case verify_opts(set, Opts) of
        true ->
            etcdc_keys:set(Path, Value, Opts);
        false ->
            {error, bad_arg}
    end.

-spec del(string()) -> {ok, map()} | {error, any()}.
del(Key) ->
    etcdc_keys:del(Key).

-spec del(string(), [del_opt()]) -> {ok, map()} | {error, any()}.
del(Key, Opts) ->
    case verify_opts(del, Opts) of
        true ->
            etcdc_keys:del(Key, Opts);
        false ->
            {error, bad_arg}
    end.

%% Watch ----------------------------------------------------------------------

-spec watch(Key :: string(), Recursive :: boolean()) ->
        {ok, Pid :: pid()}
      | {error, Reason :: term()}.
watch(Key) ->
    etcdc_watch:new(Key, []).

watch(Key, Opts) ->
    etcdc_watch:new(Key, Opts).

-spec cancel_watch(Pid :: pid()) -> ok.
cancel_watch(Pid) ->
    supervisor:terminate_child(etcdc_stream_sup, Pid).

%% Stats ----------------------------------------------------------------------

-spec stats_leader() -> {ok, map()} | {error, any()}.
stats_leader() ->
    etcdc_stats:leader().

-spec stats_self() -> {ok, map()} | {error, any()}.
stats_self() ->
    etcdc_stats:self().

-spec stats_store() -> {ok, map()} | {error, any()}.
stats_store() ->
    etcdc_stats:store().

%% Admin ----------------------------------------------------------------------

-spec get_config() -> {ok, map()} | {error, any()}.
get_config() ->
    etcdc_admin:get_config().

-spec set_config(integer(), integer(), integer()) -> {ok, map()} |
                                                     {error, any()}.
set_config(ActiveSize, RemoveDelay, SyncInterval) ->
    etcdc_admin:set_config(ActiveSize, RemoveDelay, SyncInterval).

-spec list_machines() -> {ok, map()} | {error, any()}.
list_machines() ->
    etcdc_admin:list_machines().

-spec del_machine(MachineId :: string()) -> {ok, <<>>} | {error, any()}.
del_machine(MachineId) ->
    etcdc_admin:del_machine(MachineId).

-spec leader() -> {ok, string()} | {error, any()}.
leader() ->
    etcdc_lib:call(get, etcd_client_port, "/v2/leader", []).

-spec peers() -> {ok, string()} | {error, any()}.
peers() ->
    etcdc_lib:call(get, etcd_client_port, "/v2/peers", []).

%% Internal -------------------------------------------------------------------

verify_opts(Cmd, Opts) ->
    Allowed = get_allowed_opts(Cmd),
    F = fun(Key) -> lists:member(Key, Allowed) end,
    lists:all(F, proplists:get_keys(Opts)).

get_allowed_opts(get) ->
    [recursive, consistent, sorted, stream, wait, waitIndex];
get_allowed_opts(set) ->
    [dir, prevExist, sequence, prevValue, prevIndex, ttl, ttl_renew];
get_allowed_opts(del) ->
    [recursive, sorted, stream, wait, waitIndex].
