-module(etcdc_admin).

-export([get_config/0, set_config/3,
         list_machines/0, del_machine/1]).

%% ----------------------------------------------------------------------------

-spec get_config() -> {ok, #{}} | {error, #{}}.
get_config() ->
    etcdc_lib:call(get, etcd_admin_port, "/v2/admin/config", []).

-spec set_config(integer(), integer(), integer()) -> {ok, #{}} | {error, #{}}.
set_config(ActiveSize, RemoveDelay, SyncInterval) ->
    M = #{<<"activeSize">> => ActiveSize,
          <<"removeDelay">> => RemoveDelay,
          <<"syncInterval">> => SyncInterval},
    Value = lejson:encode(M),
    io:format("payload: ~s~n", [Value]),
    etcdc_lib:call(put, etcd_admin_port, "/v2/admin/config", [], Value).

-spec list_machines() -> {ok, #{}} | {error, #{}}.
list_machines() ->
    etcdc_lib:call(get, etcd_admin_port, "/v2/admin/machines", []).

-spec del_machine(iolist()) -> {ok, #{}} | {error, #{}}.
del_machine(MachineName) ->
    Path = "v2/admin/machines/" ++ MachineName,
    etcdc_lib:call(delete, etcd_admin_port, Path, []).
