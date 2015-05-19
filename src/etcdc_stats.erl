-module(etcdc_stats).

-export([leader/0, self/0, store/0]).

%% ----------------------------------------------------------------------------

-spec leader() -> {ok, #{}} | {error, #{}}.
leader() ->
    etcdc_lib:call(get, etcd_client_port, "/v2/stats/leader", []).

-spec self() -> {ok, #{}} | {error, #{}}.
self() ->
    etcdc_lib:call(get, etcd_client_port, "/v2/stats/self", []).

-spec store() -> {ok, #{}} | {error, #{}}.
store() ->
    etcdc_lib:call(get, etcd_client_port, "/v2/stats/store", []).
