-module(letcd_stats).

-export([leader/0, self/0, store/0]).

%% ----------------------------------------------------------------------------

-spec leader() -> {ok, #{}} | {error, #{}}.
leader() ->
    letcd_lib:call(get, etcd_client_port, "/v2/stats/leader", []).

-spec self() -> {ok, #{}} | {error, #{}}.
self() ->
    letcd_lib:call(get, etcd_client_port, "/v2/stats/self", []).

-spec store() -> {ok, #{}} | {error, #{}}.
store() ->
    letcd_lib:call(get, etcd_client_port, "/v2/stats/store", []).
