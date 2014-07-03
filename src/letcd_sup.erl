%%  Top supervisor for letcd
%%
%% ----------------------------------------------------------------------------

-module(letcd_sup).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([start_link/0]).

-export([init/1]).

-behaviour(supervisor).

%% ----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% ----------------------------------------------------------------------------

init(no_arg) ->
    WatchSup = child(letcd_watch_sup, letcd_watch_sup, supervisor, []),
    StreamSup = child(letcd_stream_sup, letcd_stream_sup, supervisor, []),
    TTLSup = child(letcd_ttl_sup, letcd_ttl_sup, supervisor, []),
    Strategy = {one_for_one, 1, 10},
    {ok, {Strategy, [WatchSup, StreamSup, TTLSup]}}.

%% ----------------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, permanent, 3000, Type, [Mod]}.
