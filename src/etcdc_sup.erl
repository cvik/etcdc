%%  Top supervisor for etcdc
%%
%% ----------------------------------------------------------------------------

-module(etcdc_sup).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([start_link/0]).

-export([init/1]).

-behaviour(supervisor).

%% ----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% ----------------------------------------------------------------------------

init(no_arg) ->
    WatchSup = child(etcdc_watch_sup, etcdc_watch_sup, supervisor, []),
    StreamSup = child(etcdc_stream_sup, etcdc_stream_sup, supervisor, []),
    TTLSup = child(etcdc_ttl_sup, etcdc_ttl_sup, supervisor, []),
    Strategy = {one_for_one, 1, 10},
    {ok, {Strategy, [WatchSup, StreamSup, TTLSup]}}.

%% ----------------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, permanent, 3000, Type, [Mod]}.
