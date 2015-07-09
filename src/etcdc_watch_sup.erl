%%  Supervisor for etcdc_stream jobs
%%
%% ----------------------------------------------------------------------------

-module(etcdc_watch_sup).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([start_link/0, add_child/1]).

-export([init/1]).

-behaviour(supervisor).

%% ----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

add_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%% ----------------------------------------------------------------------------

init(no_arg) ->
    StreamWorker = child(etcdc_watch, etcdc_watch, worker, []),
    Strategy = {simple_one_for_one, 1, 60},
    {ok, {Strategy, [StreamWorker]}}.

%% ----------------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, transient, 3000, Type, [Mod]}.
