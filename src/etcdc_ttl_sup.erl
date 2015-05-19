%%  Supervisor for etcdc_ttl jobs
%%
%% ----------------------------------------------------------------------------

-module(etcdc_ttl_sup).

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
    TTLWorker = child(etcdc_ttl, etcdc_ttl, worker, []),
    Strategy = {simple_one_for_one, 1, 60},
    {ok, {Strategy, [TTLWorker]}}.

%% ----------------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, transient, 3000, Type, [Mod]}.
