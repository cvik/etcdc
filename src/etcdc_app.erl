%%  Application callback for etcdc
%%
%% ----------------------------------------------------------------------------

-module(etcdc_app).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([start/2, prep_stop/1, stop/1]).

-behaviour(application).

%% ----------------------------------------------------------------------------

start(_, _) ->
    etcdc_sup:start_link().

prep_stop(State) ->
    State.

stop(_) ->
    ok.

%% ----------------------------------------------------------------------------
