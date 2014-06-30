%%  Service that handle a single asynchronous event from etcd
%%
%% ----------------------------------------------------------------------------

-module(letcd_watch).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

-export([start_link/3, new/2]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {key, ctrl, opts}).

%% Management Api -------------------------------------------------------------

start_link(Key, Ctrl, Opts) ->
    gen_server:start_link(?MODULE, [Key, Ctrl, Opts], []).

new(Key, Opts) ->
    letcd_watch_sup:add_child([Key, self(), Opts]).

%% gen_server callbacks -------------------------------------------------------

init([Key, Ctrl, Opts]) ->
    {ok, #state{key=Key, ctrl=Ctrl, opts=Opts}, 0}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(timeout, #state{key=Key, ctrl=Ctrl, opts=Opts} = State) ->
    case letcd:get(Key, [wait|Opts--[wait]]) of
        {ok, Response} ->
            Ctrl ! {watch, self(), Response},
            {stop, normal, State};
        {error, Error} ->
            Ctrl ! {watch_error, self(), Error},
            {stop, normal, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    State.

%% Internal -------------------------------------------------------------------
