%%  Service that handle automatic renewal of a node
%%
%%  This service will link itself to the controlling process and will
%%  shut itself down if the controlling process dies.
%%
%% ----------------------------------------------------------------------------

-module(etcdc_ttl).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

-export([start_link/3, new/2]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {key, ttl, ctrl}).

%% Management Api -------------------------------------------------------------

start_link(Key, TTL, Ctrl) ->
    gen_server:start_link(?MODULE, [Key, TTL, Ctrl], []).

new(Key, TTL) ->
    etcdc_ttl_sup:add_child([Key, TTL, self()]).

%% gen_server callbacks -------------------------------------------------------

init([Key, TTL, Ctrl]) ->
    process_flag(trap_exit, true),
    link(Ctrl),
    {ok, _} = timer:send_after(round(TTL * 1000 * 0.2), renew_ttl),
    {ok, #state{key=Key, ttl=TTL, ctrl=Ctrl}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(renew_ttl, #state{key=Key, ttl=TTL} = State) ->
    case etcdc:set(Key, [<<>>], [{ttl, TTL}, prevExist]) of
        {error, _} ->
            {stop, normal, State};
        {ok, #{}} ->
            {ok, _} = timer:send_after(round(TTL * 1000 * 0.2), renew_ttl),
            {noreply, State}
    end;
handle_info({'EXIT', Ctrl, _}, #state{ctrl=Ctrl} = State) ->
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    State.

%% Internal -------------------------------------------------------------------
