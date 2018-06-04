%%  Service that handles watch events from etcd
%%
%% ----------------------------------------------------------------------------

-module(etcdc_watch).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

-export([start_link/3, new/2]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

%% Management Api -------------------------------------------------------------

start_link(Key, Ctrl, Opts) ->
    gen_server:start_link(?MODULE, [Key, Ctrl, Opts], []).

new(Key, Opts) ->
    etcdc_watch_sup:add_child([Key, self(), Opts]).

%% gen_server callbacks -------------------------------------------------------

init([Key, Ctrl, Opts]) ->
    MonRef = erlang:monitor(process, Ctrl),
    gen_server:cast(self(), check_watch),
    {ok, #{ctrl=>Ctrl, key=>Key, index=>0, mon_ref=>MonRef, opts=>Opts}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(check_watch, State) ->
    #{key:=Key, ctrl:=Ctrl, index:=Index, opts:=Opts} = State,
    case etcdc:get(Key, [wait, {waitIndex, Index}|Opts]--[continous]) of
        {error, Error} ->
            Ctrl ! {watch_error, self(), Error},
            {stop, normal, State};
        {ok, #{node:=#{modified_index:=NewIndex}}} = Response ->
            Ctrl ! {watch, self(), Response},
            case proplists:get_bool(continous, Opts) of
                true ->
                    gen_server:cast(self(), check_watch),
                    {noreply, State#{index:=NewIndex+1}};
                false ->
                    {stop, normal, State}
            end
    end;
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, _, _, _}, #{mon_ref:=MonRef} = State) ->
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    State.

%% Internal -------------------------------------------------------------------
