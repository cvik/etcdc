%%  Service that handle an asynchronous stream of events from etcd
%%
%% ----------------------------------------------------------------------------

-module(etcdc_stream).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

-export([start_link/3, new/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

%% Management Api -------------------------------------------------------------

start_link(Key, Ctrl, Recursive) ->
    gen_server:start_link(?MODULE, [Key, Ctrl, Recursive], []).

new(Key, Recursive) ->
    etcdc_stream_sup:add_child([Key, self(), Recursive]).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% gen_server callbacks -------------------------------------------------------

init([Key, Ctrl, Recursive]) ->
    MonRef = erlang:monitor(process, Ctrl),
    gen_server:cast(self(), check_watch),
    Opts = case Recursive of true -> [recursive]; false -> [] end,
    {ok, #{ctrl=>Ctrl, key=>Key, index=>0, mon_ref=>MonRef, opts=>Opts}}.

handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(check_watch, State) ->
    #{key:=Key, ctrl:=Ctrl, index:=Index, opts:=Opts} = State,
    case etcdc:get(Key, [wait, {waitIndex, Index}|Opts]) of
        {error, Error} ->
            Ctrl ! {watch_error, self(), Error},
            {stop, normal, State};
        #{node:=#{modified_index:=NewIndex}} = Response ->
            Ctrl ! {watch, self(), Response},
            gen_server:cast(self(), check_watch),
            {noreply, State#{index:=NewIndex+1}}
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
