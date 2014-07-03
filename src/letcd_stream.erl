%%  Service that handle an asynchronous stream of events from etcd
%%
%% ----------------------------------------------------------------------------

-module(letcd_stream).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

-export([start_link/3, new/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {sock, ctrl, key, parts}).

%% Management Api -------------------------------------------------------------

start_link(Key, Ctrl, Recursive) ->
    gen_server:start_link(?MODULE, [Key, Ctrl, Recursive], []).

new(Key, Recursive) ->
    letcd_stream_sup:add_child([Key, self(), Recursive]).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% gen_server callbacks -------------------------------------------------------

init([Key, Ctrl, Recursive]) ->
    Qs = "?wait=true&stream=true",
    RecQs = case Recursive of true -> "&recursive=true"; false -> "" end,
    FullKey = "/v2/keys" ++ letcd_lib:ensure_first_slash(Key) ++ Qs ++ RecQs,
    {ok, _, Sock} = letcd_lib:async_connect(FullKey, 5000),
    ok = inet:setopts(Sock, [{active, once}]),
    {ok, #state{sock=Sock, ctrl=Ctrl, key=Key, parts=[]}, 0}.

handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({tcp_closed, Sock}, #state{sock=Sock, ctrl=Ctrl} = State) ->
    Ctrl ! {self(), closed},
    {stop, normal, State};
handle_info({tcp, Sock, Payload}, #state{sock=Sock} = State) ->
    #state{ctrl=Ctrl, key=Key, parts=Parts} = State,
    Chunks = binary:split(Payload, <<"\r\n">>, [global]),
    case lists:last(Chunks) of
        <<>> ->
            case Ctrl ! process_chunks(lists:reverse([Payload|Parts])) of
                {watch, _, #{action := <<"delete">>, key := Key}} ->
                    {stop, normal, State};
                {watch, _, _} ->
                    inet:setopts(Sock, [{active, once}]),
                    {noreply, State#state{parts=[]}};
                {watch_error, _, Error} ->
                    {stop, Error, State}
            end;
        _ ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, State#state{parts=[Payload|Parts]}}
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    State.

%% Internal -------------------------------------------------------------------

process_chunks(Chunks) ->
    case binary:split(list_to_binary(Chunks), <<"\r\n">>, [global]) of
        [_Len, Body, <<>>] ->
            {watch, self(), letcd_lib:parse_response(Body)};
        [_Len, Body, <<"0">>, <<>>, <<>>] ->
            {watch, self(), letcd_lib:parse_response(Body)};
        Other ->
            {watch_error, self(), Other}
    end.
