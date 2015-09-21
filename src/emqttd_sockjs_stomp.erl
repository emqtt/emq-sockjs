%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emqttd stomp over sockjs
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emqttd_sockjs_stomp).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, recv/2, close/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sockjs_conn, parser, proto_env, proto_state}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Conn) ->
    gen_server:start_link(?MODULE, [Conn], []).

recv(Pid, Data) ->
    gen_server:cast(Pid, {recv, Data}).

close(Pid) ->
    gen_server:call(Pid, close).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Conn) ->
    io:format("Conn: ~p~n", [Conn]),
    SendFun = fun(Data) -> Conn:send(Data) end,
    %%TODO...
    ProtoState = emqttd_stomp_proto:init({{127,0,0,1}, 29292}, SendFun, []),
    {ok, #state{sockjs_conn = Conn,
                parser = emqttd_stomp_frame:parser([]),
                proto_state = ProtoState}}.

handle_call(close, _From, State) ->
    {stop, {shutdown, sockjs_closed}, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({recv, Data}, State) ->
    received(Data, State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
received(<<>>, State) ->
    noreply(State);

received(Data, State = #state{parser      = Parser,
                              proto_state = ProtoState}) ->
    case Parser(Data) of
        {more, NewParser} ->
            noreply(State#state{parser = NewParser});
        {ok, Frame, Rest} ->
            lager:info("RECV Frame: ~p", [Frame]),
            case emqttd_stomp_proto:received(Frame, ProtoState) of
                {ok, ProtoState1}           ->
                    received(Rest, reset_parser(State#state{proto_state = ProtoState1}));
                {error, Error, ProtoState1} ->
                    stop({shutdown, Error}, State#state{proto_state = ProtoState1});
                {stop, Reason, ProtoState1} ->
                    stop(Reason, State#state{proto_state = ProtoState1})
            end;
        {error, Error} ->
            io:format("~p~n", [Error]),
            stop({shutdown, frame_error}, State)
    end.

reset_parser(State = #state{proto_env = ProtoEnv}) ->
    State#state{parser = emqttd_stomp_frame:parser(ProtoEnv)}.

noreply(State) ->
    {noreply, State, hibernate}.

stop(Reason, State) ->
    {stop, Reason, State}.

