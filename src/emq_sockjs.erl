%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
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
%%% @doc emqttd sockjs
%%%
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------
-module(emq_sockjs).

-export([load/1]).

%% sockjs callback
-export([service_stomp/3]).

-record(stomp_state, {stomp_pid, stomp_opts}).

-define(INFO(Format, Args), lager:info("SockJS: " ++ Format, Args)).

load(Env) ->

    SockjsOpts = proplists:get_value(sockjs, Env, []),

    StompOpts  = proplists:get_value(stomp, Env, []),

    StompState = #stomp_state{stomp_opts = StompOpts},

    SockjsState = sockjs_handler:init_state(
                        <<"/stomp">>, fun service_stomp/3, StompState, SockjsOpts),

    VhostRoutes = [{<<"/stomp/[...]">>, sockjs_cowboy_handler, SockjsState},
                   {"/[...]", cowboy_static, [
                        {directory, {priv_dir, ?MODULE, "www"}},
                        {mimetypes, [
                            {<<".ico">>, [<<"image/x-icon">>]},
                            {<<".html">>, [<<"text/html">>]},
                            {<<".css">>, [<<"text/css">>]},
                            {<<".js">>, [<<"application/javascript">>]}]}
                   ]}],

    Routes = [{'_',  VhostRoutes}], % any vhost

    Dispatch = cowboy_router:compile(Routes),

    {Listener, Port, Acceptors} = proplists:get_value(cowboy_listener, Env),

    io:format("SockJS URL: http://127.0.0.1:~w/stomp~n", [Port]),

    cowboy:start_http(Listener, Acceptors, [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]).

%%------------------------------------------------------------------------------
%% SockJS Callback
%%------------------------------------------------------------------------------

service_stomp(Conn, init, State = #stomp_state{stomp_opts = Opts}) -> 
    {ok, Pid} = emq_sockjs_stomp_sup:start_stomp(Conn, Opts),
    {ok, State#stomp_state{stomp_pid = Pid}};

service_stomp(_Conn, {recv, Data}, State = #stomp_state{stomp_pid = Pid}) ->
    ?INFO("RECV ~p", [Data]),
    emq_sockjs_stomp:recv(Pid, Data),
    {ok, State};

service_stomp(_Conn, {info, Info}, State) ->
    ?INFO("Info ~p", [Info]),
    {ok, State};

service_stomp(Conn, closed, #stomp_state{stomp_pid = Pid}) ->
    ?INFO("Closed ~p", [Conn]),
    emq_sockjs_stomp:close(Pid), ok.

