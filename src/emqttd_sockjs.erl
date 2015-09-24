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
%%% emqttd sockjs
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emqttd_sockjs).

-export([load/1]).

%% cowboy http callback
-export([init/3, handle/2, terminate/3]).

%% sockjs callback
-export([service_stomp/3]).

-record(stomp_state, {stomp_pid, stomp_opts}).

load(Env) ->

    SockjsOpts = proplists:get_value(sockjs, Env, []),

    StompOpts  = proplists:get_value(stomp, Env, []),

    StompState = #stomp_state{stomp_opts = StompOpts},

    SockjsState = sockjs_handler:init_state(
                        <<"/stomp">>, fun service_stomp/3, StompState, SockjsOpts),

    VhostRoutes = [{<<"/stomp/[...]">>, sockjs_cowboy_handler, SockjsState},
                   {'_', ?MODULE, []}],

    Routes = [{'_',  VhostRoutes}], % any vhost

    Dispatch = cowboy_router:compile(Routes),

    {Listener, Port, Acceptors} = proplists:get_value(cowboy_listener, Env),

    lager:info("SockJS URL: http://127.0.0.1:~w/stomp", [Port]),

    cowboy:start_http(Listener, Acceptors, [{port, Port}],
                        [{env, [{dispatch, Dispatch}]}]).

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    IndexHtml = filename:join([docroot(), "index.html"]),
    {ok, Data} = file:read_file(IndexHtml),
    {ok, Req1} = cowboy_req:reply(200, [{<<"Content-Type">>, "text/html"}], Data, Req),
    {ok, Req1, State}.

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

terminate(_Reason, _Req, _State) ->
        ok.

%%------------------------------------------------------------------------------
%% SockJS Callback
%%------------------------------------------------------------------------------

service_stomp(Conn, init, State = #stomp_state{stomp_opts = Opts}) -> 
    {ok, Pid} = emqttd_sockjs_stomp_sup:start_stomp(Conn, Opts),
    {ok, State#stomp_state{stomp_pid = Pid}};

service_stomp(_Conn, {recv, Data}, State = #stomp_state{stomp_pid = Pid}) ->
    lager:info("SockJS Recv: ~p", [Data]),
    emqttd_sockjs_stomp:recv(Pid, Data),
    {ok, State};

service_stomp(_Conn, {info, Info}, State) ->
    lager:info("SockJS Info: ~p", [Info]),
    {ok, State};

service_stomp(Conn, closed, #stomp_state{stomp_pid = Pid}) ->
    lager:info("SockJS Closed:~p~n", [Conn]),
    emqttd_sockjs_stomp:close(Pid), ok.

