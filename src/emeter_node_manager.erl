%%% ------------------------------------------------------------------------------------------------
%%% "Emeter" is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version
%% @doc
%%
%% @end
%% -------------------------------------------------------------------------------------------------
-module(emeter_node_manager).
-author("pouriya.jahanbakhsh@gmail.com").
-behavior(gen_server).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/1
        ,connect/1]).

%% 'gen_server' callbacks:
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(DEF_RECONNECT_INTERVAL, 3000).
-define(TAB, ?MODULE).

-define(NOTIFY_TAG, notify).
-define(NODE_UP_TAG, nodeup).
-define(S, state).
-record(?S, {node, connection}).

-define(CONNECT_TAG, connect).

-include("emeter_log.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

start_link(Node) ->
    gen_server:start_link(?MODULE, Node, []).


connect(Pid) ->
    gen_server:call(Pid, ?CONNECT_TAG, infinity).

%% -------------------------------------------------------------------------------------------------
%% 'gen_server' callbacks:

init(Node) ->
    erlang:process_flag(trap_exit, true),
    notify(0),
    {ok, #?S{node = Node, connection = false}}.


handle_call(?CONNECT_TAG, _, #?S{node = Node}=S) ->
    ?info(<<"'connect' request received, Try connecting to ~tp">>, [Node]),
    case do_connect(Node) of
        ok ->
            {reply, ok, S};
        retry ->
            {reply, {error, retry}, S};
        {error, Rsn}=Reply ->
            {stop, Rsn, Reply, S}
    end;
handle_call(Req, _, S) ->
    {reply, {error, {unknown, [{request, Req}]}}, S}.


handle_cast(_, S) ->
    {noreply, S}.


handle_info(?NODE_UP_TAG, #?S{node = Node}=S) ->
    _ = emeter_node:add(Node),
    ?info(<<"New node ~tp added to nodes">>, [Node]),
    {noreply, S#?S{connection = true}};

handle_info({nodedown, _}, #?S{node = Node}=S) ->
    _ = emeter_node:delete(Node),
    ?error(<<"Node ~tp disconnected">>, [Node]),
    notify(?DEF_RECONNECT_INTERVAL),
    {noreply, S#?S{connection = false}};

handle_info(notify, #?S{node = Node}=S) ->
    ?info(<<"Try connecting to ~tp">>, [Node]),
    case do_connect(Node) of
        Ok when Ok == ok orelse Ok == retry ->
            {noreply, S};
        {error, Rsn} ->
            {stop, Rsn, S}
    end;

handle_info(_, S) ->
    {noreply, S}.


terminate(_, #?S{node = Node}) ->
    _ = emeter_node:delete(Node),
    ok.

code_change(_, S, _) ->
    {ok, S}.

%% -------------------------------------------------------------------------------------------------
%% Internals:

notify(0) ->
    erlang:self() ! ?NOTIFY_TAG,
    ok;
notify(Int) ->
    _ = erlang:send_after(Int, erlang:self(), ?NOTIFY_TAG),
    ok.


do_connect(Node) ->
    case net_adm:ping(Node) of
        pong ->
            try erlang:monitor_node(Node, true) of
                _ ->
                    %% Load 'emeter_agent' on remote node
                    {Mod, Bin, File} = code:get_object_code(emeter_agent_api),
                    case rpc:call(Node, code, load_binary, [Mod, File, Bin]) of
                        {module, _} ->
                            %% I should start 'emeter_agent' process too
                            case emeter_agent_api:start(Node) of
                                {ok, _} ->
                                    erlang:self() ! ?NODE_UP_TAG,
                                    ok;
                                {error, {already_started, _}} ->
                                    erlang:self() ! ?NODE_UP_TAG,
                                    ok;
                                {error, Rsn} ->
                                    ?error(<<"Could not start 'emeter_agent' process on remote node"
                                             " ~tp for reason ~tp. Stop reconnecting.">>
                                          ,[Node, Rsn]),
                                    %% Director will delete this child from its children
                                    {error, load_agent}
                            end;
                        {badrpc, Rsn} ->
                            ?error(<<"Could not load EMeter module on node ~tp for reason ~tp. Retr"
                                     "y after ~tp ms.">>
                                  ,[Node, Rsn, ?DEF_RECONNECT_INTERVAL]),
                            notify(?DEF_RECONNECT_INTERVAL),
                            retry;
                        {error, Rsn} -> % {error, _}
                            ?error(<<"Could not load EMeter agent on node ~tp for reason ~tp. Stop "
                                     "reconnecting.">>
                                  ,[Node, Rsn, ?DEF_RECONNECT_INTERVAL]),
                            {error, load_agent}
                    end
            catch
                _:_ ->
                    ?error(<<"Could not monitor node ~tp. Retry after ~tp ms.">>
                          ,[Node, ?DEF_RECONNECT_INTERVAL]),
                    notify(?DEF_RECONNECT_INTERVAL),
                    retry
            end;
        _ -> % pang
            ?error(<<"Could not connect to node ~tp. Retry after ~tp ms">>
                  ,[Node, ?DEF_RECONNECT_INTERVAL]),
            notify(?DEF_RECONNECT_INTERVAL),
            retry
    end.