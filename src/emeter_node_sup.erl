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
-module(emeter_node_sup).
-author("pouriya.jahanbakhsh@gmail.com").
-behavior(director).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/0
        ,connect/1
        ,add/1
        ,delete/1]).

%% 'director' callbacks:
-export([init/1
        ,handle_start/4
        ,handle_exit/5
        ,handle_terminate/5
        ,terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(PROC, ?MODULE).
-define(TAB, ?MODULE).

%% -------------------------------------------------------------------------------------------------
%% API:

start_link() ->
    director:start_link({local, ?PROC}, ?MODULE, undefined).


add(Node) when erlang:is_atom(Node) ->
    case director:start_child(?PROC
                             ,#{id => Node, start => {emeter_node_manager, start_link, [Node]}}) of
        {ok, _} ->
            ok;
        Err ->
            Err
    end.


delete(Node) when erlang:is_atom(Node) ->
    director:terminate_and_delete_child(?PROC, Node).


connect(Node) ->
    case director_table_ets:get_pid(?TAB, Node) of
        {ok, Pid} ->
            emeter_node_manager:connect(Pid);
        Err ->
            Err
    end.

%% -------------------------------------------------------------------------------------------------
%% 'director' callbacks:

init(_) -> % (undefined)
    ok = emeter_node:create_table(),
    Children = [#{id => Node, start => {emeter_node_manager, start_link, [Node]}}
               || Node <- emeter:value(nodes)],
    {ok, undefined, Children, [{db, [{table, ets}, {init_arg, ?TAB}]}]}.


handle_start(_, Chs, S, _) ->
    {ok, Chs, S, []}.


handle_exit(_, Chs, Rsn, S, _) ->
    Action =
        case Rsn of
            load_agent ->
                delete;
            _ ->
                stop
        end,
    {Action, Chs, S, []}.


handle_terminate(_, Chs, _, S, _) ->
    {ok, Chs, S, []}.


terminate(_, _) ->
    _ = emeter_node:delete_table(),
    ok.