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
-module(emeter_node).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([create_table/0
        ,delete_table/0
        ,add/1
        ,delete/1
        ,all/0
        ,default/0
        ,is_node_exists/1
        ,filter_config/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(TAB, emeter_node_table).
-define(NODE, node).
-record(?NODE, {node}).

-define(is_node(Node), (erlang:is_atom(Node) andalso Node =/= erlang:node())).

%% -------------------------------------------------------------------------------------------------
%% API:

create_table() ->
    _ = ets:new(?TAB, [named_table, public, {keypos, 2}]),
    _ = ets:insert(?TAB, #?NODE{node = erlang:node()}),
    ok.


delete_table() ->
    _ = ets:delete(?TAB),
    ok.


add(Node) when ?is_node(Node) ->
    _ = ets:insert(?TAB, #?NODE{node = Node}),
    ok.


delete(Node) when ?is_node(Node) ->
    _ = ets:delete(?TAB, Node),
    ok.


%% @todo use ets:select
all() ->
    FoldFun =
        fun(#?NODE{node = Node}, Nodes) ->
            [Node|Nodes]
        end,
    ets:foldl(FoldFun, [], ?TAB).


default() ->
    erlang:node().


is_node_exists(Node) when erlang:is_atom(Node) ->
    case ets:lookup(?TAB, Node) of
        [_] ->
            true;
        _ ->
            case emeter_node_sup:connect(Node) of
                ok ->
                    true;
                _ ->
                    false
            end
    end.


filter_config(Nodes) when erlang:is_list(Nodes) ->
    lists:all(fun erlang:is_atom/1, Nodes);
filter_config(_) ->
    false.