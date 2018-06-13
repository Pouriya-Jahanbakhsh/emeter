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
-module(emeter_plugin_sup).
-author("pouriya.jahanbakhsh@gmail.com").
-behavior(director).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/0
        ,start/4
        ,stop/1]).

%% 'director' callbacks:
-export([init/1
        ,handle_start/4
        ,handle_exit/5
        ,handle_terminate/5
        ,terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(PROC, ?MODULE).
-define(DEF_TERMINATE_TIMEOUT, 10000).
-define(DEF_RESTART_INTERVAL, 3000).

%% -------------------------------------------------------------------------------------------------
%% API:

start_link() ->
    director:start_link({local, ?PROC}, ?MODULE, undefined).


start(Id, Mod, Func, Args) ->
    director:start_child(?PROC, #{id => Id
                                 ,start => {Mod, Func, Args}
                                 ,terminate_timeout => ?DEF_TERMINATE_TIMEOUT}).

stop(Id) ->
    director:terminate_and_delete_child(?PROC, Id).

%% -------------------------------------------------------------------------------------------------
%% 'director' callbacks:


init(_) -> % (undefined)
    {ok, undefined, []}.


handle_start(_, Chs, S, _) ->
    {ok, Chs, S, []}.


handle_exit(_, Chs, _, S, #{restart_count := RestartCount}) ->
    Action =
        if
            RestartCount < 6 ->
                restart;
            RestartCount < 11 ->
                {restart, ?DEF_RESTART_INTERVAL};
            true ->
                wait
        end,
    {Action, Chs, S, [{log, true}]}.


handle_terminate(_, Chs, _, S, _) ->
    {ok, Chs, S, []}.


terminate(_, _) ->
    ok.