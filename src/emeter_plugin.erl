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
-module(emeter_plugin).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(gen_server).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/0
        ,start/1
        ,stop/1
        ,started/0
        ,filter_config/1]).

%% 'gen_server' callbacks:
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(PROC, ?MODULE).

-define(START_TAG, start).
-define(STOP_TAG, stop).
-define(STARTED_TAG, started).

-define(DEF_CALL_TIMEOUT, 10000).

-define(S, state).
-record(?S, {started}).

-include("emeter_log.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

start_link() ->
    gen_server:start_link({local, ?PROC}, ?MODULE, undefined, []).


start(Plugin) when erlang:is_atom(Plugin) ->
    gen_server:call(?PROC, {?START_TAG, Plugin}, ?DEF_CALL_TIMEOUT).


stop(Plugin) when erlang:is_atom(Plugin) ->
    gen_server:call(?PROC, {?STOP_TAG, Plugin}, ?DEF_CALL_TIMEOUT).


started() ->
    gen_server:call(?PROC, ?STARTED_TAG, ?DEF_CALL_TIMEOUT).


filter_config(Plugins) when erlang:is_list(Plugins) ->
    lists:all(fun erlang:is_atom/1, Plugins);
filter_config(_) ->
    fasle.

%% -------------------------------------------------------------------------------------------------
%% 'gen_server' callbacks:

init(_) ->
    case start_plugins(emeter:value(plugins), []) of
        {ok, Started} ->
            {ok, #?S{started = Started}};
        {error, Rsn} ->
            {stop, Rsn}
    end.


handle_call({?START_TAG, Plugin}, _, #?S{started = Started}=S) ->
    {Reply, Started2} =
        case do_start(Plugin) of
            ok ->
                {ok
                ,case lists:member(Plugin, Started) of
                     false ->
                         [Plugin|Started];
                     _ -> % true
                        Started
                 end};
            {error, _}=Err ->
                {Err, Started}
        end,
    {reply, Reply, S#?S{started = Started2}};
handle_call({?STOP_TAG, Plugin}, _, #?S{started = Started}=S) ->
    {Reply, Started2} =
        case do_stop(Plugin) of
            ok ->
                {ok
                ,case lists:member(Plugin, Started) of
                     true ->
                         lists:delete(Plugin, Started);
                     _ -> % false
                         Started
                 end};
            {error, _}=Err ->
                {Err, Started}
        end,
    {reply, Reply, S#?S{started = Started2}};
handle_call(?STARTED_TAG, _, #?S{started = Started}=S) ->
    {reply, Started, S};
handle_call(Req, _, S) ->
    {reply, {error, {unknown, [{request, Req}]}}, S}.



handle_cast(_, S) ->
    {noreplt, S}.


handle_info(_, S) ->
    {noreplt, S}.


terminate(_, #?S{started = Started}) ->
    lists:foreach(fun do_stop/1, Started).


code_change(_, S, _) ->
    {ok, S}.

%% -------------------------------------------------------------------------------------------------
%% Internals:

start_plugins([Plugin|Plugins], Ret) ->
    case do_start(Plugin) of
        ok ->
            start_plugins(Plugins, [Plugin|Ret]);
        Err -> % {error, _}
            Err
    end;
start_plugins(_, Ret) -> % ([], Ret)
    {ok, Ret}.


do_start(Plugin) ->
    ?info(<<"Starting plugin ~tp">>, [Plugin]),
    try Plugin:start() of
        ok ->
            ok;
        {error, _}=Err ->
            Err;
        Other ->
            {error, {return, [{value, Other}, {module, Plugin}, {function, start}]}}
    catch
        _:Rsn ->
            {error, {crash, [{reason, Rsn}
                            ,{stacktrace, erlang:get_stacktrace()}
                            ,{module, Plugin}
                            ,{function, start}]}}
    end.


do_stop(Plugin) ->
    ?info(<<"Stopping plugin ~tp">>, [Plugin]),
    try Plugin:stop() of
        ok ->
            ok;
        {error, _}=Err ->
            Err;
        Other ->
            {error, {return, [{value, Other}, {module, Plugin}, {function, start}]}}
    catch
        _:Rsn ->
            {error, {crash, [{reason, Rsn}
                            ,{stacktrace, erlang:get_stacktrace()}
                            ,{module, Plugin}
                            ,{function, start}]}}
    end.