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
-module(emeter_page).
-author("pouriya.jahanbakhsh@gmail.com").
-behavior(gen_server).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/0
        ,fetch/1
        ,run/2
        ,all/0
        ,add/5
        ,delete/1
        ,filter_config/1]).

%% 'gen_server' callbacks:
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Include:

-define(PROC, ?MODULE).
-define(TAB, ?MODULE).
-define(S, state).
-record(?S, {}).

-define(ADD_TAG, add).
-define(DELETE_TAG, delete).

-define(DEF_REFRESH_TIMEOUT, 3).
-define(DEF_API_ONLY, false).
-define(DEF_IS_TRANSFORMED, false).

-define(PAGE, page).
-record(?PAGE, {id, module, function, is_transformed, refresh_timeout, api_only}).

%% -------------------------------------------------------------------------------------------------
%% API:

start_link() ->
    gen_server:start_link({local, ?PROC}, ?MODULE, undefined, []).


fetch(Id) when erlang:is_binary(Id) ->
    case ets:lookup(?TAB, Id) of
        [PAGE] ->
            {ok, erlang:element(2, wrap(PAGE))};
        _ -> % []
            {error, not_found}
    end.


run(Id, Node) when erlang:is_binary(Id) ->
    case fetch(Id) of
        {ok, {Mod, Func, #{is_transformed := IsTransformed}}} ->
            if
                IsTransformed ->
                    try emeter_plugin_pt:forms(Mod, Func) of
                        Forms ->
                            emeter_agent_api:execute(Node, Forms)
                    catch
                        _:Rsn ->
                            {error, {crash, [{reason, Rsn}
                                            ,{stacktrace, erlang:get_stacktrace()}
                                            ,{page, Id}
                                            ,{module, Mod}
                                            ,{function, Func}]}}
                    end;
                true ->
                    try Mod:Func(Node) of
                        {ok, _}=Ok ->
                            Ok;
                        {error, _}=Err ->
                            Err;
                        Other ->
                            {error, {return, [{value, Other}
                                             ,{page, Id}
                                             ,{module, Mod}
                                             ,{function, Func}]}}
                    catch
                        _:Rsn ->
                            {error, {crash, [{reason, Rsn}
                                            ,{stacktrace, erlang:get_stacktrace()}
                                            ,{page, Id}
                                            ,{module, Mod}
                                            ,{function, Func}]}}
                    end
            end;
        _ -> % {error, not_found}
            {error, {not_found, [{page, Id}]}}
    end.


all() ->
    [wrap(Page) || Page <- ets:tab2list(?TAB)].


add(Id, PageMod, PageFunc, RefreshTimeout, APIFlag) when erlang:is_binary(Id) andalso
                                                         erlang:is_atom(PageMod) andalso
                                                         erlang:is_atom(PageFunc) andalso
                                                         erlang:is_integer(RefreshTimeout) andalso
                                                         erlang:is_boolean(APIFlag) ->
    gen_server:call(?PROC
                   ,{?ADD_TAG, #?PAGE{id = Id
                                       ,module = PageMod
                                       ,function = PageFunc
                                       ,refresh_timeout = RefreshTimeout
                                       ,is_transformed = emeter_plugin_pt:is_transformed(PageMod
                                                                                        ,PageFunc)
                                       ,api_only = APIFlag}}).


delete(Id) when erlang:is_binary(Id) ->
    gen_server:call(?PROC, {?DELETE_TAG, Id}).


filter_config(Pages) when erlang:is_list(Pages) ->
    filter_config(Pages, []);
filter_config(_) ->
    false.

%% -------------------------------------------------------------------------------------------------
%% 'gen_server' callbacks:

init(_) ->
    _ = ets:new(?TAB, [named_table, {keypos, 2}]),
    Insert =
        fun(Page) ->
            ets:insert(?TAB, Page)
        end,
    lists:foreach(Insert, emeter:value(pages)),
    {ok, #?S{}}.


handle_call({add, Page}, _, S) ->
    case fetch(Page#?PAGE.id) of
        {error, _} -> % {error, not_found}
            ets:insert(?TAB, Page),
            {reply, ok, S};
        _ -> % {ok, Page}
            {reply, already_exists, S}
    end;
handle_call({delete, Id}, _, S) ->
    case ets:lookup(?TAB, Id) of
        [Page] ->
            _ = ets:delete_object(?TAB, Page),
            {reply, ok, S};
        _ -> % {error, not_found}
            {reply, not_found, S}
    end.


handle_cast(_, S) ->
    {noreply, S}.


handle_info(_, S) ->
    {noreply, S}.


terminate(_, _) ->
    ok.


code_change(_, S, _) ->
    {ok, S}.

%% -------------------------------------------------------------------------------------------------
%% Internal:

wrap(#?PAGE{id =Id
           ,module = Mod
           ,function = Func
           ,is_transformed = IsTransformed
           ,refresh_timeout = RefreshTimeout
           ,api_only = APIFlag}) ->
    {Id, {Mod, Func, #{is_transformed => IsTransformed
                      ,refresh_timeout => RefreshTimeout
                      ,api_only => APIFlag}}}.

unwrap({Id, {Mod, Func, #{is_transformed := IsTransformed
                         ,refresh_timeout := RefreshTimeout
                         ,api_only := APIFlag}}}) ->
    #?PAGE{id =Id
          ,module = Mod
          ,function = Func
          ,is_transformed = IsTransformed
          ,refresh_timeout = RefreshTimeout
          ,api_only = APIFlag}.


filter_config([{Id, {Mod, Func, Opts}}=Page|Pages], Ret) when erlang:is_binary(Id) andalso
                                                              erlang:is_atom(Mod) andalso
                                                              erlang:is_atom(Func) andalso
                                                              erlang:is_map(Opts) ->
    case {maps:get(refresh_timeout, Opts, ?DEF_REFRESH_TIMEOUT)
         ,maps:get(api_only, Opts, ?DEF_API_ONLY)
         ,maps:get(is_transformed, Opts, ?DEF_IS_TRANSFORMED)} of
        {RefreshTimeout, APIFlag, IsTransformed} when erlang:is_number(RefreshTimeout) andalso
                                                      RefreshTimeout > 0 andalso
                                                      erlang:is_boolean(APIFlag) andalso
                                                      erlang:is_boolean(IsTransformed) ->
            filter_config(Pages, [unwrap({Id, {Mod, Func, #{is_transformed => IsTransformed
                                                           ,refresh_timeout => RefreshTimeout
                                                           ,api_only => APIFlag}}})|Ret]);
        _ -> % {_, _}
            {error, [{page, Page}]}
    end;
filter_config([Page|_], _) ->
    {error, [{page, Page}]};
filter_config(_, Ret) -> % ([], Ret)
    {ok, lists:reverse(Ret)}.