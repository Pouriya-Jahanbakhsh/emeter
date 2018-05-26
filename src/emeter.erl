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
%%           Erlang Web based metrics, monitoring, and observer.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(emeter).
-author("pouriya.jahanbakhsh@gmail.com").
-behavior(application).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start/0
        ,stop/0
        ,value/1
        ,value/2
        ,set/2
        ,about/0]).

%% 'application' callbacks:
-export([start/2
        ,stop/1]).

%% -------------------------------------------------------------------------------------------------
%% Records && Macros && Includes:

-define(APP, emeter).
-define(DEF_PORT, 1995).
-define(DEF_WEB_PANEL, true).
-define(DEF_NODES, []).
-define(DEF_PLUGINS, []).
-define(DEF_PAGES, []).

%% -------------------------------------------------------------------------------------------------
%% API:

start() ->
    case application:ensure_all_started(?APP) of
        {ok, _} ->
            ok;
        Err -> % {error, _}
            Err
    end.


stop() ->
    application:stop(?APP).


value(Key) ->
    Ref = erlang:make_ref(),
    case application:get_env(?APP, Key, Ref) of
        Ref ->
            erlang:error({not_found, [{key, Key}]});
        Val ->
            Val
    end.


set(Key, Val) ->
    application:set_env(?APP, Key, Val).


value(Key, Def) ->
    application:get_env(?APP, Key, Def).


about() ->
    #{<<"version">> => <<"0.1.0">>
     ,<<"name">> => <<"EMeter">>
     ,<<"links">> => [#{<<"url">> => <<"https://hex.pm/packages/emeter">>
                       ,<<"name">> => <<"Hex">>}
                     ,#{<<"url">> => <<"https://github.com/pouriya-jahanbakhsh/emeter">>
                       ,<<"name">> => <<"Github">>}]
     ,<<"license">> => <<"Backend source codes are published under BSD 3-Clause and web source code"
                         "s have been copied and \nedited from Elixir's wobserver project and are p"
                         "ublished under their MIT license">>
     ,<<"description">> => <<"Erlang Web based metrics, monitoring, and observer.">>}.


%% -------------------------------------------------------------------------------------------------
%% 'application' callbacks:

start(_, _) ->
    _ = reload_config(),
    Dispatch =
        [{"/api/:node/:x/[:y]", emeter_web_api, undefined}
        |case value(web_panel) of
             true ->
                 [{"/ws", emeter_web_websocket, undefined}, {'_', emeter_web_static, undefined}];
             _ -> % false
                 []
         end],
    {ok, _} = cowboy:start_clear(my_http_listener
                                ,[{port, value(api_port)}]
                                ,#{env => #{dispatch => cowboy_router:compile([{'_', Dispatch}])}}),
    emeter_sup:start_link().


stop(_) ->
    ok.

%% -------------------------------------------------------------------------------------------------
%% Internals:

reload_config() ->
    reload_config([{api_port, ?DEF_PORT, fun erlang:is_integer/1}
                  ,{web_panel, ?DEF_WEB_PANEL, fun erlang:is_boolean/1}
                  ,{nodes, ?DEF_NODES, fun emeter_node:filter_config/1}
                  ,{plugins, ?DEF_PLUGINS, fun emeter_plugin:filter_config/1}
                  ,{pages, ?DEF_PAGES, fun emeter_page:filter_config/1}]).


reload_config([{Key, Def, Filter}|Rest]) ->
    Ref = erlang:make_ref(),
    case application:get_env(?APP, Key, Ref) of
        Ref ->
            set(Key, Def),
            reload_config(Rest);
        Val ->
            case Filter(Val) of
                {ok, Val2} ->
                    set(Key, Val2),
                    reload_config(Rest);
                true ->
                    set(Key, Val),
                    reload_config(Rest);
                false ->
                    erlang:error({config, [{key, Key}, {value, Val}]});
                {error, Rsn} ->
                    erlang:error({config, [{key, Key}, {value, Val}] ++ Rsn})
            end
    end;
reload_config(_) -> % ([])
    ok.