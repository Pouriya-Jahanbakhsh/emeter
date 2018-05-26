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
-module(emeter_api).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([handle_request/3
        ,encode/1
        ,decode/1
        ,wrap_true_reply/1
        ,wrap_false_reply/1
        ,default_node/0]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(RUN_TIMEOUT, 3000).

-ifdef(emeter_use_jiffy).
    -define(decode(Data), jiffy:decode(Data, [return_maps])).
    -define(encode(Data), jiffy:encode(Data, [force_utf8])).
-else.
    -define(decode(Data), jsone:decode(Data)).
    -define(encode(Data), jsone:encode(Data)).
-endif.

%% -------------------------------------------------------------------------------------------------
%% API:

handle_request(Cmd, Args, Node) when erlang:is_binary(Cmd) andalso
                                      erlang:is_list(Args) andalso
                                      erlang:is_atom(Node) ->
    case process_request(Cmd, Args, Node) of
        {ok, _}=Ok ->
            Ok;
        unknown ->
            case Args of
                [] ->
                    emeter_page:run(Cmd, Node);
                _ ->
                    {error, {unknown, [{node, Node}, {command, Cmd}, {arguments, Args}]}}
            end;
        Err ->
            Err
    end;
handle_request(Cmd, Args, Node) ->
    {error, {argument, [{command, Cmd}, {arguments, Args}, {node, Node}]}}.


decode(Data) ->
    try
        {ok, ?decode(Data)}
    catch
        _:Rsn ->
            {error, {json_decode, [{reason, Rsn}, {stacktrace, erlang:get_stacktrace()}]}}
    end.


encode(Data) ->
    try
        {ok, ?encode(Data)}
    catch
        _:Rsn ->
            {error, {json_encode, [{reason, Rsn}, {stacktrace, erlang:get_stacktrace()}]}}
    end.


wrap_true_reply(Data) ->
    #{<<"ok">> => true, <<"data">> => Data}.


wrap_false_reply(Rsn) ->
    #{<<"ok">> => false, <<"error">> => erlang:list_to_binary(io_lib:print(Rsn))}.


default_node() ->
    emeter_node:default().

%% -------------------------------------------------------------------------------------------------
%% Internals:

process_request(<<"system">>, [], Node) ->
    emeter_agent_api:call(Node, system_info, [], ?RUN_TIMEOUT);

process_request(<<"custom">>, [], _) ->
    {ok, [#{<<"title">> => Id
           ,<<"command">> => Id
           ,<<"refresh">> => RefreshTimeout
           ,<<"api_only">> => APIFlag}
         || {Id, {_, _, #{refresh_timeout := RefreshTimeout, api_only := APIFlag}}}
         <- emeter_page:all()]};

process_request(<<"node">>, [], Node) ->
    {ok, format_node(Node)};

process_request(<<"nodes">>, [], _) ->
    {ok, [format_node(Node) || Node <- emeter_node:all()]};

process_request(<<"allocators">>, [], Node) ->
    emeter_agent_api:call(Node, allocators_info, [], ?RUN_TIMEOUT);

process_request(<<"application">>, [], Node) ->
    emeter_agent_api:call(Node, applications_info, [], ?RUN_TIMEOUT);

process_request(<<"application">>, [Arg], Node) ->
    emeter_agent_api:call(Node, application_info, [Arg], ?RUN_TIMEOUT);
process_request(<<"process">>, [], Node) ->
    emeter_agent_api:call(Node, registered_pids_info, [], ?RUN_TIMEOUT);

process_request(<<"process">>, [Arg], Node) ->
    emeter_agent_api:call(Node, process_full_info, [Arg], ?RUN_TIMEOUT);

process_request(<<"ports">>, [], Node) ->
    emeter_agent_api:call(Node, ports_info, [], ?RUN_TIMEOUT);

process_request(<<"table">>, [], Node) ->
    emeter_agent_api:call(Node, ets_tables_info, [], ?RUN_TIMEOUT);

process_request(<<"table">>, [Arg], Node) ->
    emeter_agent_api:call(Node, ets_table_full_info, [Arg], ?RUN_TIMEOUT);

process_request(<<"about">>, [], _) ->
    {ok, emeter:about()};

process_request(_, _, _) ->
    unknown.


format_node(Node) ->
    #{<<"name">> => erlang:atom_to_binary(Node, utf8)}.