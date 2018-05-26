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
-module(emeter_web_websocket).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% 'cowboy' callbacks:
-export([init/2
        ,websocket_init/1
        ,websocket_handle/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("emeter_log.hrl").

%% -------------------------------------------------------------------------------------------------
%% 'cowboy' callbacks:

init(Req, State) ->
    {cowboy_websocket, Req, State}.


websocket_init(_) ->
    {ok, emeter_node:default()}.

websocket_handle({text, Json}, State) ->
    case emeter_api:decode(Json) of
        {ok, ParsedJson} ->
            handle_request(ParsedJson, State);
        {error, _} ->
            {reply
            ,{text, error_reply(<<"unknown">>, <<"Could not parse packet as JSON">>)}
            ,State}
    end.

%% -------------------------------------------------------------------------------------------------
%% Internals:

handle_request(#{<<"command">> := <<"ping">>}, Node) ->
    {reply, {text, ok_reply(<<"ping">>, #{})}, Node};

handle_request(#{<<"command">> := <<"switch_node">>}=Req, Node) ->
    Data = maps:get(<<"data">>, Req, null),
    {Reply, Node2} =
        case emeter_agent_api:can_be_atom(Data) of
            {true, Data2} ->
                case lists:member(Data2, emeter_node:all()) of
                    true ->
                        {#{<<"success">> => <<"Switched to: ", Data/binary>>, <<"node">> => Data}
                        ,Data2};
                    false ->
                        {#{<<"error">> => <<"Node is not in node list">>}, Node}
                end;
            _ -> % false
                {#{<<"error">> => <<"Bad node name">>}, Node}
        end,
    {reply, {text, ok_reply(<<"switch_node">>, Reply)}, Node2};

handle_request(#{<<"command">> := Cmd}, Node) ->
    {Cmd2, Args} = parse_command(Cmd),
    case emeter_api:handle_request(Cmd2, Args, Node) of
        {ok, Response} ->
            {reply, {text, ok_reply(Cmd, Response)}, Node};
        {error, Rsn} ->
            {reply, {text, error_reply(Cmd2, Rsn)}, Node}
    end;

handle_request(_, Node) ->
    {reply, {text, error_reply(<<"unknown">>, <<"Unknown command">>)}, Node}.


ok_reply(Cmd, Reply) ->
    Reply2 = emeter_api:wrap_true_reply(Reply),
    case emeter_api:encode(Reply2#{<<"type">> => Cmd
                                 ,<<"timestamp">> => emeter_agent_api:timestamp_second()}) of
        {ok, Response} ->
            Response;
        {error, Rsn} ->
            ?error(<<"Encoding to JSON causes error ~tp">>, [Rsn]),
            erlang:list_to_binary(io_lib:format("{\"error\":\"Internal server error\",\"timestamp\""
            ":~tp,\"type\":\"~ts\"}"           , [emeter_agent_api:timestamp_second(), Cmd]))
    end.

error_reply(Cmd, Rsn) ->
    Reply2 = emeter_api:wrap_false_reply(Rsn),
    case emeter_api:encode(Reply2#{<<"type">> => Cmd
                                 ,<<"timestamp">> => emeter_agent_api:timestamp_second()}) of
        {ok, Response} ->
            Response;
        {error, Rsn} ->
            ?error(<<"Encoding to JSON causes error ~tp">>, [Rsn]),
            erlang:list_to_binary(io_lib:format("{\"error\":\"Internal server error\",\"timestamp\""
            ":~tp,\"type\":\"~ts\"}"           , [emeter_agent_api:timestamp_second(), Cmd]))
    end.


parse_command(<<"application/", Rest/bytes>>) ->
    {<<"application">>, [Rest]};
parse_command(<<"process/", Rest/bytes>>) ->
    {<<"process">>, [Rest]};
parse_command(<<"table/", Rest/bytes>>) ->
    {<<"table">>, [Rest]};
parse_command(Cmd) ->
    {Cmd, []}.