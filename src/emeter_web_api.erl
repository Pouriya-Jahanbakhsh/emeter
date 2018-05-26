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
-module(emeter_web_api).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% 'cowboy' callbacks:
-export([init/2
        ,content_types_provided/2
        ,reply/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("emeter_log.hrl").

%% -------------------------------------------------------------------------------------------------
%% 'cowboy' callbacks:

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.


content_types_provided(Req, Opts) ->
    {[{<<"application/json">>, reply}, {<<"text/plain">>, reply}, {<<"text/html">>, reply}], Req, Opts}.


reply(#{bindings := Bindings}=Req, _) ->
    process_request(Bindings, Req);
reply(Req, S) ->
    {error_reply(unknown_request), Req, S}.

%% -------------------------------------------------------------------------------------------------
%% Internals:

process_request(#{node := Node, x := X}=Bindings, Req) ->
    case emeter_agent_api:can_be_atom(Node) of
        {true, Node2} ->
            Node3 = format_node(Node2),
            case emeter_node:is_node_exists(Node3) of
                true ->
                    {Call, Args} =
                        case maps:is_key(y, Bindings) of
                            true ->
                                {X, [maps:get(y, Bindings)]};
                            _ ->
                                {X, []}
                        end,
                    case emeter_api:handle_request(Call, Args, Node3) of
                        {ok, Response} ->
                            {ok_reply(Response), Req, Node3};
                        {error, Rsn} ->
                            {error_reply(Rsn), Req, Node3}
                    end;
                _ -> % false
                    {error_reply({unknown_node, [{node, Node}]}), Req, Node3}
            end;
        _ -> % false
            {error_reply({bad_node, [{node, Node}]}), Req, Node}
    end.


ok_reply(Data) ->
    Reply2 = emeter_api:wrap_true_reply(Data),
    case emeter_api:encode(Reply2#{<<"timestamp">> => emeter_agent_api:timestamp_second()}) of
        {ok, Response} ->
            Response;
        {error, Rsn} ->
            ?error(<<"Encoding to JSON causes error ~tp">>, [Rsn]),
            io:format(<<"Encoding to JSON causes error ~tp">>, [Rsn]),
            erlang:list_to_binary(io_lib:format("{\"error\":\"Internal server error\",\"timestamp\""
            ":~tp}"                            , [emeter_agent_api:timestamp_second()]))
    end.


error_reply(Rsn) ->
    Reply2 = emeter_api:wrap_false_reply(Rsn),
    case emeter_api:encode(Reply2#{<<"timestamp">> => emeter_agent_api:timestamp_second()}) of
        {ok, Response} ->
            Response;
        {error, Rsn} ->
            ?error(<<"Encoding to JSON causes error ~tp">>, [Rsn]),
            io:format(<<"Encoding to JSON causes error ~tp">>, [Rsn]),
            erlang:list_to_binary(io_lib:format("{\"error\":\"Internal server error\",\"timestamp\""
            ":~tp}"                            , [emeter_agent_api:timestamp_second()]))
    end.


format_node(local) ->
    emeter_api:default_node();
format_node(X) ->
    X.