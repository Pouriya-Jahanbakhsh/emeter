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
-module(emeter_plugin_pt).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([is_transformed/2
        ,forms/2]).

%% 'compile' callback:
-export([parse_transform/2]).

%% -------------------------------------------------------------------------------------------------
%% API:

is_transformed(Mod, Func) ->
    case erlang:function_exported(Mod, Func, 0) of
        true ->
            should_be_transformed(Func, 0);
        _ -> % false
            false
    end.


forms(Mod, Func) ->
    erlang:binary_to_term(Mod:Func()).

%% -------------------------------------------------------------------------------------------------
%% 'compile' callback:

parse_transform(AST, _) ->
    transform(AST, []).

%% -------------------------------------------------------------------------------------------------
%% Internals:

transform([Form|AST], Ret) ->
    case erl_syntax:type(Form) of
        function ->
            transform(AST, [transform_function(Form)|Ret]);
        _ ->
            transform(AST, [Form|Ret])
    end;
transform(_, Ret) ->
    lists:reverse(Ret).


transform_function(Func) ->
    {Name, Arity} = erl_syntax_lib:analyze_function(Func),
    case should_be_transformed(Name, Arity) of
        true ->
            [Clause] = erl_syntax:function_clauses(Func),
            make_function(Name, Clause);
        _ -> % false
            Func
    end.


make_function(Name0, Clause) ->
    Pos = erl_syntax:get_pos(Clause),
    Name = erl_syntax:set_pos(erl_syntax:atom(Name0), Pos),
    Bodybin = erlang:binary_to_list(erlang:term_to_binary(erl_syntax:clause_body(Clause))),
    BodyTerm = erl_syntax:binary([erl_syntax:binary_field(erl_syntax:string(Bodybin))]),
    erl_syntax:revert(erl_syntax:function(erl_syntax:revert(Name)
                                         ,[erl_syntax:clause([]
                                                            ,[]
                                                            ,[erl_syntax:revert(BodyTerm)])])).


should_be_transformed(Func, 0) ->
    case lists:reverse(erlang:atom_to_list(Func)) of
        [$t, $p, $_, _ | _]-> % *_pt/0
            true;
        _ ->
            false
    end;
should_be_transformed(_, _) ->
    false.