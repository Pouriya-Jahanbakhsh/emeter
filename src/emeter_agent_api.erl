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
-module(emeter_agent_api).
-author("pouriya.jahanbakhsh@gmail.com").
-behavior(gen_server).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([call/4
        ,call/3
        ,system_info/0
        ,allocators_info/0
        ,process_short_info/1
        ,process_full_info/1
        ,applications_info/0
        ,application_info/1
        ,registered_pids_info/0
        ,registered_pid_short_info/2
        ,ports_info/0
        ,port_info/1
        ,ets_tables_info/0
        ,ets_table_short_info/1
        ,ets_table_full_info/1
        ,are_system_functions_exported/1
        ,start/1
        ,execute/2
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,can_be_atom/1
        ,can_be_ets_table/1
        ,can_be_process/1
        ,timestamp_second/0]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(SCHEDULER_INFO_TAB, emeter_scheduler_wall_time).
-define(DEF_RUN_TIMEOUT, 3000).
-define(DEF_GET_STATE_TIMEOUT, 10000).
-define(stacktrace, {stacktrace, erlang:get_stacktrace()}).

%% Guards:
-define(is_remote_node(Arg), (erlang:is_atom(Arg) andalso erlang:node() /= Arg)).
-define(is_timeout(Arg), ((erlang:is_integer(Arg) andalso Arg >= 0) orelse Arg == infinity)).
-define(is_process(Arg), (erlang:is_atom(Arg) orelse erlang:is_pid(Arg))).

-define(is_ets_table(Arg), ((erlang:is_integer(Arg) andalso Arg >= 0) orelse
                           erlang:is_atom(Arg) orelse
                           erlang:is_reference(Arg))). % New OTP versions use reference for ETS id

%% Agent process:
-define(PROC, ?MODULE).
-define(S, state).
-record(?S, {}).
-define(EXECUTE_TAG, execute).
-define(DEF_HIBERNATE_TIMEOUT, 7000).

-include("emeter_log.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:
%% In all functions i want to guarantee yielding {ok, Result} or {error, Why}, not raising exception

call(Node, Func, Args, Timeout) when ?is_remote_node(Node) andalso
                                     erlang:is_atom(Func) andalso
                                     erlang:is_list(Args) andalso
                                     ?is_timeout(Timeout) ->
    %% In future releases i will use our agent process for this call:
    case rpc:call(Node, emeter_agent_api, call, [Func, Args, Timeout], Timeout) of
        {badrpc, Rsn} ->
            {error, {rpc, [{reason, Rsn}
                          ,{node, Node}
                          ,{function, Func}
                          ,{arguments, Args}
                          ,{timeout, Timeout}]}};
        Data -> % {ok, _} | {error, _}
            Data
    end;
call(Node, Func, Args, Timeout) when Node =:= erlang:node() andalso
                                     erlang:is_atom(Func) andalso
                                     erlang:is_list(Args) andalso
                                     ?is_timeout(Timeout) ->
    call(Func, Args, Timeout);
call(Node, Func, Args, Timeout) ->
    {error, {argument, [{node, Node}
                       ,{function, Func}
                       ,{arguments, Args}
                       ,{timeout, Timeout}
                       ,?stacktrace]}}.


call(Func, Args, Timeout) ->
    Pid = erlang:self(),
    Ref = erlang:make_ref(),
    RunFun =
        fun() ->
            Result =
                try
                    erlang:apply(?MODULE, Func, Args)
                catch
                    _:Rsn ->
                        {error, {crash, [{reason, Rsn}
                                        ,?stacktrace
                                        ,{function, Func}
                                        ,{arguments, Args}]}}
                end,
            Pid ! {Ref, Result}
        end,
    Result =
        try erlang:spawn(RunFun) of
            RunPid ->
                receive
                    {Ref, RunResult} ->
                        RunResult
                after Timeout ->
                    erlang:exit(RunPid, kill),
                    {error, {timeout, [{timeout, Timeout}
                                      ,{function, Func}
                                      ,{arguments, Args}]}}
                end
        catch
            _:Rsn -> % system_limit
                {error, {spawn, [{reason, Rsn}, {function, Func}, {arguments, Args}]}}
        end,
    case Result of
        {Tag, _} when Tag == ok orelse Tag == error ->
            Result;
        _ ->
            {error, {return, [{value, Result}, {function, Func}, {arguments, Args}]}}
    end.

system_info() ->
    {ok, #{<<"architecture">> => architecture_info()
          ,<<"cpu">> => cpu_info()
          ,<<"memory">> => memory_info()
          ,<<"statistics">> => statistics()
          ,<<"scheduler">> => scheduler_info()}}.


allocators_info() ->
    SumFunc =
        fun({Type, Info}) ->
            XY =
                fun({_, _, D}) ->
                    {_, [{_, X, _, _}, {_, Y, _, _}]} = lists:keyfind(mbcs, 1, D),
                    {X, Y}
                end,
            XYs = [XY(Item) || Item <- Info],
            Fold =
                fun({X_, Y_}, {XAcc, YAcc}) ->
                    {XAcc + X_, YAcc + Y_}
                end,
            {X, Y} = lists:foldl(Fold, {0, 0}, XYs),
            #{<<"type">> => Type, <<"block">> => X, <<"carrier">> => Y}
        end,
    Sum = [SumFunc({Type, Info}) || {Type, Info}
          <- erlang:system_info({allocator_sizes, erlang:system_info(alloc_util_allocators)})],
    Filter =
        fun(#{<<"block">> := B, <<"carrier">> := C}) ->
            B =/= 0 andalso C =/= 0
        end,
    {ok, lists:filter(Filter, Sum)}.


process_short_info(Proc) when ?is_process(Proc) ->
    case fetch_process_info(Proc, [status, initial_call, current_function, dictionary]) of
        {ok, Find} -> % Find is a fun which holds list of returned information too
            {InitialCall, InitialCallBin} = process_initial_call(Find),
            {ok, #{<<"status">> => binary(Find(status, <<"unknown">>))
                  ,<<"init">> => InitialCallBin
                  ,<<"current">> => binary(Find(current_function, <<"unknown">>))
                  ,<<"behaviour">> => process_behavior(InitialCall)}};
        Err ->
            Err
    end;
process_short_info(Arg) when erlang:is_binary(Arg) ->
    case can_be_process(Arg) of
        {true, Proc} ->
            process_short_info(Proc);
        false ->
            {error, {argument, [{process, Arg}, {function, process_short_info}]}}
    end;
process_short_info(Arg) ->
    {error, {argument, [{process, Arg}, {function, process_short_info}]}}.


process_full_info(Proc) when ?is_process(Proc) ->
    case fetch_process_info(Proc
                           ,[trap_exit
                            ,links
                            ,group_leader
                            ,registered_name
                            ,priority
                            ,message_queue_len
                            ,total_heap_size
                            ,stack_size
                            ,heap_size
                            ,garbage_collection
                            ,error_handler
                            ,status
                            ,initial_call
                            ,current_function
                            ,dictionary]) of
        {ok, Find} ->
            {InitialCall, InitialCallBin} = process_initial_call(Find),
            Ancestors =
                case lists:keyfind('$ancestors', 1, Find(dictionary, [])) of
                    {_, Ancestors2} ->
                        [binary(X) || X <- Ancestors2];
                    _ -> % {...} | false
                        []
                end,
            Memory = Find(garbage_collection, [{min_heap_size, 0}, {full_sweep_after, 0}]),
            Behavior = process_behavior(InitialCall),
            {ok, #{<<"trap_exit">> => Find(trap_exit, <<"false">>)
                  ,<<"state">> => process_state(Proc, InitialCall, Behavior)
                  ,<<"relations">> => #{<<"links">> => [binary(X) || X <- Find(links, [])]
                                       ,<<"group_leader">> => binary(Find(group_leader, <<"unknown">>))
                                       ,<<"ancestors">> => Ancestors}
                  ,<<"registered_name">> => binary(Find(registered_name, Proc))
                  ,<<"priority">> => binary(Find(priority, <<"normal">>))
                  ,<<"pid">> => binary(Proc)
                  ,<<"meta">> => #{<<"status">> => binary(Find(status, <<"unknown">>))
                                  ,<<"init">> => InitialCallBin
                                  ,<<"current">> => binary(Find(current_function, <<"unknown">>))
                                  ,<<"behaviour">> => Behavior}
                  ,<<"message_queue_len">> => Find(message_queue_len, 0)
                  ,<<"memory">> => #{<<"total">> => Find(total_heap_size, 0)
                                    ,<<"stack_size">> => Find(stack_size, 0)
                                    ,<<"stack_and_heap">> => Find(total_heap_size, 0)
                                    ,<<"heap_size">> => Find(heap_size, 0)
                                    ,<<"gc_min_heap_size">> =>
                                     proplists:get_value(min_heap_size, Memory, 0)
                                    ,<<"gc_full_sweep_after">> =>
                                     proplists:get_value(full_sweep_after, Memory, 0)}
                 ,<<"error_handler">> => binary(Find(error_handler, error_handler))}};
        Err ->
            Err
    end;
process_full_info(Arg) when erlang:is_binary(Arg) ->
    case can_be_process(Arg) of
        {true, Proc} ->
            process_full_info(Proc);
        false ->
            {error, {argument, [{process, Arg}, {function, process_full_info}]}}
    end;
process_full_info(Arg) ->
    {error, {arguemnt, [{process, Arg}, {function, process_full_info}]}}.


applications_info() ->
    %% Which application has supervision tree?
    Alive =
        fun({App, _, _}) ->
            try application_controller:get_master(App) of
                Pid when erlang:is_pid(Pid) ->
                    true;
                _ ->
                    false
            catch
                _:_ ->
                    false
            end
        end,
    {ok, [#{<<"name">> => binary(App)
           ,<<"description">> => binary(Desc)
           ,<<"version">> => binary(Ver)}
         || {App, Desc, Ver} <- lists:filter(Alive, application:which_applications())]}.


application_info(App) when erlang:is_atom(App) ->
    try application_controller:get_master(App) of
        Master when erlang:is_pid(Master) ->
            try application_master:get_child(Master) of
                {RootSup, _} when erlang:is_pid(RootSup) ->
                    case supervison_tree_info(Master, worker) of
                        {ok, Info} ->
                            case supervison_tree_info(RootSup, supervisor) of
                                {ok, Info2} ->
                                    {ok, Info#{<<"children">> => [Info2]}};
                                Err -> % {error, _}
                                    Err
                            end;
                        Err -> % {error, _}
                            Err
                    end;
                Other ->
                    {error, {application_supervisor, [{supervisor, Other}
                                                     ,{master, Master}
                                                     ,{application, App}]}}
            catch
                _:Rsn ->
                    {error, {application_supervisor, [{reason, Rsn}
                                                     ,{master, Master}
                                                     ,{application, App}]}}
            end;
        Other ->
            {error, {application_master, [{master, Other}, {application, App}]}}
    catch
        _:Rsn ->
            {error, {application_master, [{reason, Rsn}, {application, App}]}}
    end;
application_info(Arg) when erlang:is_binary(Arg) ->
    case can_be_atom(Arg) of
        {true, App} ->
            application_info(App);
        false ->
            {error, {argument, [{application, Arg}, {function, application_info}]}}
    end;
application_info(Arg) ->
    {error, {argument, [{application, Arg}, {function, application_info}]}}.


registered_pids_info() ->
    registered_pids_info([{Name, X} || {Name, X}
                         <- [{Name2, erlang:whereis(Name2)} || Name2 <- erlang:registered()]
                         ,  erlang:is_pid(X)]
                        ,[]).


ports_info() ->
    ports_info(erlang:ports(), []).


ets_tables_info() ->
    ets_tables_info(ets:all(), []).


ets_table_short_info(Tab) when ?is_ets_table(Tab) ->
    case fetch_ets_table_info(Tab) of
        {ok, Find} ->
            {ok, #{<<"type">> => binary(Find(type, <<"unknown">>))
                  ,<<"size">> => Find(size, 0)
                  ,<<"protection">> => binary(Find(protection, <<"unknown">>))
                  ,<<"owner">> => binary(Find(owner, <<"unknown">>))
                  ,<<"name">> => binary(Find(name, <<"unknown">>))
                  ,<<"meta">> => #{<<"write_concurrency">> =>
                                   binary(Find(write_concurrency, <<"unknown">>))
                                  ,<<"read_concurrency">> =>
                                   binary(Find(read_concurrency, <<"unknown">>))
                                  ,<<"compressed">> => binary(Find(compressed, <<"unknown">>))}
                  ,<<"memory">> => Find(memory, 0)
                  ,<<"id">> => binary(Tab)}};
        Err ->
            Err
    end;
ets_table_short_info(Arg) when erlang:is_binary(Arg) ->
    case can_be_ets_table(Arg) of
        {true, Tab} ->
            ets_table_short_info(Tab);
        false ->
            {error, {argument, [{table, Arg}, {function, ets_table_short_info}]}}
    end;
ets_table_short_info(Arg) ->
    {error, {argument, [{table, Arg}, {function, ets_table_short_info}]}}.


ets_table_full_info(Tab) when ?is_ets_table(Tab) ->
    case ets_table_short_info(Tab) of
        {ok, Info} ->
            FoldFun =
                fun(Item, Acc) ->
                    [[binary(Item2) || Item2 <- erlang:tuple_to_list(Item)] |Acc]
                end,
            Data =
                try
                    ets:foldr(FoldFun, [], Tab)
                catch
                    _:_ ->
                        []
                end,
            {ok, Info#{<<"data">> => Data}};
        Err ->
            Err
    end;
ets_table_full_info(Arg) when erlang:is_binary(Arg) ->
    case can_be_ets_table(Arg) of
        {true, Tab} ->
            ets_table_full_info(Tab);
        false ->
            {error, {argument, [{table, Arg}, {function, ets_table_full_info}]}}
    end;
ets_table_full_info(Arg) ->
    {error, {argument, [{table, Arg}, {function, ets_table_full_info}]}}.


can_be_process(<<"%3", Int:8/integer, _/binary>> = QS) when Int == 67 orelse
                                                            Int == 99 -> % Int: C | c - starts with
                                                                         % 3c or %3C which is (<)
                                                                         % character and may be
                                                                         % <Int.Int.Int>
    try
        case cow_qs:parse_qs(QS) of
            [{Pid, true}] ->
                can_be_process(Pid);
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end;
can_be_process(<<"<", _/binary>> = Pid) ->
    try
        {true, erlang:list_to_pid(erlang:binary_to_list(Pid))}
    catch
        _:_ ->
            false
    end;
can_be_process(Arg) ->
    can_be_atom(Arg).


can_be_atom(<<"%27", _/bytes>> = QS) -> % starts with ' and may be '...'
    try
        case cow_qs:parse_qs(QS) of
            [{Data, true}] ->
                can_be_atom(Data);
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end;
can_be_atom(<<Int:8/integer, _/binary>> = Atom) when (Int > 96 andalso Int < 123) orelse
                                                     Int == 39 -> % Int: a-z and '
    try
        % Prevent registering new atoms:
        {true, erlang:list_to_existing_atom(erlang:binary_to_list(Atom))}
    catch
        _:_ ->
            false
    end;
can_be_atom(_) ->
    false.


can_be_ets_table(<<Int:8/integer, _/binary>> = TabId) when Int > 46 andalso Int < 58 ->
    try
        {true, erlang:binary_to_integer(TabId)}
    catch
        _:_ ->
            false
    end;
can_be_ets_table(<<"%23", _/bytes>> = QS) -> % starts with # and may be reference #Ref...
    try
        case cow_qs:parse_qs(QS) of
            [{Data, true}] ->
                % New OTP versions have list_to_ref/1
                {true, erlang:list_to_ref(erlang:binary_to_list(Data))};
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end;
can_be_ets_table(<<"#Ref", _/bytes>>=Ref) ->
    try
        {true, erlang:list_to_ref(erlang:binary_to_list(Ref))}
    catch
        _:_ ->
            false
    end;
can_be_ets_table(Arg) ->
    can_be_atom(Arg).


timestamp_second() ->
    {Mega, S, _} = os:timestamp(),
    (Mega*1000000)+S.

%% -------------------------------------------------------------------------------------------------
%% Agent process API:

-spec
start(node()) ->
    {'ok', pid()} | {'error', term()}.
%% @doc
%%      Starts agent process and registers it locally on Node named emeter_agent_api.
%% @end
start(Node) when erlang:is_atom(Node) ->
    if
        Node == erlang:node() ->
            gen_server:start({local, ?PROC}, ?MODULE, undefined, []);
        true ->
            case rpc:call(Node, ?MODULE, start, [Node]) of
                {badrpc, Rsn} ->
                    {error, {rpc, [{reason, Rsn}, {node, Node}]}};
                Other -> % {ok, _} | {error, _}
                    Other
            end
    end.


-spec
execute(node(), list()) ->
    {'ok', term()} | {'error', term()}.
%% @doc
%%      Executes Forms on node Node.
%% @end
execute(Node, Forms) when erlang:is_atom(Node) andalso erlang:is_list(Forms) ->
    case rpc:server_call(Node, ?PROC, ?EXECUTE_TAG, {?EXECUTE_TAG, Forms}) of
        {badrpc, Rsn} ->
            {error, {rpc, [{reason, Rsn}, {node, Node}]}};
        {ok, _}=Ok ->
            Ok;
        {error, _}=Err ->
            Err;
        Other ->
            {error, {return, [{value, Other}, {node, Node}]}}
    end.

%% -------------------------------------------------------------------------------------------------
%% 'gen_server':

%% @hidden
init(_) ->
    {ok, #?S{}, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
handle_call(Req, _, S) ->
    {reply, {error, {unknown, [{request, Req}]}}, S, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
handle_cast(_, S) ->
    {noreply, S, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
handle_info({From, {?EXECUTE_TAG, Exprs}}, S) ->
    From ! {?EXECUTE_TAG, erlang:node(), execute(Exprs)},
    {noreply, S, ?DEF_HIBERNATE_TIMEOUT};
handle_info(timeout, S) ->
    {noreply, S, hibernate};
handle_info(_, S) ->
    {noreply, S, ?DEF_HIBERNATE_TIMEOUT}.


%% @hidden
terminate(_, _) ->
    ok.


%% @hidden
code_change(_, S, _) ->
    {ok, S}.

%% -------------------------------------------------------------------------------------------------
%% Internal:

call(Func, Args, Timeout) ->
    Pid = erlang:self(),
    Ref = erlang:make_ref(),
    RunFun =
        fun() ->
            Result =
                try
                    erlang:apply(?MODULE, Func, Args)
                catch
                    _:Rsn ->
                        {error, {crash, [{reason, Rsn}
                                        ,?stacktrace
                                        ,{function, Func}
                                        ,{arguments, Args}]}}
                end,
            Pid ! {Ref, Result}
        end,
    Result =
        try erlang:spawn(RunFun) of
            RunPid ->
                receive
                    {Ref, RunResult} ->
                        RunResult
                after Timeout ->
                    erlang:exit(RunPid, kill),
                    {error, {timeout, [{timeout, Timeout}
                                      ,{function, Func}
                                      ,{arguments, Args}]}}
                end
        catch
            _:Rsn -> % system_limit
                {error, {spawn, [{reason, Rsn}, {function, Func}, {arguments, Args}]}}
        end,
    case Result of
        {Tag, _} when Tag == ok orelse Tag == error ->
            Result;
        _ ->
            {error, {return, [{value, Result}, {function, Func}, {arguments, Args}]}}
    end.

execute(Exprs) ->
    try erl_eval:exprs(Exprs, erl_eval:new_bindings()) of
        {value, Val, _} ->
            Val
    catch
        _:Rsn ->
            {error, {crash, [{reason, Rsn}, ?stacktrace]}}
    end.


architecture_info() ->
    #{<<"otp_release">> => binary(erlang:system_info(otp_release))
     ,<<"erts_version">> => binary(erlang:system_info(version))
     ,<<"system_architecture">> => binary(erlang:system_info(system_architecture))
     ,<<"kernel_poll">> => erlang:system_info(kernel_poll)
     ,<<"smp_support">> => erlang:system_info(smp_support)
     ,<<"threads">> => erlang:system_info(threads)
     ,<<"thread_pool_size">> => erlang:system_info(thread_pool_size)
     ,<<"wordsize_internal">> => erlang:system_info({wordsize, internal})
     ,<<"wordsize_external">> => erlang:system_info({wordsize, external})}.


cpu_info() ->
    Schs = erlang:system_info(logical_processors),
    Sch =
        case erlang:system_info(multi_scheduling) of
            enabled ->
                Schs;
            _ ->
                1
        end,
    #{<<"logical_processors">> => Schs
     ,<<"logical_processors_online">> => erlang:system_info(logical_processors_online)
     ,<<"logical_processors_available">> => erlang:system_info(logical_processors_available)
     ,<<"schedulers">> => erlang:system_info(schedulers)
     ,<<"schedulers_online">> => Schs
     ,<<"schedulers_available">> => Sch}.


memory_info() ->
    M = erlang:memory(),
    Find =
        fun(Key, Def) ->
            case lists:keyfind(Key, 1, M) of
                {_, Val} ->
                    Val;
                _ ->
                    Def
            end
        end,
    #{<<"atom">> => Find(atom, 0)
     ,<<"binary">> => Find(binary, 0)
     ,<<"code">> => Find(code, 0)
     ,<<"ets">> => Find(ets, 0)
     ,<<"process">> => Find(processes, 0)
     ,<<"total">> => Find(total, 0)}.


statistics() ->
    AtomCount =
        try
            erlang:system_info(atom_count)
        catch
            _:_ ->
                try
                    %% thanks to https://engineering.klarna.com/monitoring-erlang-atoms-c1d6a741328e
                    Info = erlang:system_info(info),
                    Chunks = binary:split(Info, <<"=">>, [global]),
                    [TabInfo] = [X || <<"index_table:atom_tab", X/binary>> <- Chunks],
                    Lines = binary:split(TabInfo, <<"\n">>, [global]),
                    Chunks2 = [erlang:list_to_tuple(binary:split(L, <<": ">>))
                        || L <- Lines, L =/= <<>>],
                    binary_to_integer(proplists:get_value(<<"entries">>, Chunks2))
                catch
                    _:_ ->
                        <<"unknown">>
                end
        end,
    {{_, I}, {_, O}} = erlang:statistics(io),
    #{<<"uptime">> => erlang:element(1, erlang:statistics(wall_clock))
     ,<<"process_running">> => erlang:statistics(run_queue)
     ,<<"process_total">> => erlang:system_info(process_count)
     ,<<"process_max">> => erlang:system_info(process_limit)
     ,<<"input">> => I
     ,<<"output">> => O
     ,<<"atom_count">> => AtomCount}.


scheduler_info() ->
    %% Thanks to github.com/shinyscorpion/wobserver/blob/master/lib/wobserver/system/scheduler.ex
    case ets:info(?SCHEDULER_INFO_TAB) of
        undefined ->
            erlang:system_flag(scheduler_wall_time, true),
            ets:new(?SCHEDULER_INFO_TAB, [named_table, public]),
            ok;
        _ ->
            ok
    end,
    SchInfo = lists:keysort(1, erlang:statistics(scheduler_wall_time)),
    Result =
        case ets:lookup(?SCHEDULER_INFO_TAB, key) of
            [{_, Info}] ->
                Map =
                    fun({{_, U1, T1}, {_, U2, T2}}) ->
                        case {U2 - U1, T2 - T1} of
                            {_, 0} ->
                                0;
                            {U3, T3} ->
                                U3 / T3
                        end
                    end,
                [Map(Item) || Item <- lists:zip(SchInfo, Info)];
            _ ->
                Map =
                    fun({_, U, T}) ->
                        case T of
                            0 ->
                                0;
                            _ ->
                                U / T
                        end
                    end,
                [Map(Item) || Item <- SchInfo]
        end,
    ets:insert(?SCHEDULER_INFO_TAB, {key, SchInfo}),
    Result.


supervison_tree_info(Pid, supervisor) ->
    try gen_server:call(Pid, which_children, ?DEF_RUN_TIMEOUT div 2) of
        %% We don't know that this is a valid supervisor or not
        %% Can't you believe? look at github.com/processone/xmpp/blob/1.1.19/src/xmpp.erl#L114 :-S
        Children when erlang:is_list(Children) ->
            case process_short_info(Pid) of
                {ok, PidInfo} ->
                    case supervisor_tree_info(Children, []) of
                        {ok, Info} ->
                            {ok, #{<<"pid">> => binary(Pid)
                                  ,<<"name">> => binary(Pid)
                                  ,<<"meta">> => PidInfo
                                  ,<<"children">> => Info}};
                        Err -> % {error, _}
                            Err
                    end;
                Err -> % {error, _}
                    Err
            end;
        Other ->
            {error, {which_children, [{value, Other}, {supervisor, Pid}]}}
    catch
        _:Rsn ->
            {error, {which_children, [{reason, Rsn}, {supervisor, Pid}]}}
    end;
supervison_tree_info(Pid, _) -> % (Pid, worker)
    case process_short_info(Pid) of
        {ok, Info} ->
            {ok, #{<<"pid">> => binary(Pid)
                  ,<<"name">> => binary(Pid)
                  ,<<"meta">> => Info
                  ,<<"children">> => []}};
        Err -> % {error, _}
            Err
    end.


supervisor_tree_info([{_, Pid, Type, _}|Rest]
                    ,Info) when erlang:is_pid(Pid) andalso
                                (Type =:= supervisor orelse Type =:= worker) ->
    case supervison_tree_info(Pid, Type) of
        {ok, Info2} ->
            supervisor_tree_info(Rest, [Info2|Info]);
        Err -> % {error, _}
            Err
    end;
supervisor_tree_info([{_, _, _, _}|Rest], Info) ->
    supervisor_tree_info(Rest, Info);
supervisor_tree_info([], Info) ->
    {ok, lists:reverse(Info)};
supervisor_tree_info([Info|_], _) ->
    {error, {which_children, [{info, Info}]}}.


registered_pids_info([{Name, Pid}|Pids], Info) ->
    case registered_pid_short_info(Name, Pid) of
        {ok, Info2} ->
            registered_pids_info(Pids, [Info2|Info]);
        Err -> % {error, _}
            Err
    end;
registered_pids_info([], Info) ->
    {ok, lists:reverse(Info)}.


registered_pid_short_info(Name, Pid) when erlang:is_atom(Name) andalso erlang:is_pid(Pid) ->
    case fetch_process_info(Pid
                           ,[reductions
                            ,initial_call
                            ,message_queue_len
                            ,current_function
                            ,dictionary]) of
        {ok, Find} ->
            {_, InitialCallBin} = process_initial_call(Find),
            {ok, #{<<"reductions">> => binary(Find(reductions, 0))
                 ,<<"pid">> => binary(Pid)
                 ,<<"name">> => binary(Name)
                 ,<<"message_queue_length">> => Find(message_queue_len, 0)
                 ,<<"memory">> => 0
                 ,<<"init">> => InitialCallBin
                 ,<<"current">> => binary(Find(current_function, <<"unknown">>))}};
        Err -> % {error, _}
            Err
    end.


ports_info([Port|Ports], InfoList) ->
    case port_info(Port) of
        {ok, Info} ->
            ports_info(Ports, [Info|InfoList]);
        Err ->
            Err
    end;
ports_info(_, InfoList) ->
    {ok, InfoList}.


port_info(Port) when erlang:is_port(Port) orelse erlang:is_atom(Port) ->
    case fetch_port_info(Port) of
        {ok, Find} ->
            {ok, #{<<"port">> => binary(Port)
                 ,<<"output">> => Find(output, 0)
                 ,<<"os_pid">> => binary(Find(os_pid, <<"undefined">>))
                 ,<<"name">> => binary(Find(name, <<"undefined">>))
                 ,<<"links">> => [binary(X) || X <- Find(links, [])]
                 ,<<"input">> => Find(input, 0)
                 ,<<"id">> => Find(id, 0)
                 ,<<"connected">> => binary(Find(connected, 0))}};
        Err ->
            Err
    end.


fetch_port_info(Port) when erlang:is_port(Port) ->
    CurrentNode = erlang:node(),
    PortNode = erlang:node(Port),
    if
        PortNode =:= CurrentNode ->
            case erlang:port_info(Port) of
                Info2 when erlang:is_list(Info2) ->
                    {ok
                    % Yield Find/2 fun
                    ,fun(Key, Def) ->
                        case lists:keyfind(Key, 1, Info2) of
                            {_, Val} ->
                                Val;
                            _ ->
                                Def
                        end
                     end};
                _ ->
                    {error, {unknown, [{function, port_info}, {port, Port}]}}
            end;
        true ->
            try rpc:call(PortNode
                        ,erlang
                        ,port_info
                        ,[Port]
                        ,?DEF_RUN_TIMEOUT) of
                Info2 when erlang:is_list(Info2) ->
                    {ok
                    ,fun(Key, Def) ->
                        case lists:keyfind(Key, 1, Info2) of
                            {_, Val} ->
                                Val;
                            _ ->
                                Def
                        end
                     end};
                {badrpc, Rsn} ->
                    {error, {rpc, [{reason, Rsn}, {port, Port}, {function, port_info}]}};
                _ ->
                    {error, {unknown, [{port, Port}, {function, port_info}]}}
            catch
                _:Rsn2 ->
                    {error, {rpc, [{reason, Rsn2}
                                  ,?stacktrace
                                  ,{port, Port}
                                  ,{function, port_info}]}}
            end
    end;
fetch_port_info(Name) when erlang:is_atom(Name) ->
    case erlang:whereis(Name) of
        Port when erlang:is_port(Port) ->
            fetch_port_info(Port);
        _ -> % undefined | ProcessName
            {error, {unknown, [{name, Name}, {function, port_info}]}}
    end;
fetch_port_info(Port) ->
    {error, {argument, [{port, Port}, {function, port_info}]}}.


fetch_ets_table_info(Tab) when ?is_ets_table(Tab) ->
    try ets:info(Tab) of
        Info when erlang:is_list(Info) ->
            {ok
                ,fun(Key, Def) ->
                case lists:keyfind(Key, 1, Info) of
                    {_, Val} ->
                        Val;
                    _ ->
                        Def
                end
                 end};
        Other ->
            {error, {unknown, [{value, Other}, {function, ets_info}, {table, Tab}]}}
    catch
        _:Rsn ->
            {error, {unknown, [{reason, Rsn}, {function, ets_info}, {table, Tab}]}}
    end;
fetch_ets_table_info(Tab) ->
    {error, {unknown, [{function, ets_info}, {table, Tab}]}}.


ets_tables_info([Tab|Tabs], InfoList) ->
    case ets_table_short_info(Tab) of
        {ok, Info} ->
            ets_tables_info(Tabs, [Info|InfoList]);
        Err ->
            Err
    end;
ets_tables_info(_, InfoList) -> % ([], ...)
    {ok, InfoList}.


fetch_process_info(Pid, Opts) when erlang:is_pid(Pid) andalso erlang:is_list(Opts) ->
    CurrentNode = erlang:node(),
    PidNode = erlang:node(Pid),
    if
        PidNode =:= CurrentNode ->
            case erlang:process_info(Pid, Opts) of
                Info2 when erlang:is_list(Info2) ->
                    {ok
                    ,fun(Key, Def) ->
                        case lists:keyfind(Key, 1, Info2) of
                            {_, Val} ->
                                Val;
                            _ ->
                                Def
                        end
                     end};
                _ ->
                    {error, {unknown, [{function, process_info}, {pid, Pid}]}}
            end;
        true ->
            try rpc:call(PidNode, erlang, process_info, [Pid, Opts], ?DEF_RUN_TIMEOUT) of
                Info2 when erlang:is_list(Info2) ->
                    {ok
                    ,fun(Key, Def) ->
                        case lists:keyfind(Key, 1, Info2) of
                            {_, Val} ->
                                Val;
                            _ ->
                                Def
                        end
                     end};
                {badrpc, Rsn} ->
                    {error, {rpc, [{reason, Rsn}, {pid, Pid}, {function, process_info}]}};
                _ ->
                    {error, {unknown, [{pid, Pid}, {function, process_info}]}}
            catch
                _:Rsn2 ->
                    {error, {rpc, [{reason, Rsn2}
                                  ,?stacktrace
                                  ,{pid, Pid}
                                  ,{function, process_info}]}}
            end
    end;
fetch_process_info(Name, Opts) when erlang:is_atom(Name) andalso erlang:is_list(Opts) ->
    case erlang:whereis(Name) of
        Pid when erlang:is_pid(Pid) ->
            fetch_process_info(Pid, Opts);
        _ -> % undefined | PortName
            {error, {unknown, [{name, Name}, {function, process_info}]}}
    end;
fetch_process_info(Pid, Opts) ->
    {error, {argument, [{pid, Pid}, {function, process_info}, {options, Opts}]}}.


process_initial_call(Find) ->
    case lists:keyfind('$initial_call', 1, Find(dictionary, [])) of
        {_, InitialCallMFA} ->
            {InitialCallMFA, binary(InitialCallMFA)};
        _ -> % {...} | false
            InitialCall = Find(initial_call, unknown),
            {InitialCall, binary(InitialCall)}
    end.

process_behavior({application_master, init, 4}) ->
    <<"application">>;
process_behavior({Mod, _, _}) when erlang:is_atom(Mod) ->
    case erlang:function_exported(Mod, behaviour_info, 1) of
        true ->
            binary(Mod);
        false ->
            Attrs =
                try
                    Mod:module_info(attributes)
                catch
                    _:_ ->
                        []
                end,
            case lists:keyfind(behaviour, 1, Attrs) of
                {_, [Behavior|_]} ->
                    binary(Behavior);
                _ ->
                    case lists:keyfind(behavior, 1, Attrs) of
                        {_, [Behavior|_]} ->
                            binary(Behavior);
                        _ ->
                            <<"unknown">>
                    end
            end
    end;
process_behavior(_) ->
    <<"unknown">>.


process_state(Pid, {Mod, _, _}, Behavior) ->
    CanGetState =
        try Mod:module_info(exports) of
            Exports ->
                case are_system_functions_exported(Exports) of
                    true ->
                        true;
                    _ ->
                        try (erlang:binary_to_atom(Behavior, utf8)):module_info(exports) of
                            BehaviorExports ->
                                case are_system_functions_exported(BehaviorExports) of
                                    true ->
                                        true;
                                    _ ->
                                        false
                                end
                        catch
                            _:_ ->
                                false
                        end
                end
        catch
            _:_ ->
                false
        end,
    if
        CanGetState ->
            try
                binary(sys:get_state(Pid, ?DEF_GET_STATE_TIMEOUT))
            catch
                _:_ ->
                    <<>>
            end;
        true ->
            <<>>
    end;
process_state(_, _, _) ->
    <<>>.


are_system_functions_exported(Exports) ->
    are_system_functions_exported(Exports, [{system_code_change, 4}
                                           ,{system_continue, 3}
                                           ,{system_get_state, 1}
                                           ,{system_replace_state, 2}
                                           ,{system_terminate, 4}]).


are_system_functions_exported(_, []) ->
    true;
are_system_functions_exported([FA|FAs], SFAs) ->
    case lists:member(FA, SFAs) of
        true ->
            are_system_functions_exported(FAs, lists:delete(FA, SFAs));
        _ -> % false
            are_system_functions_exported(FAs, SFAs)
    end;
are_system_functions_exported(_, _) -> % ([], [_|_])
    false.


binary(Arg) when erlang:is_atom(Arg) ->
    erlang:atom_to_binary(Arg, 'utf8');
binary(Arg) when erlang:is_integer(Arg) ->
    erlang:integer_to_binary(Arg);
binary({M, F, A}) when erlang:is_atom(M) andalso erlang:is_atom(F) andalso erlang:is_integer(A) ->
    <<(binary(M))/binary, ":", (binary(F))/binary, "/", (binary(A))/binary>>;
binary(Arg) when erlang:is_port(Arg) ->
    binary(erlang:port_to_list(Arg));
binary(Arg) when erlang:is_pid(Arg) ->
    binary(erlang:pid_to_list(Arg));
binary(Arg) when erlang:is_binary(Arg) ->
    Arg;
binary(Arg) ->
    case (erlang:is_list(Arg) andalso io_lib:deep_char_list(Arg)) of
        true ->
            erlang:list_to_binary(Arg);
        _ -> % false
            binary(io_lib:format("~tp", [Arg]))
    end.
