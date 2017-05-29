%% @author Sergey Smirnov <sasmir@gmail.com>

-module(dcbc_solver).

-export([start_link/4]).

-record(state, {solver_args, master, name, port = none, sol_incumbent = none,
                sol_status = port_terminated}).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

start_link(CbcPath, Name, MasterPid, Args) ->
    gen_server:start_link(?MODULE, [CbcPath, Name, MasterPid, Args], []).

init([CbcPath, Name, MasterPid, Args]) ->
    SolverArgs = proplists:get_value(solver_args, Args, []),
    Stub = proplists:get_value(stub, Args),
    BestVal = proplists:get_value(best_val, Args),
    gen_server:cast(self(), {do_init, CbcPath, Stub, BestVal}),
    monitor(process, MasterPid),
    {ok, #state{solver_args = SolverArgs, master = MasterPid, name = Name}}.

handle_cast({do_init, CbcPath, Stub, BestVal}, State) ->
    ok = file:write_file(stub_filename(), Stub),
    Args = make_port_args(BestVal, State#state.solver_args),
    io:format("Starting solver for ~p: ~s ~p~n", [State#state.name, CbcPath, Args]),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, CbcPath}, [{packet, 2}, nouse_stdio,
                                                   binary, {args, Args}]),
    {noreply, State#state{port = Port}};
handle_cast({update_best_val, Val}, #state{port = Port} = State) ->
    Port ! {self(), {command, <<1, Val/float>>}},
    {noreply, State}.

make_port_args(none, SolverArgs) ->
    [stub_filename(), "-q", "-p", "-o", log_filename(),
     "--" | SolverArgs];
make_port_args(BestVal, SolverArgs) ->
    [stub_filename(), "-q", "-p", "-o", log_filename(),
     "-b", float_to_list(BestVal), "--" | SolverArgs].

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) -> 
    {stop, master_down, State};
handle_info({'EXIT', _Port, _ExitReason}, State) ->
    Log = get_file_content(log_filename()),
    Sol = get_file_content(sol_filename()),
    gen_server:cast(State#state.master,
                    {solver_done, State#state.name, State#state.sol_status,
                     State#state.sol_incumbent, Sol, Log}),
    {stop, normal, State};
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    {noreply, handle_port_msg(decode(Data), State)}.

get_file_content(FileName) ->
    case file:read_file(FileName) of
        {ok, Content} -> Content;
        {error, _Reason} -> <<>>
    end.

handle_port_msg({best_val, Val}, State) ->
    gen_server:cast(State#state.master, {best_val, State#state.name, Val}),
    State;
handle_port_msg({done, Val, "optimal"}, State) ->
    State#state{sol_incumbent = Val, sol_status = optimal};
handle_port_msg({done, Val, "infeasible"}, State) ->
    State#state{sol_status = infeasible};
handle_port_msg({done, Val, "stopped"}, State) ->
    State#state{sol_status = stopped}.

decode(<<2, BestVal/float, Status/binary>>) ->
    {done, BestVal, binary_to_list(Status)};
decode(<<3, BestVal/float>>) ->
    {best_val, BestVal}.

terminate(_Reason, _State) ->
    file:delete(stub_filename()),
    file:delete(sol_filename()),
    file:delete(log_filename()),
    ok.

stub_filename() ->
    "stub" ++ pid_to_list(self()) ++ ".nl".

sol_filename() ->
    "stub" ++ pid_to_list(self()) ++ ".sol".

log_filename() ->
    "stub" ++ pid_to_list(self()) ++ ".log".
