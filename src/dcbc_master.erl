-module(dcbc_master).

-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {best_val = none, best_sol = {none, <<>>}, stubs, start_ts, solver_args}).

-record(subp, {path, pid = none, status = none, slave_pid = none, ref = none}).

start_link(SolverArgs, FileNames) ->
    gen_server:start_link(?MODULE, [SolverArgs, FileNames], []).

init([SolverArgs, FileNames]) ->
    gen_server:cast(self(), do_init),
    {Pairs, _} = lists:mapfoldl(fun (Path, Id) -> {{Id, #subp{path = Path}}, Id + 1} end, 1, FileNames),
    Stubs = dict:from_list(Pairs),
    {ok, #state{stubs = Stubs, solver_args = SolverArgs, start_ts = now()}}.

handle_cast(do_init, State) ->
    {ok, Slaves} = dcbc_registry:lookup(slave),
    {noreply, initial_submit({State, Slaves})};
handle_cast({best_val, Name, Val}, State) ->
    {noreply, update_best(Name, Val, none, State)};
handle_cast({solver_done, Name, Status, Val, Sol, Log}, State0) ->
    io:format("~.3f ~p ~p, result=~p~n", [seconds_elapsed(State0#state.start_ts), Name, Status, Val]),
    ok = file:write_file(integer_to_list(Name) ++ ".log", Log),
    State = update_best(Name, Val, Sol, State0),
    Subp = dict:fetch(Name, State#state.stubs),
    demonitor(Subp#subp.ref, [flush]),
    Subp1 = Subp#subp{status = Status, ref = none, pid = none, slave_pid = none},
    State1 = State#state{stubs = dict:store(Name, Subp1, State#state.stubs)},
    {noreply, submit_next_problem(State1, Subp#subp.slave_pid)}.

initial_submit({State, []}) ->
    State;
initial_submit({State, Slaves}) ->
    case find_next_problem(State#state.stubs) of
        {none, _} ->
            State;
        {Name, _} ->
            initial_submit(submit_problem(Name, {State, Slaves}))
    end.

find_next_problem(Stubs) ->
    dict:fold(
      fun (K, #subp{status = none}, {none, HasRunning}) -> {K, HasRunning};
          (_K, #subp{status = running}, {Next, _}) -> {Next, true};
          (_K, _V, A) -> A end, {none, false}, Stubs).

submit_problem(_Name, {State, []}) ->
    {State, []};
submit_problem(Name, {State, [SlavePid | Tail] = Slaves}) ->
    Subp = dict:fetch(Name, State#state.stubs),
    {ok, Data} = file:read_file(Subp#subp.path),
    case dcbc_slave:start_solver(SlavePid, Name,
                                 [{best_val, State#state.best_val}, {stub, Data},
                                  {solver_args, State#state.solver_args}]) of
        {ok, SolverPid} ->
            io:format("~.3f ~p started: ~s~n", [seconds_elapsed(State#state.start_ts), Name, Subp#subp.path]),
            Ref = monitor(process, SolverPid),
            Subp1 = Subp#subp{pid = SolverPid, slave_pid = SlavePid, ref = Ref, status = running},
            {State#state{stubs = dict:store(Name, Subp1, State#state.stubs)}, Slaves};
        {error, _} ->
            io:format("Failed to start ~s as ~p~n", [Subp#subp.path, Name]),
            submit_problem(Name, {State, Tail})
    end.

update_best(Name, Val, Sol, #state{best_val = BV1, best_sol = {BV2, _BS}} = State) ->
    case {is_better_than(Val, BV1, minimize), is_better_than(Val, BV2, minimize), Sol} of %% NOTE: minimization
        {true, true, Sol} when Sol =/= none ->
            io:format("~.3f ~p best_val and best_sol: ~p~n", [seconds_elapsed(State#state.start_ts), Name, Val]),
            broadcast_best_val(Name, Val, State#state.stubs),
            State#state{best_val = Val, best_sol = {Val, Sol}};
        {true, _, Sol} ->
            io:format("~.3f ~p best_val: ~p~n", [seconds_elapsed(State#state.start_ts), Name, Val]),
            broadcast_best_val(Name, Val, State#state.stubs),
            State#state{best_val = Val};
        {false, true, Sol} when Sol =/= none ->
            io:format("~.3f ~p best_sol: ~p~n", [seconds_elapsed(State#state.start_ts), Name, Val]),
            State#state{best_sol = {Val, Sol}};
        _ ->
            State
    end.

seconds_elapsed({_, S0, Mu0}) ->
    {_, S, Mu} = now(),
    S - S0 + (Mu - Mu0) / 1000000.

is_better_than(none, _, _) ->
    false;
is_better_than(_New, none, _) ->
    true;
is_better_than(New, Current, minimize) when New < Current ->
    true;
is_better_than(_New, _Current, minimize) ->
    false;
is_better_than(New, Current, maximize) when New > Current ->
    true;
is_better_than(_New, _Current, maximize) ->
    false.

broadcast_best_val(Name, Val, Stubs) ->
    dict:map(fun (K, #subp{pid = Pid}) when (K =/= Name) and (Pid =/= none) ->
                     gen_server:cast(Pid, {update_best_val, Val});
                 (_,_) ->
                     ok
             end, Stubs).

handle_info({'DOWN', _Ref, process, _Pid, normal}, State) ->
    {noreply, State};
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    {Name, Subp} = dict:fold(
                     fun (Name, S = #subp{ref = R}, none) when R =:= Ref -> {Name, S};
                         (_, _, Acc) -> Acc end,
                     none, State#state.stubs),
    io:format("Solver crashed for subproblem ~p: ~p~n", [Name, Reason]),
    Subp1 = Subp#subp{pid = none, ref = none, slave_pid = none, status = crashed},
    State1 = State#state{stubs = dict:store(Name, Subp1, State#state.stubs)},
    {noreply, submit_next_problem(State1, Subp#subp.slave_pid)}.

terminate(_Reason, State) ->
    dict:fold(fun (K, _V, _A) -> file:delete(integer_to_list(K) ++ ".log") end,
              ok, State#state.stubs).

submit_next_problem(State, SlavePid) ->
    case find_next_problem(State#state.stubs) of
        {none, false} ->
            {BestVal, BestSol} = State#state.best_sol,
            ok = file:write_file("solution.sol", BestSol),
            io:format("~.3f Done, best value = ~p, best value in solution = ~p~nsolution written to solution.sol~n",
                      [seconds_elapsed(State#state.start_ts), State#state.best_val, BestVal]),            
            State;
        {none, true} ->
            State;
        {Name, _} ->
            {State1, _Slaves} = submit_problem(Name, {State, [SlavePid]}),
            State1
    end.
