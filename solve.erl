#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -boot start_sasl %%%NAME%%%

main(Args) ->
    %{ok, State} = dcbc_master:init([Stubs]),
    %gen_server:enter_loop(dcbc_master, [], State).
    {SArgs, Stubs} = split_args(Args, []),
    pong = net_adm:ping('registry@irbis1.isa.ru'),
    global:sync(),
    {ok, Pid} = dcbc_master:start_link(SArgs, Stubs),
    monitor(process, Pid),
    receive
        {'DOWN', _Ref, process, Pid, _Reason} ->
            ok
    end.

split_args(["--" | Tail], SArgs) ->
    {SArgs, split_args2(Tail, [])};
split_args([Arg | Tail], SArgs) ->
    split_args(Tail, [Arg | SArgs]);
split_args([], SArgs) ->
    {[], SArgs}.

split_args2([Stub | Tail], Stubs) ->
    split_args2(Tail, [Stub | Stubs]);
split_args2([], Stubs) ->
    Stubs.
