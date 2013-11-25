%% @author Sergey Smirnov <sasmir@gmail.com>

-module(dcbc_registry).

-export([start_link/0, register/2, unregister/1, lookup/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2]).

-record(entry, {type, pid, ref}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

register(Pid, Type) when is_pid(Pid)->
    gen_server:call({global, ?MODULE}, {register, Pid, Type}).

unregister(Pid) when is_pid(Pid) ->
    gen_server:call({global, ?MODULE}, {unregister, Pid}).

lookup(Type) ->
    gen_server:call({global, ?MODULE}, {lookup, Type}).

handle_call({register, Pid, Type}, _From, State) ->
    Ref = monitor(process, Pid),
    {reply, ok, [#entry{type = Type, pid = Pid, ref = Ref} | State]};
handle_call({unregister, Pid}, _From, State) ->
    Entry = lists:keyfind(Pid, #entry.pid, State),
    demonitor(Entry#entry.ref, [flush]),
    {reply, ok, lists:delete(Entry, State)};
handle_call({lookup, Type}, _From, State) ->
    Pids = lists:foldl(
      fun (#entry{pid = P, type = T}, Acc) when T =:= Type ->
              [P | Acc];
          (_, Acc) ->
              Acc end,
     [], State),
    {reply, {ok, Pids}, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    Entry = lists:keyfind(Ref, #entry.ref, State),
    {noreply, lists:delete(Entry, State)}.
