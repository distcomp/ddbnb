-module(dcbc_slave_sup).

-export([start_link/1]).

-behaviour(supervisor).
-export([init/1]).

-define(CHILD(I, A), {I, {I, start_link, A}, temporary, 5000, worker, [I]}).

start_link(CbcPath) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [CbcPath]).

init([CbcPath]) ->
    {ok, {{simple_one_for_one, 2, 1}, [?CHILD(dcbc_solver, [CbcPath])]}}.
