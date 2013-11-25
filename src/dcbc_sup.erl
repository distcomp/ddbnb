%% @author Sergey Smirnov <sasmir@gmail.com>

-module(dcbc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CHILD_TMP(I, Type, Args), {I, {I, start_link, Args}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(registry) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [registry]);
start_link(Type) ->
    {ok, Node} = application:get_env(registry_node),
    pong = net_adm:ping(Node),
    global:sync(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Type]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([master]) ->
    {ok, Files} = application:get_env(files),
    {ok, Args} = application:get_env(args),
    {ok, {{one_for_one, 0, 1}, [?CHILD_TMP(dcbc_master, worker, [Args, Files])]}};
init([slave]) ->
    {ok, CbcPath} = application:get_env(cbc_path),
    {ok, Ncpu} = application:get_env(num_cpu),
    {ok, {{one_for_one, 5, 10}, [?CHILD(dcbc_slave, worker, [Ncpu]),
                                 ?CHILD(dcbc_slave_sup, supervisor, [CbcPath])]}};
init([registry]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(dcbc_registry, worker, [])]}}.
