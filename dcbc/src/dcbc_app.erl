%% @author Sergey Smirnov <sasmir@gmail.com>

-module(dcbc_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(dcbc).

start(_StartType, _StartArgs) ->
    case application:get_env(working_mode) of
        undefined -> dcbc_sup:start_link(master);
        {ok, WorkingMode} -> dcbc_sup:start_link(WorkingMode)
    end.

stop(_State) ->
    ok.
