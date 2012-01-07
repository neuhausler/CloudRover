%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the cloudrover application.

-module(cloudrover_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cloudrover.
start(_Type, _StartArgs) ->
    cloudrover_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cloudrover.
stop(_State) ->
    ok.
