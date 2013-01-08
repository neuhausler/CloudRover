%% @author Marcel Neuhausler
%% @copyright 2012 Marcel Neuhausler
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Supervisor for the cloudrover application.

-module(cloudrover_sup).
-author('Marcel Neuhausler').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
	{ok, {_, Specs}} = init([]),

	Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
	New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
	Kill = sets:subtract(Old, New),

	sets:fold(
		fun (Id, ok) ->
			supervisor:terminate_child(?MODULE, Id),
			supervisor:delete_child(?MODULE, Id),
			ok
		end,
		ok,
		Kill),

	[supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
	ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	{ok, Dispatch} = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "dispatch.conf"])),
	{ok, Config}   = file:consult(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "cloudrover.conf"])),
	{ok, Port}     = get_option(port, Config),
	{ok, LogDir}   = get_option(log_dir, Config),
	{ok, WorkDir}  = get_option(work_dir, Config),
	{ok, PidFile}  = get_option(pid_file, Config),

	filelib:ensure_dir(LogDir),
	filelib:ensure_dir(WorkDir),

	ok= file:write_file(PidFile, os:getpid()),

	WebConfig = [
		{ip, "0.0.0.0"},
		{port, Port},
		{log_dir, LogDir},
		{dispatch, Dispatch},
		{error_handler, cloudrover_wm_error_handler}
	],
	
	StateServerConfig = [
		{work_dir, WorkDir}
	],

    ShutdownManagerConfig = [
        {work_dir, WorkDir}
    ],

    StateServer =
	{
		cloudrover_stateserver,
		{cloudrover_stateserver, start, [StateServerConfig]},
		permanent,
		5000,
		worker,
		[]
	},

    ShutdownManager =
    {
        cloudrover_shutdown_manager,
        {cloudrover_shutdown_manager, start, [ShutdownManagerConfig]},
        permanent,
        5000,
        worker,
        []
    },

    WebServer =
	{
		webmachine_mochiweb,
		{webmachine_mochiweb, start, [WebConfig]},
		permanent,
		5000,
		worker,
		[mochiweb_socket_server, cloudrover_stateserver]
	},

	Processes = [WebServer, StateServer, ShutdownManager],
	{ok, { {one_for_all, 10, 10}, Processes} }.



%% Utils

get_option(Option, Options) ->
	case lists:keytake(Option, 1, Options) of
		false -> {ok, foo};
		{value, {Option, Value}, _NewOptions} -> {ok, Value}
	end.

