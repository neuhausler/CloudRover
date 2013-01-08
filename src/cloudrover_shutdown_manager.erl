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

-module(cloudrover_shutdown_manager).
-behaviour(gen_fsm).

-record(state,
	{
		accessKey = undefined,
		workDir   = undefined,
		gitSrc    = undefined,
		gitSh     = undefined,
		gitShDir  = undefined,
		keyValueStore
	}
).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%% public APIs
-export(
	[
		start/1
	]).

start(Config) ->
	error_logger:info_report("shutdown_manager start called"),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Config, []).



%% gen_fsm callbacks

init(Config) ->
    error_logger:info_report("shutdown_manager init called"),
    {ok, state_name, nobody}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(Event, From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

