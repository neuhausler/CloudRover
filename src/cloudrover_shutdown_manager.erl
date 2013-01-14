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
%%
%% There are two "Shutdown"-options available. Those options provide a way to shutdown an image or a server
%% when certain expected requests didn't get received.
%%
%% 1) Shutdown initiated when services doesn't receive initial configuration information (accessKey, GitHub repository)
%%    within a configurable time
%% 2) Shutdown initiated when service doesn't receive "keep-alive" signal within a configurable time.
%%

-module(cloudrover_shutdown_manager).
-behaviour(gen_fsm).

-record(state,
	{
		accesskey_set = false,
		giturl_set    = false,
		shutdown_cmd,
		bootstrap_timeout
	}
).

%% gen_fsm callbacks
-export([init/1, bootstrap/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%% public APIs
-export(
	[
		start/1,
        bootstrap_accesskey_is_set/0,
        bootstrap_giturl_is_set/0
	]).

start(Config) ->
	error_logger:info_report("shutdown_manager start called"),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Config, []).

bootstrap_accesskey_is_set() ->
    gen_fsm:send_event(?MODULE, accesskey_set).

bootstrap_giturl_is_set() ->
    gen_fsm:send_event(?MODULE, giturl_set).

%% gen_fsm callbacks

init(Config) ->
    error_logger:info_report("shutdown_manager init called"),
    {ok, ShutdownCmd} = get_option(bootstrap_shutdown_cmd, Config),
    {ok, TimeoutSecs} = get_option(bootstrap_timeout_secs, Config),
    State = #state{shutdown_cmd = ShutdownCmd, bootstrap_timeout = TimeoutSecs},
    erlang:send_after((State#state.bootstrap_timeout*1000), self(), check_for_shutdown),
    {ok, bootstrap, State}.

bootstrap(Event, State) ->
    case Event of
        accesskey_set ->
            StateNew = State#state{accesskey_set = true};
        giturl_set ->
            StateNew = State#state{giturl_set = true}
    end,
    {next_state, bootstrap, StateNew}.

handle_info(check_for_shutdown, _StateName, State) ->
    error_logger:info_report("in check_for_shutdown"),
    case {State#state.accesskey_set, State#state.giturl_set} of
        {true, true} ->
            {next_state, initalized, State};
        {_, _} ->
            error_logger:info_report("will be shutting down the system"),
            Output = cloudrover_utils:os_cmd(State#state.shutdown_cmd),
            io:format("Output: ~p~n", [Output]),
            {next_state, shutting_down, State}
    end;

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


%% gen_fsm default callbacks

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Utils
%%

get_option(Option, Options) ->
    case lists:keytake(Option, 1, Options) of
        false -> {ok, foo};
        {value, {Option, Value}, _NewOptions} -> {ok, Value}
    end.

