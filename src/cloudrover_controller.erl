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

-module(cloudrover_controller).
-compile(export_all).

accessKeySet() ->
	cloudrover_stateserver:accessKeySet().

setAccessKey(AccessKey) ->
	cloudrover_stateserver:setAccessKey(AccessKey).

checkAccessKey(AccessKey) ->
	cloudrover_stateserver:correctAccessKey(AccessKey).

setGitSrc(AccessKey, GitSrcUrl) ->
	cloudrover_stateserver:setGitSrc(AccessKey, GitSrcUrl).

setGitSh(AccessKey, GitShUrl) ->
	cloudrover_stateserver:setGitSh(AccessKey, GitShUrl).

setKeyValue(AccessKey, Key, Value) ->
	cloudrover_stateserver:setKeyValue(AccessKey, Key, Value).

getValueForKey(Key) ->
	cloudrover_stateserver:getValueForKey(Key).

runScript(AccessKey, GroupName, ScriptName, Env) ->
	Response = case cloudrover_stateserver:correctAccessKey(AccessKey) of
		false ->
			accesskey_problem;
		true ->
			case updateGitShRepository(AccessKey) of
				ok ->
					runScriptInGroup(GroupName, ScriptName, Env);
				error ->
					gitError
			end
	end,
	Response.


%% Utils
%%

updateGitShRepository(AccessKey) ->
	WorkDir  = cloudrover_stateserver:getWorkDir(),
	ShDir    = cloudrover_stateserver:getShDir(),
	GitShURL = cloudrover_stateserver:getGitSh(AccessKey),

	{Command, Dir} = case filelib:is_dir(WorkDir ++ ShDir) of
		false -> {"git clone " ++ GitShURL, WorkDir};
		true  -> {"git pull", WorkDir ++ ShDir}
	end,
	case execCommand(Command, Dir, []) of
		error -> error;
		unexpected -> error;
		_Otherwise -> ok
	end.

runScriptInGroup(GroupName, ScriptName, Env) ->
	WorkDir = cloudrover_stateserver:getWorkDir(),
	ShDir   = cloudrover_stateserver:getShDir(),

	Dir = WorkDir ++ ShDir ++ "/" ++ GroupName,
	Command = "./" ++ ScriptName,
	execCommand(Command, Dir, Env).

execCommand(Command, Dir, Env) ->
	Result = cloudrover_utils:sh(Command, [{cd, Dir}, {use_stdout, false}, {env, Env}]),
	case Result of
		{ok, VsnString} ->
			error_logger:info_report(VsnString),
			Value = string:strip(VsnString, right, $\n),
			Value;
		notOk ->
			error;
		_Otherwise ->
			unexpected
	end.
