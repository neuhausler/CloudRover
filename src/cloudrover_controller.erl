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

setRSAKey(AccessKey, RSAKey) ->
	cloudrover_stateserver:setRSAKey(AccessKey, RSAKey).

setGitSrc(AccessKey, GitSrcUrl) ->
	cloudrover_stateserver:setGitSrc(AccessKey, GitSrcUrl).

setGitSh(AccessKey, GitShUrl) ->
	cloudrover_stateserver:setGitSh(AccessKey, GitShUrl).

setKeyValue(AccessKey, Key, Value) ->
	cloudrover_stateserver:setKeyValue(AccessKey, Key, Value).

getValueForKey(Key) ->
	cloudrover_stateserver:getValueForKey(Key).

runScript(AccessKey, ScriptName) ->
	cloudrover_stateserver:runScript(AccessKey, ScriptName).
