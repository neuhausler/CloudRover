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

-module(cloudrover_base_utils).
-export(
	[
		forbidden/2,
		getValueFromJSON/2
	]).


forbidden(ReqData, Context) ->
	case dict:find(accesskey, wrq:path_info(ReqData)) of
		error ->
			{true, ReqData, Context};
		{ok, AccessKey} ->
			case cloudrover_controller:accessKeySet() of
				false ->
					{true, ReqData, Context};
				true ->
					case cloudrover_controller:checkAccessKey(AccessKey) of
						true ->
							{false, ReqData, Context};
						false ->
							{true, ReqData, Context}
					end
			end
	end.


getValueFromJSON(Key, JSONData) ->
	case lists:keytake(Key, 1, JSONData) of
		false -> not_found;
		{value, {Key, Value}, _JSONData} -> {ok, Value}
	end.
