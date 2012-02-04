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

-module(cloudrover_base_get_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(DICT_CMD, "dict").

init([]) ->
%%	{{trace, "/tmp"}, dict:new()}.
	{ok, dict:new()}.

allowed_methods(ReqData, Context) ->
	{['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
	Domain = case wrq:path_tokens(ReqData) of
		[?DICT_CMD, Key] -> {dict, Key};
		_Other -> not_found
	end,
	case Domain of 
		not_found ->
			{"{}", ReqData, Context};
		{dict, KeyName} ->
			case cloudrover_controller:getValueForKey(KeyName) of
				not_found ->
					{"{}", ReqData, Context};
				Value ->
					{mochijson:encode({struct, [{KeyName, Value}]}), ReqData, Context}
			end
	end.

