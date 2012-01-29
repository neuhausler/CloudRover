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

-module(cloudrover_base_sh_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, forbidden/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").


init([]) ->
%%	{{trace, "/tmp"}, dict:new()}.
	{ok, dict:new()}.


allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

forbidden(ReqData, Context) ->
	cloudrover_base_utils:forbidden(ReqData, Context).

to_json(ReqData, Context) ->
	{ok, AccessKey} = dict:find(accesskey, wrq:path_info(ReqData)),
	case mochijson:decode(wrq:req_body(ReqData)) of
		{struct, JSONData} ->
			case resolveScriptAndGroup(JSONData) of
				not_found ->
					{"{}", ReqData, Context};
				{GroupName, ScriptName} ->
					Value = cloudrover_controller:runScript(AccessKey, GroupName, ScriptName),
					{mochijson:encode({struct, [{"Output", Value}]}), ReqData, Context}
			end;
		_Otherwise ->
	    	{"{}", ReqData, Context}
	end.


%% Utils
%%

resolveScriptAndGroup(JSONData) -> 
	GroupName = case cloudrover_base_utils:getValueFromJSON("group",  JSONData) of
		not_found -> not_found;
		{ok, Group} -> Group
	end,
	ScriptName = case cloudrover_base_utils:getValueFromJSON("script", JSONData) of
		not_found -> not_found;
		{ok, Script} -> Script
	end,
	ScriptAndGroup = {GroupName, ScriptName},
	case(ScriptAndGroup) of
		{not_found, _} -> not_found;
		{_, not_found} -> not_found;
		{GroupName, ScriptName} -> {GroupName, ScriptName}
	end.
