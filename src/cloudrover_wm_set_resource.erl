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

-module(cloudrover_wm_set_resource).
-export([init/1, allowed_methods/2, content_types_accepted/2, forbidden/2, from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(DICT_CMD  , "dict").

init([]) ->
%%	{{trace, "/tmp"}, dict:new()}.
	{ok, dict:new()}.


allowed_methods(ReqData, Context) ->
	{['PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
	{[{"application/json", from_json}], ReqData, Context}.

forbidden(ReqData, Context) ->
	cloudrover_wm_utils:forbidden(ReqData, Context).

from_json(ReqData, Context) ->
	{ok, AccessKey} = dict:find(accesskey, wrq:path_info(ReqData)),
	Domain = case wrq:path_tokens(ReqData) of
		[?DICT_CMD, Key] -> {dict, Key};
		_Other -> notFound
	end,
	case Domain of 
		notFound ->
			{false, ReqData, Context};
		{dict, KeyName} ->
			case mochijson:decode(wrq:req_body(ReqData)) of
				{struct, JSONData} ->
					case cloudrover_wm_utils:getValueFromJSON("value", JSONData) of
						{ok, Value} ->
%%							error_logger:info_msg("Set Key: ~p Value: ~p~n", [KeyName, Value]),
							cloudrover_controller:setKeyValue(AccessKey, KeyName, Value),
							{true, ReqData, Context};
						not_found ->
							{false, ReqData, Context}
					end;
				_Otherwise ->
					{false, ReqData, Context}
			end
	end.

%% Utils
