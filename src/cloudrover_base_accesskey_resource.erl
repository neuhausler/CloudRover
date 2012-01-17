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

-module(cloudrover_base_accesskey_resource).
-export([init/1, allowed_methods/2, content_types_accepted/2, forbidden/2, from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
%%	{{trace, "/tmp"}, dict:new()}.
	{ok, dict:new()}.


allowed_methods(ReqData, Context) ->
    {['PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

forbidden(ReqData, Context) ->
	case cloudrover_controller:accessKeySet() of
        true ->
			{true, ReqData, Context};
		false ->
			{false, ReqData, Context}
    end.

from_json(ReqData, Context) ->
	case mochijson:decode(wrq:req_body(ReqData)) of
		{struct, JSONData} ->
			case getValueFromJSON("accesskey", JSONData) of
				{ok, Value} ->
					cloudrover_controller:setAccessKey(Value),
					{true, ReqData, Context};
				not_found ->
					{false, ReqData, Context}
			end;
		_Otherwise ->
    		{false, ReqData, Context}
	end.

%% Utils

getValueFromJSON(Key, JSONData) ->
    case lists:keytake(Key, 1, JSONData) of
       false -> not_found;
       {value, {Key, Value}, _JSONData} -> {ok, Value}
    end.
