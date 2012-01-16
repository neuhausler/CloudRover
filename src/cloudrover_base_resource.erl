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

-module(cloudrover_base_resource).
-export([init/1, allowed_methods/2, content_types_accepted/2, forbidden/2, resource_exists/2, to_html/2, from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(SET_CMD   , "set").
-define(GIT_CMD   , "git").
-define(SCRIPT_CMD, "script").
-define(DICT_CMD  , "dict").

init([]) ->
	{{trace, "/tmp"}, dict:new()}.
%%	{ok, dict:new()}.


allowed_methods(ReqData, State) ->
    {['GET', 'PUT'], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.


forbidden(ReqData, State) ->
	case dict:find(accesskey, wrq:path_info(ReqData)) of
		error ->
			{true, ReqData, State};
		{ok, AccessKey} ->
    		case cloudrover_controller:accessKeySet() of
        		false ->
					case AccessKey == ?SET_CMD of
						true ->
	            			{false, ReqData, State};
						false  ->
							{true, ReqData, State}
					end;
        		true ->
					case cloudrover_controller:checkAccessKey(AccessKey) of
						true ->
    						{false, ReqData, State};
						false ->
							{true, ReqData, State}
					end
			end
    end.

resource_exists(ReqData, State) ->
	Response = case wrq:path_tokens(ReqData) of
		[?GIT_CMD] -> ok;
		[?SCRIPT_CMD, _ScriptName] -> ok;
		[?DICT_CMD, _KeyValue] -> ok;
		_Other -> notOk
	end,
	case Response of 
		ok -> {true, ReqData, State};
		notOk -> {false, ReqData, State}
	end.


to_html(ReqData, State) ->
	{"<html><head><title>CloudRover</title></head><body>Base</body></html>", ReqData, State}.


from_json(ReqData, State) ->
	%% handle set AccessKey
	case dict:find(accesskey, wrq:path_info(ReqData)) of
		{ok, ?SET_CMD} ->
			case mochijson:decode(wrq:req_body(ReqData)) of
				{struct, JSONData} ->
					case getValueFromJSON("accesskey", JSONData) of
						{ok, Value} ->
							cloudrover_controller:setAccessKey(Value),
	    					{true, ReqData, State};
						not_found ->
							{false, ReqData, State}
					end;
				_Otherwise ->
	    			{false, ReqData, State}
			end;
		_Otherwise ->
    		{false, ReqData, State}
	end.

%% Utils

getValueFromJSON(Key, JSONData) ->
    case lists:keytake(Key, 1, JSONData) of
       false -> not_found;
       {value, {Key, Value}, _JSONData} -> {ok, Value}
    end.
