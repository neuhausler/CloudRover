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

-module(cloudrover_wm_default_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
	{ok, nostate}.

to_html(ReqData, Context) ->
	{"<html><head><title>CloudRover</title></head><body>Hello, Earth!</body></html>", ReqData, Context}.
