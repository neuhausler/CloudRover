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

-module(cloudrover_base_rsa_resource).
-export([init/1, allowed_methods/2, forbidden/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
%%	{{trace, "/tmp"}, dict:new()}.
	{ok, dict:new()}.


allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

forbidden(ReqData, Context) ->
	cloudrover_base_utils:forbidden(ReqData, Context).

process_post(ReqData, Context) ->
	{ok, AccessKey} = dict:find(accesskey, wrq:path_info(ReqData)),
    RSAKey = wrq:req_body(ReqData),
    case cloudrover_controller:setRSAKey(AccessKey, RSAKey) of
        ok ->
            {true, ReqData, Context};
        {error, Err} ->
            {Err, ReqData, Context}
    end.    


%% Utils
