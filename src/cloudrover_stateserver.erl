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

-module(cloudrover_stateserver).
-behaviour(gen_server).

-define(KEY, "key").

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%% public APIs
-export([start/0, setAccessKey/1, getAccessKey/0, accessKeySet/0, correctAccessKey/1]).

start() ->
	error_logger:info_report("stateserver start called"),
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

setAccessKey(AccessKey)          -> gen_server:call(?MODULE, {setaccesskey, AccessKey}).
getAccessKey()                   -> gen_server:call(?MODULE, getaccesskey).
accessKeySet()                   -> gen_server:call(?MODULE, accesskeyset).
correctAccessKey(OtherAccessKey) -> gen_server:call(?MODULE, {correctaccesskey, OtherAccessKey}).


%% gen_server callbacks

init([]) ->
	error_logger:info_report("stateserver init called"),
	{ok, dict:new()}.


handle_call({setaccesskey, AccessKey}, _From, State) ->
	error_logger:info_report("stateserver setaccesskey called"),
    case dict:is_key(?KEY, State) of
        true ->
    		{reply, already_set, State};
        false ->
            NewState= dict:append(?KEY, AccessKey, State),
            {reply, ok, NewState}
    end;

handle_call(getaccesskey, _From, State) ->
    Response = case dict:is_key(?KEY, State) of
        true ->
        	{ok, [Value | _Tail]} = dict:find(?KEY, State),
        	Value;
        false ->
            not_found
    end,
    {reply, Response, State};

handle_call(accesskeyset, _From, State) ->
    Response = dict:is_key(?KEY, State),
    {reply, Response, State};

handle_call({correctaccesskey, OtherAccessKey}, _From, State) ->
    Response = case dict:is_key(?KEY, State) of
        true ->
        	{ok, [AccessKey | _Tail]} = dict:find(?KEY, State),
        	AccessKey == OtherAccessKey;
        false ->
            false
    end,
    {reply, Response, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

terminate(_Reason, _State) ->
	error_logger:info_report("stateserver terminate called"),
	ok.

%% default implementation of some other callbacks
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
	