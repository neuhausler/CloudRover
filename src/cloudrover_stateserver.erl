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

-record(state,
	{
		accessKey = undefined,
		git = undefined,
		keyValueStore
	}
).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%% public APIs
-export([start/0, setAccessKey/1, getAccessKey/0, accessKeySet/0, correctAccessKey/1, setKeyValue/3, getValueForKey/1]).

start() ->
	error_logger:info_report("stateserver start called"),
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

setAccessKey(AccessKey)          -> gen_server:call(?MODULE, {setaccesskey, AccessKey}).
getAccessKey()                   -> gen_server:call(?MODULE,  getaccesskey).
accessKeySet()                   -> gen_server:call(?MODULE,  accesskeyset).
correctAccessKey(OtherAccessKey) -> gen_server:call(?MODULE, {correctaccesskey, OtherAccessKey}).

setKeyValue(AccessKey, Key, Value) -> gen_server:call(?MODULE, {setkeyvalue, {AccessKey, Key, Value}}).
getValueForKey(Key)                -> gen_server:call(?MODULE, {getvalueforkey, Key}).


%% gen_server callbacks

init([]) ->
	error_logger:info_report("stateserver init called"),
	{ok, #state{keyValueStore = dict:new()}}.

handle_call({setaccesskey, AccessKey}, _From, Context) ->
	error_logger:info_report("stateserver setaccesskey called"),
    case Context#state.accessKey of
        undefined ->
            NewContext = Context#state{accessKey= AccessKey},
            {reply, ok, NewContext};
        _OtherWise ->
    		{reply, already_set, Context}
    end;

handle_call(getaccesskey, _From, Context) ->
    Response = case Context#state.accessKey of
        undefined ->
            not_found;
        AccessKey ->
        	AccessKey
    end,
    {reply, Response, Context};

handle_call(accesskeyset, _From, Context) ->
	Response = case Context#state.accessKey of
		undefined  -> false;
		_OtherWise -> true
	end,
    {reply, Response, Context};

handle_call({correctaccesskey, OtherAccessKey}, _From, Context) ->
    Response = accessKeyOk(Context, OtherAccessKey),
    {reply, Response, Context};

handle_call({setkeyvalue, {AccessKey, Key, Value}}, _From, Context) ->
	CleanedKeyValueStore = dict:erase(Key, Context#state.keyValueStore),
    Response = case accessKeyOk(Context, AccessKey) of
		false ->
			NewContext = Context,
			accesskey_problem;
		true ->
			NewKeyValueStore = dict:append(Key, Value, CleanedKeyValueStore),
			NewContext= Context#state{keyValueStore = NewKeyValueStore },
			done
		end,
    {reply, Response, NewContext};

handle_call({getvalueforkey, Key}, _From, Context) ->
	Response = case dict:find(Key, Context#state.keyValueStore) of
		{ok, [Value | _Tail]} -> Value;
		error -> not_found
	end,
    {reply, Response, Context};
	
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


%% Utils

accessKeyOk(Context, OtherAccessKey) ->
	case Context#state.accessKey of
        undefined -> false;
        AccessKey -> AccessKey == OtherAccessKey
    end.
	