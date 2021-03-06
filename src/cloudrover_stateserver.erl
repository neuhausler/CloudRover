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
		workDir   = undefined,
		gitSrc    = undefined,
		gitSh     = undefined,
		gitShDir  = undefined,
		keyValueStore
	}
).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%% public APIs
-export(
[
    start/1,
    setAccessKey/1,
    accessKeySet/0,
    correctAccessKey/1,
    setGitSrc/2,
    gitSrcSet/0,
    setGitSh/2,
    getGitSh/1,
    gitShSet/0,
    setKeyValue/3,
    getValueForKey/1,
    getWorkDir/0,
    getShDir/0
]).

start(Config) ->
	error_logger:info_report("stateserver start called"),
	gen_server:start_link({local,?MODULE}, ?MODULE, Config, []).


setAccessKey(AccessKey)          -> gen_server:call(?MODULE, {setaccesskey, AccessKey}).
accessKeySet()                   -> gen_server:call(?MODULE,  accesskeyset).
correctAccessKey(OtherAccessKey) -> gen_server:call(?MODULE, {correctaccesskey, OtherAccessKey}).

setGitSrc(AccessKey, GitSrcUrl) -> gen_server:call(?MODULE, {setgitsrc, {AccessKey, GitSrcUrl}}).
gitSrcSet()                     -> gen_server:call(?MODULE,  gitsrcset).

setGitSh(AccessKey, GitShUrl) -> gen_server:call(?MODULE, {setgitsh, {AccessKey, GitShUrl}}).
getGitSh(AccessKey)           -> gen_server:call(?MODULE, {getgitsh, AccessKey}).
gitShSet()                    -> gen_server:call(?MODULE,  gitshset).

setKeyValue(AccessKey, Key, Value) -> gen_server:call(?MODULE, {setkeyvalue, {AccessKey, Key, Value}}).
getValueForKey(Key)                -> gen_server:call(?MODULE, {getvalueforkey, Key}).

getWorkDir() -> gen_server:call(?MODULE,  getworkdir).
getShDir()   -> gen_server:call(?MODULE,  getshdir).



%% gen_server callbacks

init(Config) ->
	error_logger:info_report("stateserver init called"),
	{ok, WorkDir} = get_option(work_dir, Config),
	State = #state{keyValueStore = dict:new(), workDir = WorkDir},
	{ok, State}.



handle_call({setaccesskey, AccessKey}, _From, Context) ->
	error_logger:info_report("stateserver setaccesskey called"),
	case Context#state.accessKey of
		undefined ->
			NewContext = Context#state{accessKey= AccessKey},
            cloudrover_shutdown_manager:bootstrap_accesskey_is_set(),
			{reply, ok, NewContext};
		_OtherWise ->
			{reply, already_set, Context}
	end;

handle_call(accesskeyset, _From, Context) ->
	Response = case Context#state.accessKey of
		undefined  -> false;
		_OtherWise -> true
	end,
	{reply, Response, Context};

handle_call({correctaccesskey, OtherAccessKey}, _From, Context) ->
	Response = accessKeyOk(Context, OtherAccessKey),
	{reply, Response, Context};




handle_call({setgitsrc, {AccessKey, GitSrcUrl}}, _From, Context) ->
	error_logger:info_report("stateserver setgitsrc called"),
	Response = case accessKeyOk(Context, AccessKey) of
		false ->
			NewContext = Context,
			accesskey_problem;
		true ->
			case Context#state.gitSrc of
				undefined ->
					NewContext = Context#state{gitSrc= GitSrcUrl},
					ok;
				_OtherWise ->
					NewContext = Context,
					already_set
			end
	end,
	{reply, Response, NewContext};

handle_call(gitsrcset, _From, Context) ->
	Response = case Context#state.gitSrc of
		undefined  -> false;
		_OtherWise -> true
	end,
	{reply, Response, Context};




handle_call({setgitsh, {AccessKey, GitShUrl}}, _From, Context) ->
	error_logger:info_report("stateserver setgitsh called"),
	Response = case accessKeyOk(Context, AccessKey) of
		false ->
			NewContext = Context,
			accesskey_problem;
		true ->
			case Context#state.gitSh of
				undefined ->
					GitShDir = resolveGitDirFromURL(GitShUrl),
					NewContext = Context#state{gitSh= GitShUrl, gitShDir= GitShDir},
                    cloudrover_shutdown_manager:bootstrap_giturl_is_set(),
					ok;
				_OtherWise ->
					NewContext = Context,
					already_set
			end
	end,
	{reply, Response, NewContext};

handle_call({getgitsh, AccessKey}, _From, Context ) ->
	Response = case accessKeyOk(Context, AccessKey) of
		false -> accesskey_problem;
		true  -> Context#state.gitSh
	end,
	{reply, Response, Context};
	

handle_call(gitshset, _From, Context) ->
	Response = case Context#state.gitSh of
		undefined  -> false;
		_OtherWise -> true
	end,
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


handle_call(getworkdir, _From, Context) ->
	Response = case Context#state.workDir of
		undefined -> not_found;
		WorkDir -> WorkDir
	end,
	{reply, Response, Context};

handle_call(getshdir, _From, Context) ->
	Response = case Context#state.gitShDir of
		undefined -> not_found;
		ShDir -> ShDir
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
%%

get_option(Option, Options) ->
	case lists:keytake(Option, 1, Options) of
		false -> {ok, foo};
		{value, {Option, Value}, _NewOptions} -> {ok, Value}
	end.

accessKeyOk(Context, OtherAccessKey) ->
	case Context#state.accessKey of
		undefined -> false;
		AccessKey -> AccessKey == OtherAccessKey
	end.

resolveGitDirFromURL(URL) ->
	DirPart = string:substr(URL, string:rstr(URL, "/")+1),
	string:substr(DirPart, 1, string:len(DirPart)-4).
