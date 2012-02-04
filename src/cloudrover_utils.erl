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
%%
%%    Some of the functions are adapted from rebar/rebar_utils
%%    https://github.com/basho/rebar/blob/master/src/rebar_utils.erl

-module(cloudrover_utils).
-export(
	[
		get_cwd/0,
		sh/2,
		os_cmd/1
	]).

%%
%% API Functions
%%

get_cwd() ->
	{ok, Dir} = file:get_cwd(),
	Dir.

os_cmd(Command) ->
	os:cmd(Command).

%%
%% Options = [Option] -- defaults to [use_stdout, abort_on_error]
%% Option = ErrorOption | OutputOption | {cd, string()} | {env, Env}
%% ErrorOption = return_on_error | abort_on_error | {abort_on_error, string()}
%% OutputOption = use_stdout | {use_stdout, bool()}
%% Env = [{string(), Val}]
%% Val = string() | false
%%
sh(Command0, Options0) ->
	io:format("sh info:\n\tcwd: ~p\n\tcmd: ~s\n\topts: ~p\n", [get_cwd(), Command0, Options0]),

	DefaultOptions = [use_stdout, abort_on_error],
	Options = [expand_sh_flag(V) || V <- proplists:compact(Options0 ++ DefaultOptions)],

%%    ErrorHandler = proplists:get_value(error_handler, Options),
	OutputHandler = proplists:get_value(output_handler, Options),

	PortSettings = proplists:get_all_values(port_settings, Options) ++ [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
	Port = open_port({spawn, Command0}, PortSettings),

	case sh_loop(Port, OutputHandler, []) of
		{ok, _Output} = Ok ->
			Ok;
		{error, {_Rc, _Output}=Err} ->
			io:format("Error: ~p\n", [Err]),
			notOk
%%            ErrorHandler(Command0, Err)
	end.



%%
%% Local Functions
%%

expand_sh_flag(return_on_error) ->
	{error_handler,
		fun(_Command, Err) ->
		{error, Err}
		end};

expand_sh_flag({abort_on_error, Message}) ->
	{error_handler,
		log_msg_and_abort(Message)};

expand_sh_flag(abort_on_error) ->
	{error_handler,
		fun log_and_abort/2};

expand_sh_flag(use_stdout) ->
	{output_handler,
		fun(Line, Acc) ->
			io:format("~s", [Line]),
			[Line | Acc]
		end};

expand_sh_flag({use_stdout, false}) ->
	{output_handler,
		fun(Line, Acc) ->
		[Line | Acc]
		end};

expand_sh_flag({cd, _CdArg} = Cd) ->
	{port_settings, Cd};

expand_sh_flag({env, _EnvArg} = Env) ->
	{port_settings, Env}.

log_msg_and_abort(Message) ->
	fun(_Command, {_Rc, _Output}) ->
		abort(Message, [])
	end.

log_and_abort(Command, {Rc, Output}) ->
	abort("~s failed with error: ~w and output:~n~s~n", [Command, Rc, Output]).

abort(String, Args) ->
	io:format(String, Args),
	halt(1).

sh_loop(Port, Fun, Acc) ->
	receive
		{Port, {data, {eol, Line}}} ->
			sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
		{Port, {data, {noeol, Line}}} ->
			sh_loop(Port, Fun, Fun(Line, Acc));
		{Port, {exit_status, 0}} ->
			{ok, lists:flatten(lists:reverse(Acc))};
		{Port, {exit_status, Rc}} ->
			{error, {Rc, lists:flatten(lists:reverse(Acc))}}
	end.
