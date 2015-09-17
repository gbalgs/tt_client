%% @author lihuachao
%% @doc 基本的性能测试函数


-module(gb_util_tc).

-export([run/2,run_process/2]).

%% ====================================================================
%% API functions
%% ====================================================================

	
run_process(N,F) ->
	spawn(fun() -> run(N,F) end).

run(N, F) when is_function(F,0) ->
	erlang:statistics(runtime),
	erlang:statistics(wall_clock),
	for0(1,N,F),
	{_, Time1} = erlang:statistics(runtime),
	{_, Time2} = erlang:statistics(wall_clock),
	U1 = Time1 * 1000 / N,
	U2 = Time2 * 1000 / N,
	io:format("apply fun time=~p (~p) microseconds~n",[U1,U2]);
run(N, F) when is_function(F,1) ->
	erlang:statistics(runtime),
	erlang:statistics(wall_clock),
	for1(1,N,F),
	{_, Time1} = erlang:statistics(runtime),
	{_, Time2} = erlang:statistics(wall_clock),
	U1 = Time1 * 1000 / N,
	U2 = Time2 * 1000 / N,
	io:format("apply fun time=~p (~p) microseconds~n",[U1,U2]).

for0(N,N,F) ->
	F();
for0(M,N,F) ->
	F(),
	for0(M+1,N,F).

for1(N,N,F) ->
	F(N);
for1(M,N,F) ->
	F(M),
	for1(M+1,N,F).

%% ====================================================================
%% Internal functions
%% ====================================================================

