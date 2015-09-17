%% @author lihuachao
%% @doc @todo Add description to common_interval_timer.


-module(gb_util_interval_timer).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add_plan/2,cancel/1,work/0]).

-define(INTERVAL_TIMER_QUEUE,interval_timer_queue).
-define(INTERVAL_TIMER_REF,interval_timer_ref).


-include("gb_util_common.hrl").
%% ====================================================================
%% Internal functions
%% ====================================================================

%%接收到interval_timer_run这个消息就调用work()方法


%%加一个计时器
add_plan(Second,Fun) when is_function(Fun, 2)->
	Now = gb_util:now(),
	Inteval = Second - Now,
	case Inteval>0 of
		true->
			Queue = get_queue(),
			Ref = erlang:make_ref(),
			Add = {Second,Ref,Fun},
			set_queue(lists:keysort(1, [Add|Queue])),
			case get(?INTERVAL_TIMER_REF) of
				?undefined->
					TimerRef = erlang:send_after(Inteval*1000, self(), interval_timer_run),
					put(?INTERVAL_TIMER_REF,{Second,TimerRef});
				{S,TR}->
					case Second<S of
						true->
							erlang:cancel_timer(TR),
							TimerRef = erlang:send_after(Inteval*1000, self(), interval_timer_run),
							put(?INTERVAL_TIMER_REF,{Second,TimerRef});
						false->
							ok
					end
			end,
			Ref;
		false->
			Fun(Second,Now),
			no_add
	end.

cancel(no_add)->
	ok;
cancel(Ref)->
	case get_queue() of
		[]->
			ok;
		Queue->
			NewQueu = lists:keydelete(Ref, 2, Queue),
			set_queue(NewQueu)
	end.


work()->
	Now = gb_util:now(),
	List = get_queue(),
	{RunSList,NewList} = lists:partition(fun({S,_,_})->
												 S=<Now
										 end, List),
	set_queue(NewList),
	case NewList of
		[]->
			erase(?INTERVAL_TIMER_REF);
		[{S,_,_}|_]->
			Inteval = S - Now,
			TimerRef = erlang:send_after(Inteval*1000, self(), interval_timer_run),
			put(?INTERVAL_TIMER_REF,{S,TimerRef})
	end,
	lists:foreach(fun({S,_,Fun})->
						  Fun(S,Now)
				  end, RunSList).

get_queue()->
	case get(?INTERVAL_TIMER_QUEUE) of
		?undefined->
			[];
		L when is_list(L)->
			L;
		_->
			[]
	end.
set_queue(Queue)->
	put(?INTERVAL_TIMER_QUEUE,Queue).