%% @author lihuachao
%% @doc 测试机器人(没有走平台)


-module(robot).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([login/1,send/1]).

-include("all_proto.hrl").

%%连接游戏服TCP参数
-define(TCP_OPTIONS, [
					  binary,
					  {packet, 2},
					  {active, true},
					  {reuseaddr, true},
 					  {nodelay, true},
					  {keepalive, true},
					  {exit_on_close, true}]).
%%游戏服IP
-define(IP,"127.0.0.1").
%%游戏服端口
-define(PORT,12317).
%%游戏服编号
-define(SERVER_ID,2).
%%游戏服验证key(和游戏服setting配置的登陆验证key一样)
-define(KEY,"jiash0jsa&XS%KSGIKHIGH").

-define(TIMEOUT_SECOND,30*1000).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {socket,userid,isCreate=false}).


login(ID)->
	{ok,Pid} = gen_server:start(?MODULE, [ID], []),
	put(robot_pid,Pid),
	Pid.

send(Record)->
	Pid = get(robot_pid),
	case is_pid(Pid) andalso is_process_alive(Pid) of
		true->
			Pid ! {send_msg,Record},
			ok;
		false->
			{error,robot_not_exist}
	end.
	

init([ID]) ->
	process_flag(trap_exit,true),
	self() ! {init,ID},
	{ok,#state{}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _, Binary}, #state{socket=_Socket}=State) ->
	case catch proto:decode(Binary) of
		{'EXIT',Info} ->
			io:format("decode err:~p~n~w~n", [Info,Binary]);
		{_,RC}->
			io:format("==receive msg==>>~p~n",[RC]),
			{noreply, State}
	end;
handle_info({tcp_closed, Socket}, #state{socket=Socket} = State)->
	{stop, normal, State};
handle_info({send_msg,Record}, #state{socket=Socket,isCreate=IsCreate} = State)->
	case IsCreate of
		true->
			send_socket(Socket, Record),
			{noreply, State};
		false->
			io:format("can not send msg, not create account~n"),
			{noreply, State}
	end;
handle_info(send_heart, #state{socket=Socket} = State)->
	send_socket(Socket, #cs_account_heart{}),
	{noreply, State};
handle_info({init,ID},State)->
	case gen_tcp:connect(?IP,?PORT, ?TCP_OPTIONS, 5000) of
		{ok, Socket}->
			io:format("==============~n"),
			put(socket,Socket),
			timer:send_interval(?TIMEOUT_SECOND, send_heart),
			auth(Socket,lists:concat(["robot",ID]),ID,?SERVER_ID,gb_util:now()),
			{noreply, State#state{socket=Socket,userid=ID}};
		{error,Reason}->
			io:format("connect to server error:~p~n", [Reason]),
			{stop,normal,State}
	end;
handle_info(Info,State)->
	io:format("validate msg:~p~n",[Info]),
	{noreply,State}.


terminate(_Reason, _State) ->
	case _Reason of
	normal->
		ok;
	_->
		io:format("=====error robot===~p~n",[_Reason]),
		catch erlang:send(tt_server,error)
end,
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
%% 登录认证
auth(Socket,AccountName,UserID,ServerID,UnixTime) ->
	Ticket = gb_util:md5(lists:concat([AccountName,UserID,UnixTime,?KEY])),
	Record = #cs_account_login{
							   userID=UserID,
							   unixTime=UnixTime,
							   accountName=AccountName,
							   ticket=Ticket,
                               serverID=ServerID},
	send_socket(Socket, Record).

%% 获取角色列表（并创建角色）
%% ensure_role_exist(_,_,true)->
%% 	next;
%% ensure_role_exist(Socket,UserID,false)->
%% 	send_create_role(Socket,UserID).
%%
%% send_create_role(Socket,UserID)->
%% 	RoleName = lists:concat(["robot",UserID]),
%% 	send_socket(Socket, #cs_account_create{modelID=1,roleName=RoleName}).

send_socket(Socket,Record)->
	case catch proto:encode(Record) of
		{'EXIT',_Info} ->
			io:format("encode err:~p~n", [Record]);
		IoList ->
			gen_tcp:send(Socket, IoList)
	end.