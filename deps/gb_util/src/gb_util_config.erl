%% @author lihuachao
%% @doc @todo Add description to common_config.


-module(gb_util_config).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 reload_all/1,
		 reload/2,
		 do_load_gen_src/6
		 ]).


-include("gb_util_common.hrl").
%% ====================================================================
%% Internal functions
%% ====================================================================


%%加载一个配置文件
reload(ConfigName,ConfigList) when is_atom(ConfigName)->
	case lists:keyfind(ConfigName, 1, ConfigList) of
		false->
			{error,not_found};
		ConfRec->
			reload2(ConfRec),
			ok
	end.

%%加载全部配置文件
reload_all(ConfigList)->
	lists:foreach(fun(ConfRec)->
						  reload2(ConfRec)
				  end, ConfigList),
	ok.

reload2({ConfigModuleName,FilePath,FileType}) ->
	reload2({ConfigModuleName,FilePath,FileType,nofun});

reload2({ConfigModuleName,FilePath,FileType,Fun}) ->
	reload2({ConfigModuleName,FilePath,FileType,Fun,set});

reload2({ConfigModuleName,FilePath,_,_,_}=ConfRec) ->
 	try
			do_load_config(ConfRec)
%% 		{ok, Code} = do_load_config(ConfRec),
%% 		file:write_file(lists:concat([get_server_dir(),"ebin/config/", ConfigModuleName, ".beam"]), Code, [write, binary])
	catch
		Err:Reason->
			io:format("Reason=~p,ConfigModuleName=~p,FilePath=~p",[Reason,ConfigModuleName,FilePath]),
			throw({Err,Reason})
	end.


do_load_config({ConfigModuleName,FilePath,record_consult,Fun,Type}) ->
	{ok,RecList} = file:consult(FilePath),
	KeyValues = [ begin
					  Key = element(2,Rec), {Key,Rec}
				  end || Rec<- RecList ],
	ValList = RecList,
	do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList,Fun,record_consult);

do_load_config({ConfigModuleName,FilePath,record_list,Fun,Type}) ->
	{ok,[RecList]} = file:consult(FilePath),
	KeyValues = [ begin
					  Key = element(2,Rec), {Key,Rec}
				  end || Rec<- RecList ],
	ValList = RecList,
	do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList,Fun,record_list);

do_load_config({ConfigModuleName,FilePath,key_value_consult, Fun, Type})->
	{ok,RecList} = file:consult(FilePath),
	KeyValues = 
		lists:map(fun({Key,Value})->
						 {Key,Value};
					({_,Key,Value})->
						{Key,Value}
				  end, RecList),
	do_load_gen_src(ConfigModuleName,Type,KeyValues,KeyValues,Fun,key_value_consult);

do_load_config({ConfigModuleName,FilePath,key_value_list, Fun, Type})->
	{ok,[RecList]} = file:consult(FilePath),
	KeyValues = 
		lists:map(fun({Key,Value})->
						 {Key,Value};
					({_,Key,Value})->
						{Key,Value}
				  end, RecList),
	do_load_gen_src(ConfigModuleName,Type,KeyValues,KeyValues,Fun,key_value_list).

%%@doc 生成源代码，执行编译并load
do_load_gen_src(ConfigModuleName,Type,KeyValues,ValList,Fun,FileType)->
	try
		Src = gen_src(ConfigModuleName,Type,KeyValues,ValList,Fun,FileType),
		{Mod, Code} = gb_util_dynamic_compile:from_string(Src),
		code:load_binary(Mod, gb_util:to_list(ConfigModuleName) ++ ".erl", Code),
		{ok, Code}
	catch
		Type:Reason -> 
			Trace = erlang:get_stacktrace(), string:substr(erlang:get_stacktrace(), 1,200),
			io:format("Error compiling ~p: Type=~w,Reason=~w,Trace=~w,~n", [ConfigModuleName, Type, Reason,Trace ]),
			throw({Type,Reason})
	end.


gen_src(ConfModuleName,Type,KeyValues,ValList,Fun,FileType) ->
    KeyValues2 =
        if Type =:= bag ->
                lists:foldl(fun({K, V}, Acc) ->
                                    case lists:keyfind(K, 1, Acc) of
                                        false ->
                                            [{K, [V]}|Acc];
                                        {K, VO} ->
                                            [{K, [V|VO]}|lists:keydelete(K, 1, Acc)]
                                    end
                            end, [], KeyValues);
           true ->
                KeyValues
        end,
	{KeyValues3,NewValList} =
		case is_function(Fun, 1) of
			true->
				KVList = Fun(KeyValues2),
				if
					FileType == key_value_consult orelse FileType == key_value_consult ->
						{KVList,KVList};
					true->
						NVList = 
							lists:map(fun({_,Value})->
											 Value
									  end, KVList),
						{KVList,NVList}
				end;
			false->
				{KeyValues2,ValList}
		end,
    GetList = lists:foldl(fun({Key, Value}, C) ->
                                lists:concat([C,lists:flatten(io_lib:format("get(~w) -> ~w;\n", [Key, Value]))])
                        end,
                        "",
                        KeyValues3),
    StrList = lists:flatten(io_lib:format("     ~w.\n", [NewValList])),
    
"-module(" ++ gb_util:to_list(ConfModuleName) ++ ").
-export([list/0,get/1]).

list()->"++ StrList ++"\n"++
GetList ++
"get(_) -> undefined.\n".


