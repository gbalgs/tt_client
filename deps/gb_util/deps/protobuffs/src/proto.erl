-module(proto).

-compile(export_all).


-define(undef, undefined).



-export_type([int8/0,int16/0,int32/0,int64/0,str/0]).

-type int8() :: -128..127.
-type int16() :: -32768..32767.
-type int32()   :: -2147483648..2147483647.
-type int64()   :: -9223372036854775808..9223372036854775807.
-type str() :: [0..255].

encode(Record)->
	RecordName = erlang:element(1, Record),
	MsgID = proto_struct:get_id(RecordName),
	encode(MsgID,Record).
	
encode(MsgID,Record)->
	Bin = proto_struct:encode_def(MsgID, Record),
%%  	Size = iolist_size(Bin)+2,
	[<<MsgID:16>>| Bin].
	

%% encode(int64, A) ->
%% 	<<A:64>>;
%% encode(int32, A) ->
%% 	<<A:32>>;
%% encode(int16, A) ->
%% 	<<A:16>>;
%% encode(int8,  A) ->
%% 	<<A:8>>;
%% encode(string, A) ->
%% 	Bin = iolist_to_binary(A),
%% 	BitSize=  erlang:byte_size(Bin),
%% 	<<BitSize:16, Bin/binary>>;
%% encode(bool, A) ->
%% 	if A=:=true ->
%% 		<<1:8>>;
%% 	true ->
%% 		<<0:8>>
%% 	end;
%% encode({list, Type}, A) ->
%% 	Len = length(A),
%% 	Bin = [encode(Type, Element)||Element<-A],
%% 	[<<Len:16>>, Bin];
%% encode(tuple, Record) ->
%% 	[TupleType|VarList] = tuple_to_list(Record),
%% 	[ID|TypeList] = proto_struct:encode_def(TupleType),
%% 	Bin = lists:zipwith(fun encode/2, TypeList, VarList),
%% 	[<<ID:16>>, Bin].


decode(<<ID:16,Bin/binary>>) ->
	[RecName|TypeList] = proto_struct:decode_def(ID),
	%% 剩余字节严格匹配，不隐藏前端多发字节的错误
	{ok, <<>>, Result} = decode3(TypeList, Bin, [RecName]),
	{ID,list_to_tuple(Result)}.

decode3([int64|TypeList],Bin,Result) ->
	<<Int:64/signed-integer, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [Int|Result]);
decode3([int32|TypeList],Bin,Result) ->
	<<Int:32/signed-integer, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [Int|Result]);
decode3([int16|TypeList],Bin,Result) ->
	<<Int:16/signed-integer, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [Int|Result]);
decode3([int8|TypeList],Bin,Result) ->
	<<Int:8/signed-integer, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [Int|Result]);
decode3([string|TypeList],Bin,Result) ->
	<<Len:16/signed-integer, StrBin:Len/binary-unit:8, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [StrBin|Result]);
decode3([bool|TypeList], Bin, Result) ->
	<<Bool:8/signed-integer, Bin2/binary>> = Bin,
	BoolErl = 
	if Bool =:= 1 ->
		true;
	true ->
		false
	end,
	decode3(TypeList, Bin2, [BoolErl|Result]);
decode3([{list,Type}|TypeList],Bin, Result) ->
	<<Len:16/signed-integer, Bin2/binary>> = Bin,
	ChildTypeList = lists:duplicate(Len, Type),
	{ok, Bin3, List} = decode3(ChildTypeList, Bin2, []),
	decode3(TypeList, Bin3, [List|Result]);
decode3([any|TypeList],Bin,Result)->
	<<ID:8/signed-integer,Bin2/binary>> = Bin,
	case ID of
		241->
			<<V:8/signed-integer,Rest1/binary>> = Bin2,
			decode3(TypeList, Rest1, [V|Result]);
		242->
			<<V:16/signed-integer,Rest1/binary>> = Bin2,
			decode3(TypeList, Rest1, [V|Result]);
		243->
			<<V:32/signed-integer,Rest1/binary>> = Bin2,
			decode3(TypeList, Rest1, [V|Result]);
		244->
			<<V:64/signed-integer,Rest1/binary>> = Bin2,
			decode3(TypeList, Rest1, [V|Result]);
		245->
			<<V:8/signed-integer,Rest1/binary>> = Bin2,
			decode3(TypeList, Rest1, [V|Result]);
		246->
			<<Len:16/signed-integer,Rest1/binary>> = Bin2,
			Len1 = Len * 8,
			<<V:Len1,Rest2/binary>> = Rest1,
			decode3(TypeList, Rest2, [V|Result]);
		_->
			<<RecordID:16/signed-integer,_Bin3/binary>>=Bin,
			decode3([RecordID|TypeList],Bin,Result)
	end;
decode3([RecordID|TypeList], Bin, Result) ->
%	<<ID:16, Bin2/binary>> = Bin,
	[RecName|ChildTypeList] = proto_struct:decode_def(RecordID),
	{ok, Bin3, ChildVal} = decode3(ChildTypeList, Bin, [RecName]),
	decode3(TypeList, Bin3, [list_to_tuple(ChildVal)|Result]);
decode3([],Bin,Result) ->
	{ok, Bin, lists:reverse(Result)}.

