-module(proto_struct).
-compile(export_all).

-include("define_proto.hrl").

string(Str) when is_binary(Str)->
	[<<(byte_size(Str)):16>>, Str];
string(Str) when is_list(Str)->
	Str2 = list_to_binary(Str),
	[<<(byte_size(Str2)):16>>, Str2].


-define(int8(V), <<V:8>>).
-define(int16(V), <<V:16>>).
-define(int32(V), <<V:32>>).
-define(int64(V), <<V:64>>).
-define(bool(V), (case V of true -> <<1:8>>; false -> <<0:8>> end)).
-define(string(V), (string(V))).
-define(tuple(MsgID,V), (encode_def(MsgID,V))).


-define(list_int8(List),	[<<(length(List)):16>>, [ ?int8(E) || E<-List ] ]).
-define(list_int16(List),	[<<(length(List)):16>>, [ ?int16(E) || E<-List ]]).
-define(list_int32(List), 	[<<(length(List)):16>>, [ ?int32(E) || E<-List ]]).
-define(list_int64(List), 	[<<(length(List)):16>>, [ ?int64(E) || E<-List ]]).
-define(list_bool(List), 	[<<(length(List)):16>>, [ ?bool(E) || E <- List ]]).
-define(list_string(List),	[<<(length(List)):16>>, [ ?string(E) || E<-List ]]).
-define(list_tuple(MsgID,List),	[<<(length(List)):16>>, [ ?tuple(MsgID,E) || E<-List ]]).

encode_def(?CS_ACCOUNT_LOGIN, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?string(V4),?string(V5),?int16(V6)];
encode_def(?SC_ACCOUNT_LOGIN, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(?CS_ACCOUNT_CREATE, R)->
	{_,V2,V3}=R,
	[?string(V2),?int8(V3)];
encode_def(?SC_ACCOUNT_CREATE, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(?SC_ACCOUNT_KICK, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(?CS_ACCOUNT_HEART, R)->
	{_}=R,
	[];
encode_def(?SC_ACCOUNT_HEART, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(?CS_ACCOUNT_LOGOUT, R)->
	{_}=R,
	[];
encode_def(_, _) -> <<>>.

decode_def(?CS_ACCOUNT_LOGIN)->
	[cs_account_login,int32,int32,string,string,int16];
decode_def(?SC_ACCOUNT_LOGIN)->
	[sc_account_login,int8,bool];
decode_def(?CS_ACCOUNT_CREATE)->
	[cs_account_create,string,int8];
decode_def(?SC_ACCOUNT_CREATE)->
	[sc_account_create,int8];
decode_def(?SC_ACCOUNT_KICK)->
	[sc_account_kick,int8];
decode_def(?CS_ACCOUNT_HEART)->
	[cs_account_heart];
decode_def(?SC_ACCOUNT_HEART)->
	[sc_account_heart,int32];
decode_def(?CS_ACCOUNT_LOGOUT)->
	[cs_account_logout];
decode_def(_) -> [].

get_id(cs_account_login)->10001;
get_id(sc_account_login)->10002;
get_id(cs_account_create)->10003;
get_id(sc_account_create)->10004;
get_id(sc_account_kick)->10005;
get_id(cs_account_heart)->10006;
get_id(sc_account_heart)->10007;
get_id(cs_account_logout)->10008;
get_id(_)->0.