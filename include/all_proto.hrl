-include("define_proto.hrl").
-define(int8, type:int8()).
-define(int16, type:int16()).
-define(int32, type:int32()).
-define(int64, type:int64()).
-define(uint8, type:int8()).
-define(uint16, type:int16()).
-define(uint32, type:int32()).
-define(uint64, type:int64()).
-define(string, type:str()).

-record(cs_account_login,{
	userID=[] :: ?int32,
	unixTime=[] :: ?int32,
	accountName=[] :: ?string,
	ticket=[] :: ?string,
	serverID=[] :: ?int16}).
-record(sc_account_login,{
	result=[] :: ?int8,
	isCreate=[] :: boolean()}).
-record(cs_account_create,{
	roleName=[] :: ?string,
	modelID=[] :: ?int8}).
-record(sc_account_create,{
	result=[] :: ?int8}).
-record(sc_account_kick,{
	reason=[] :: ?int8}).
-record(cs_account_heart,{
	}).
-record(sc_account_heart,{
	unixTime=[] :: ?int32}).
-record(cs_account_logout,{
	}).
