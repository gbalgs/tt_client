-module(proto_route).
-compile(export_all).
-include("define_proto.hrl").

route(?CS_ACCOUNT_LOGIN) ->
	{role,role_account};
route(?CS_ACCOUNT_CREATE) ->
	{role,role_account};
route(?CS_ACCOUNT_HEART) ->
	{role,role_account};
route(?CS_ACCOUNT_LOGOUT) ->
	{role,role_account};
route(_) ->undefined.
