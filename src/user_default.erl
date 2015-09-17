%% @author lihuachao
%% @doc @todo Add description to user_default.


-module(user_default).

%% ====================================================================
%% API functions
%% ====================================================================

-include("all_proto.hrl").

-export([pg/0]).


%%加载协议
pg()->
	gb_util_proto:parse("./proto","./src","./include").


