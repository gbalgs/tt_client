%% @author lihuachao
%% @doc @todo Add description to gb_util_proto.


-module(gb_util_proto).

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%%解析protobuf协议文件(.proto结尾的文件)
%%ProtoDir:protobuf协议文件所在的目录
%%ErlOutDir:生成的erl文件输出目录
%%HrlOutDir:生成的hrl文件输出目录
parse(ProtoDir,ErlOutDir,HrlOutDir)->
	proto_compile:scan_dir(ProtoDir,ErlOutDir,HrlOutDir).