%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Copyright (C) 2019 ... All rights reserved.
%%      FileName ：hash.erl
%%      Create   ：Jin <ymilitarym@163.com
%%      Date     : 2019-05-10
%%      Describle: 
%%      
%%      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(hash).
-export([hash/2]).
-export([to_hash_32/1,
				 to_hash_40/1,
				 to_hash_64/1,
				 to_hash_128/1,
				 get_file_name/1]).

hash(Method, Data)	->
	crypto:hash(Method, Data).

to_hash_32(Binary) when is_binary(Binary)	->
	<<X:128/big-unsigned-integer>> = Binary,
	lists:flatten(io_lib:format("~32.16.0b", [X])).

to_hash_40(Binary) when is_binary(Binary)	->
	<<X:160/big-unsigned-integer>> = Binary,
	lists:flatten(io_lib:format("~40.16.0b", [X])).

to_hash_64(Binary) when is_binary(Binary)	->
	<<X:256/big-unsigned-integer>> = Binary,
	lists:flatten(io_lib:format("~64.16.0b", [X])).

to_hash_128(Binary) when is_binary(Binary)	->
	<<X:512/big-unsigned-integer>> = Binary,
	lists:flatten(io_lib:format("~128.16.0b", [X])).

get_file_name(File)	->
	to_hex(erlang:md5(File)).

to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).

%% @spec hexdigit(integer()) -> char()
%% @doc Convert an integer less than 16 to a hex digit.
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

%% Internal API

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).
