%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Copyright (C) 2019 ... All rights reserved.
%%      FileName ：netease.erl
%%      Create   ：Jin <ymilitarym@163.com
%%      Date     : 2019-05-13
%%      Describle: 
%%      
%%      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(netease).
-export([verify_server/4,
				 send_message/1,
				 copy_message/1]).
-export([verify_copy_message_server/4,
				 handle_copy_message/4,
				 create_account/1,
				 update_account/1,
				 get_user_info/1,
				 refresh_token/1,
				 sys_notify/1,
				 send_friend_relationship/1,
				 modify_friend_alias/1,
				 delete_friend/1,
				 add_blacklist/1,
				 cancel_blacklist/1,
				 keep_silence/1,
				 cancel_silence/1,
				 block/1, unblock/1]).


add_blacklist(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {add_blacklist, ObjectRecord})
	end).

cancel_blacklist(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {cancel_blacklist, ObjectRecord})
	end).

keep_silence(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {keep_silence, ObjectRecord})
	end).

cancel_silence(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {cancel_silence, ObjectRecord})
	end).



delete_friend(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {delete_friend, ObjectRecord})
	end).

modify_friend_alias(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {modify_friend_alias, ObjectRecord})
	end).

send_friend_relationship(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {send_friend_relationship, ObjectRecord})
	end).

verify_server(Body, OriCheckSum, OriCurTime, OriMD5)	->
	verify_server(Body, OriCheckSum, OriCurTime, OriMD5, 5000).

verify_server(Body, OriCheckSum, OriCurTime, OriMD5, Timeout)	->
	poolboy:transaction(?MODULE, fun(Worker) ->
    gen_server:call(Worker, {verify_server, {Body, OriCheckSum, OriCurTime, OriMD5}}, Timeout)
  end).

copy_message(Body)	->
	poolboy:transaction(?MODULE, fun(Worker) ->
    gen_server:cast(Worker, {copy_message, Body})
  end).

verify_copy_message_server(Body, OriCheckSum, OriCurTime, OriMD5)	->
	case verify_server(Body, OriCheckSum, OriCurTime, OriMD5) of
		ok	->
			ok;
		_	->
			error
	end.

handle_copy_message(Body, OriCheckSum, OriCurTime, OriMD5)	->
	lager:info("Mdule:~p, Line:~p, Body:~p~n", [?MODULE, ?LINE, Body]),
	case verify_copy_message_server(Body, OriCheckSum, OriCurTime, OriMD5) of
		ok	->
			lager:info("sign verify sucessfully."),
			copy_message(Body),
			ok;
		error	->
			error
	end.

create_account(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:call(Worker, {create_account, ObjectRecord})
	end).

update_account(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {update_account, ObjectRecord})
	end).

get_user_info(CpAccountID)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:call(Worker, {get_user_info, CpAccountID})
	end).

refresh_token(CpAccountID)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:call(Worker, {refresh_token, CpAccountID})
	end).

block(CpAccountID)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {block, CpAccountID})
	end).

unblock(CpAccountID)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {unblock, CpAccountID})
	end).

sys_notify(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Worker)	->
		gen_server:cast(Worker, {sys_notify, ObjectRecord})
	end).

send_message(ObjectRecord)	->
	poolboy:transaction(?MODULE, fun(Work)	->
		gen_server:cast(Work, {send_message, ObjectRecord})
	end).

