%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Copyright (C) 2019 ... All rights reserved.
%%      FileName ：netease_service.erl
%%      Create   ：Jin <ymilitarym@163.com
%%      Date     : 2019-05-10
%%      Describle: 
%%      
%%      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(netease_service).
-export([handle_info/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([terminate/2, code_change/3]).
-record(state, {key, secret, redis, url}).

start_link(Args) ->
	io:format("Args:~p~n", [Args]),
  	gen_server:start_link(?MODULE, Args, []).


init(Args) ->
 	AppKey = proplists:get_value(app_key, Args),
	AppSecret = proplists:get_value(app_secret, Args),
	Url = proplists:get_value(url, Args),
	Redis = proplists:get_value(redis, Args),
	State = #state{key = AppKey, secret = AppSecret, redis = Redis, url = Url},
  	{ok, State}.

handle_call({create_account, ObjectRecord}, _From, #state{key = AppKey, secret = AppSecret, url = Url, redis = Redis} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	NickName = proplists:get_value(<<"nickname">>, ObjectRecord, null),
	Props = proplists:get_value(<<"props">>, ObjectRecord, null),
	Avatar = proplists:get_value(<<"avatar">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"name">>, NickName},
				{<<"props">>, Props},
				{<<"icon">>, Avatar}],
	Body = handle_create_netease_account_body(BodyList),
	Headers = get_header(Body, AppKey, AppSecret),
	Uri = Url ++ "user/create.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	{ok, {_Status, _Headers, Res}} = httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	Json = jsx:decode(util:to_unicode_binary(Res)),
	Token = 
	case proplists:get_value(<<"code">>, Json, 200) == 200 of
		true	->
			proplists:get_value(<<"token">>, proplists:get_value(<<"info">>, Json, []), null);
		false	->
			null
	end,
	%%%%%%%%%此处需要保存token
	%%%%%%%%%
	%%%%%%%%%
	case Token == null of
		true  ->
			ok;
		false ->
			NeteaseTokenKey = get_netease_token_key(),
			redis_pool:q(Redis, [hset, NeteaseTokenKey, CpAccountID, Token])
	end,
	{reply, Token, State};

handle_call({refresh_token, CpAccountID}, _From, #state{key = AppKey, secret = AppSecret, url = Url, redis = Redis} = State)	->
	NeteaseTokenKey = get_netease_token_key(),
	{_Status, Result} = redis_pool:q(Redis, [hget, NeteaseTokenKey, CpAccountID]),
	if
		Result == undefined ->
			Token = undefined;
		Result == <<"null">>	->
			Token = undefined;
		Result == <<>>	->
			Token = undefined;
		true	->
			Token = Result
	end,
	case Token == undefined of
		false	->
			NewToken = Token;
		true	->
			Body = "accid=" ++ util:to_list(CpAccountID),
			Uri = Url ++ "user/refreshToken.action" ++ "?" ++ Body,
			ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
			Headers = get_header(Body, AppKey, AppSecret),
			Response = httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
			{ok, {_ResStatus, _ResContentInfo, Res}} = Response,
			Json = jsx:decode(util:to_unicode_binary(Res)),
			case proplists:get_value(<<"code">>, Json, 200) == 200 of
				true	->
					NewToken = proplists:get_value(<<"token">>, proplists:get_value(<<"info">>, Json, []), null),
					redis_pool:q(Redis, [hset, NeteaseTokenKey, CpAccountID, NewToken]),
					NewToken;
				false	->
					NewToken = null
			end
	end,
	{reply, NewToken, State};

handle_call({get_user_info, CpAccountIDs}, _From, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	Body = "accids=" ++ handle_list_string(CpAccountIDs),
	Uri = Url ++ "user/getUinfos.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	{ok, {_Status, _Headers, Res}} = httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	Json = jsx:decode(util:to_unicode_binary(Res)),
	UInfos = 
	case proplists:get_value(<<"code">>, Json, 200) == 200 of
		true	->
			proplists:get_value(<<"uinfos">>, Json, []);
		false	->
			[]
	end,
	{reply, UInfos, State};

handle_call({verify_server, {Body, OriCheckSum, OriCurTime, OriMD5}}, _From, #state{secret = AppSecret} = State)	->
	TestArgs = [OriCheckSum, OriCurTime, OriMD5],
	Reply = 
	case util:contain_undefined(TestArgs) of
		false	->
			MD5 = erlang:md5(Body),
			MD5Hash = hash:to_hash_32(MD5),
			Data = AppSecret ++ MD5Hash ++ OriCurTime,
			CheckSum = hash:hash(sha, Data),
			CheckSumHash = hash:to_hash_40(CheckSum),
			case OriMD5 == MD5Hash andalso CheckSumHash == OriCheckSum of
				true  ->
					ok;
				false ->
					error
			end;
		true  ->
			{error, "miss params"}
	end,
	{reply, Reply, State};
handle_call(_Requset, _From , State)	->
	{reply, {error, "error Requset"}, State}.

handle_cast({add_blacklist, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	Friend = proplists:get_value(<<"friend">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"faccid">>, Friend},
				{<<"relationType">>, 1},
				{<<"value">>, 1}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "user/setSpecialRelation.action?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({cancel_blacklist, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	Friend = proplists:get_value(<<"friend">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"faccid">>, Friend},
				{<<"relationType">>, 1},
				{<<"value">>, 0}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "user/setSpecialRelation.action?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({keep_silence, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	Friend = proplists:get_value(<<"friend">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"faccid">>, Friend},
				{<<"relationType">>, 2},
				{<<"value">>, 1}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "user/setSpecialRelation.action?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({cancel_silence, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	Friend = proplists:get_value(<<"friend">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"faccid">>, Friend},
				{<<"relationType">>, 2},
				{<<"value">>, 0}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "user/setSpecialRelation.action?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({delete_friend, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	Friend = proplists:get_value(<<"friend">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"faccid">>, Friend}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "friend/delete.action?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};


handle_cast({modify_friend_alias, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	Friend = proplists:get_value(<<"friend">>, ObjectRecord, null),
	Alias = proplists:get_value(<<"alias">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"faccid">>, Friend},
				{<<"alias">>, Alias}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "friend/update.action?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({send_friend_relationship, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	Friend = proplists:get_value(<<"friend">>, ObjectRecord, null),
	Type = proplists:get_value(<<"type">>, ObjectRecord, 1),
	Msg = proplists:get_value(<<"msg">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"faccid">>, Friend},
				{<<"type">>, Type},
				{<<"msg">>, Msg}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "friend/add.action?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({send_message, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	From = proplists:get_value(<<"from">>, ObjectRecord, null),
	Ope = util:to_number(proplists:get_value(<<"ope">>, ObjectRecord, 0)),
	To = proplists:get_value(<<"to">>, ObjectRecord, null),
	Type = proplists:get_value(<<"type">>, ObjectRecord, null),
	BodyMessage = proplists:get_value(<<"body">>, ObjectRecord, <<>>),
	BodyList = [{<<"from">>, From},
				{<<"ope">>, Ope},
				{<<"to">>, To},
				{<<"type">>, Type},
				{<<"body">>, BodyMessage}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "msg/sendMsg.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({sys_notify, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	From = proplists:get_value(<<"from">>, ObjectRecord, null),
	To = proplists:get_value(<<"to">>, ObjectRecord, null),
	Type = proplists:get_value(<<"type">>, ObjectRecord, 0),
	Attach = proplists:get_value(<<"attach">>, ObjectRecord, null),
	Save = proplists:get_value(<<"save">>, ObjectRecord, 0),
	BodyList = [{<<"from">>, From},
				{<<"to">>, To},
				{<<"msgtype">>, Type},
				{<<"attach">>, Attach},
				{<<"save">>, Save}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "msg/sendAttachMsg.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({unblock, CpAccountID}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	Body = "accid=" ++ util:to_list(CpAccountID),
	Uri = Url ++ "user/unblock.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({block, CpAccountID}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	Body = "accid=" ++ util:to_list(CpAccountID),
	Uri = Url ++ "user/block.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({update_account, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	NickName = proplists:get_value(<<"nickname">>, ObjectRecord, null),
	Avatar = proplists:get_value(<<"avatar">>, ObjectRecord, null),
	Sign = proplists:get_value(<<"sign">>, ObjectRecord, null),
	Birthday = proplists:get_value(<<"birth">>, ObjectRecord, null),
	Gender = proplists:get_value(<<"gender">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"name">>, NickName},
				{<<"icon">>, Avatar},
				{<<"sign">>, Sign},
				{<<"birth">>, Birthday},
				{<<"gender">>, Gender}],
	Body = handle_create_netease_account_body(BodyList),
	Uri = Url ++ "user/updateUinfo.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	Headers = get_header(Body, AppKey, AppSecret),
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({create_account, ObjectRecord}, #state{key = AppKey, secret = AppSecret, url = Url} = State)	->
	CpAccountID = proplists:get_value(<<"cp_account_id">>, ObjectRecord, null),
	NickName = proplists:get_value(<<"nickname">>, ObjectRecord, null),
	Props = proplists:get_value(<<"props">>, ObjectRecord, null),
	Avatar = proplists:get_value(<<"avatar">>, ObjectRecord, null),
	BodyList = [{<<"accid">>, CpAccountID},
				{<<"name">>, NickName},
				{<<"props">>, Props},
				{<<"icon">>, Avatar}],
	Body = handle_create_netease_account_body(BodyList),
	Headers = get_header(Body, AppKey, AppSecret),
	Uri = Url ++ "user/create.action" ++ "?" ++ Body,
	ContentType = "Content-Type:application/x-www-form-urlencoded;charset=utf-8",
	httpc:request(post, {Uri, Headers, ContentType, []}, [], []),
	{noreply, State};

handle_cast({copy_message, Body}, State)	->
	lager:info("Module:~p, Line:~p, Body:~p~n", [?MODULE, ?LINE, Body]),
	%%%% 处理抄送消息
	{noreply, State};
handle_cast(shutdown, State) ->
  	{stop, normal, State};
handle_cast(Msg, State) ->
	lager:info("Msg:~p~n", [Msg]),
    {noreply, State}.

handle_info(_Message, State) -> 
	{noreply, State}.

%% Server termination
terminate(_Reason, _State) ->
	ok.

%% Code change
code_change(_OldVersion, State, _Extra) -> {ok, State}.


get_header(Body, AppKey, AppSecret)	->
	CurTime = util:to_list(util:timestamp()),
	Nonce = hash:get_file_name(util:to_list(CurTime)),
	BodyMD5Hash = hash:to_hash_32(erlang:md5(Body)),
	Data = AppSecret ++ Nonce ++ CurTime,
	CheckSum = hash:hash(sha, Data),
	CheckSumHash = hash:to_hash_40(CheckSum),
	[{"AppKey", AppKey},
	 {"CurTime", CurTime},
	 {"CheckSum", CheckSumHash},
	 {"Md5", BodyMD5Hash},
	 {"Nonce", Nonce}].

handle_create_netease_account_body(TupleList)	->
	F = fun({_Key, Value})	->
		not(Value == null)
	end,
	Filter = lists:filter(F, TupleList),
	F1 = fun({K1, V1})	->
		"&" ++ percent_encode(K1) ++ "=" ++ percent_encode(util:to_binary(V1))
	end,
	[_Head | Tail] = lists:concat(lists:map(F1, Filter)),
	Tail.

handle_list_string(CpAccountID) when is_binary(CpAccountID)	->
	lists:concat(["[", "\"", util:to_list(CpAccountID), "\"]"]);
handle_list_string(CpAccountIDs) when is_list(CpAccountIDs)	->
	F = fun(CpAccountID)	->
		",\"" ++ util:to_list(CpAccountID) ++ "\""
	end,
	[_Head | Tail] = _String = lists:concat(lists:map(F, CpAccountIDs)),
	lists:concat(["[", Tail, "]"]).

get_netease_token_key()	->
	"NETEASE_TOKEN.".

percent_encode(String)	->
	PlusString = re:replace(http_uri:encode(String), "\\+", "%20", [global, {return, list}]),
	StarString = re:replace(PlusString, "\\*", "%2A", [global, {return, list}]),
	re:replace(StarString, "%7E", "~", [global, {return, list}]).

