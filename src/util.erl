%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Copyright (C) 2019 ... All rights reserved.
%%      FileName ：util.erl
%%      Create   ：Jin <ymilitarym@163.com
%%      Date     : 2019-05-10
%%      Describle: 
%%      
%%      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(util).
-export([to_list/1, timestamp/0,
				 to_unicode_binary/1,
				 contain_undefined/1,
				 to_number/1, to_binary/1]).

can_find(_Element,[]) ->
		false;

can_find(Element,[Item]) ->
		case (Item =:= Element) of
			true -> true;
			false -> false
		end;

can_find(Element,[Item|ListTail]) ->
		case ( Item =:= Element ) of
		  true    ->  true;
		  false   ->  can_find(Element, ListTail)
		end.

contain_empty(List) ->
		can_find(undefined,List) or can_find(undefined,List) or can_find(<<>>,List).

contain_undefined(List) ->
		can_find(undefined,List).

contain_null(List) ->
		can_find(null,List).


exist(_Element,[]) ->
		false;
exist(Element,[Item]) ->
		case (Item =:= Element) of
			true -> true;
			false -> false
		end;
exist(Element,[Item|ListTail]) ->
		case ( Item =:= Element ) of
        true    ->  true;
        false   ->  exist(Element, ListTail)
    end.

to_list(Key) ->
		if
			Key == undefined ->
				"";
			is_list(Key) ->
			  unicode:characters_to_list(Key);
			is_atom(Key) ->
				atom_to_list(Key);
			is_binary(Key) ->
				unicode:characters_to_list(Key);
			is_integer(Key) ->
				integer_to_list(Key);
			is_float(Key) ->
				float_to_list(Key);
			true ->
			  R = io_lib:format("~p",[Key]),
			  lists:flatten(R)
		end.

to_term(String) ->
		S = to_list(String),
		case erlang:is_list(S) of
		  true ->
		    {ok, T, _} = erl_scan:string(S++"."),
		    case erl_parse:parse_term(T) of
		        {ok, Term} ->
		            Term;
		        _ ->
		            undefined
		    end;
		  false ->
			  undefined
		end.

to_unicode_binary(Key) ->
  %% Key不能是binary哦！
  unicode:characters_to_binary(cp_value:to_list(Key),utf8).

to_binary(Key) ->
   if
    	Key == undefined ->
      	<<"">>;
		is_list(Key) ->
      ?MODULE:to_unicode_binary(Key);
			%erlang:list_to_binary(Key);
		is_atom(Key) ->
			case Key of
		    	null ->
		       		<<"">>;
		    	Key ->
		    	   	atom_to_binary(Key, latin1)
		 	end;
      	is_binary(Key) ->
         	Key;
      	is_integer(Key) ->
         	erlang:list_to_binary(integer_to_list(Key));
    	is_float(Key) ->
     		erlang:list_to_binary(float_to_list(Key));
        true ->
            error
   end.

to_number(Key) ->
		ListKey = to_list(Key),
		case ?MODULE:is_number(ListKey) of
		  {true, integer} ->
		    erlang:list_to_integer(ListKey);
		  {true, float} ->
		    erlang:list_to_float(ListKey);
		  {false,not_number} ->
		    -1;
		  Other ->
			  error
  end.

is_number(String)  ->
    StringNumber = to_list(String),
    case string:to_integer(String) of
        {error,no_integer} ->
            {false,not_number};
        {_N,[]} ->
            {true,integer};
        {_,_Other} ->
            case string:to_float(StringNumber) of
                {error,no_float} ->
                    %{false,no_float};%%这种情况存在嘛？上面已经判断不是inter,这里判断不是float
                    {false,not_number};
                {_,[]} ->
                    {true,float};
                {_,_OtherTail} ->
                    {false,not_number}
            end
    end.

is_boolean(Value) ->
    case to_list(Value) of
        "true" ->
            true;
        "false" ->
            false;
        "True" ->
            true;
        "False" ->
            false;
        _ ->
            error
    end.

timestamp() ->
	{Megaseconds, Seconds, _Microseconds} = os:timestamp(),
	Megaseconds * 1000000 + Seconds.
