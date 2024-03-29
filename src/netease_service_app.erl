%%%-------------------------------------------------------------------
%% @doc netease_service public API
%% @end
%%%-------------------------------------------------------------------

-module(netease_service_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(StartType, StartArgs) ->
	redis_pool_sup:start_link(),
    netease_service_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
