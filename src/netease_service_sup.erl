%%%-------------------------------------------------------------------
%% @doc netease_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(netease_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
		NeteaseServiceSpecs = get_netease_service_specs(),
	    {ok, {{one_for_one, 1, 100}, NeteaseServiceSpecs}}.

get_netease_service_specs()	->
		% {ok, App} = application:get_application(?MODULE),
		App = netease_service,
		{ok, Services} = application:get_env(App, netease_services),
		[get_netease_service_spec(App, Service) || Service <- Services].

get_netease_service_spec(App, Service)	->
		{ok, Opts} = application:get_env(App, Service),
		Args = [{name, {local, Service}},
				{worker_module, netease_service}] ++ Opts,
		poolboy:child_spec(Service, Args, Opts).













%%====================================================================
%% Internal functions
%%====================================================================
