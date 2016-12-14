%%%-------------------------------------------------------------------
%% @doc chain top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chain_sup).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	lager:start(),
	lager:info("~s is ~s!", [lager, cool]),
	lager:warning("but pay ~s!", [attention]),
	lager:error("there is always some ~s", [error]),
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================