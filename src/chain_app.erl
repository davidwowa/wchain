%%%-------------------------------------------------------------------
%% @doc chain public API
%% @end
%%%-------------------------------------------------------------------

-module(chain_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
%%     {ok, _Apps1} = application:ensure_all_started(lager),
%%     {ok, _Apps2} = application:ensure_all_started(ranch),
%% 		ok = setup_cowboy(),
%%     ok = setup_mnesia(),
	lager:start(),
	lager:info("~s is ~s", [lager, cool]),
	lager:warning("but pay ~s!", [attention]),
	lager:error("there is always some ~s", [error]),
	chain_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================