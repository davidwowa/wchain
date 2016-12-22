%%%-------------------------------------------------------------------
%% @doc wchain public API
%% @end
%%%-------------------------------------------------------------------

-module(wchain_app).

-behaviour(application).

-include("block.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	{ok, _Apps1} = application:ensure_all_started(lager),
	{ok, _Apps2} = application:ensure_all_started(ranch),
	%% 	lager:start(),
	lager:info("CLIENT: ~p", [self()]),
	ok = setup_cowboy(),
	ok = setup_mnesia(),
	wchain_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%====================================================================
%% Internal functions
%%====================================================================
setup_cowboy() ->
	lager:info("setup cowboy server"),
	Dispatch = cowboy_router:compile([
									  {'_', [                                                                       %% Hostname fuer den die Routen gelten
												{"/", cowboy_static, {priv_file, wchain, "index.html"}},            
												{"/websocket", ws_handler, []},            %% /websocket wird von dem Modul ws_handler ausgeliefert
												{"/static/[...]", cowboy_static, {priv_dir, wchain, "static"}},
												{"/css/[...]", cowboy_static, {priv_dir, wchain, "css"}}
											]}
									 ]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 5555}], [{env, [{dispatch, Dispatch}]}]),                        %% HTTP-Server starten
	ok.

setup_mnesia() ->
	lager:info("setup mnesia"),
	{ok, _Apps} = application:ensure_all_started(mnesia, permanent),
	case mnesia:wait_for_tables([client], 5000) of                      %% Prueft ob alle Tabellen vorhanden sind
		ok ->
			ok;
		_ ->
			mnesia:stop(),
			ok = install_mnesia_tables()                                %% Wenn Tabellen fehlen, werden die eingerichtet
	end,
	ok.

install_mnesia_tables() ->
	mnesia:create_schema([node()]),                                     %% Zunaechst wird ein Schema erzeugt
	ok = mnesia:start(),
	mnesia:create_table(client,                                         %% Dann werden Tabellen angelegt
						[{attributes, record_info(fields, client)},
						 {ram_copies, [node()]}]),
	ok.