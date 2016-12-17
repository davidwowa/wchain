-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% Wird beim initialen Verbindungsaufbau aufgerufen und upgradet das Protokoll auf einen Websocket
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

%% Wird aufgerufen, sobald der Websocket erstellt wurde
websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

%% Behandelt eingehende Nachrichten des Websockets
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% Behandelt eingehende Nachrichten anderer Erlang-Prozesse
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%% Wird aufgerufen, wenn die Verbindung geschlossen wird
websocket_terminate(_Reason, _Req, _State) ->
    ok.
