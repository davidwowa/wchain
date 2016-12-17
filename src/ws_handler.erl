-module(ws_handler).
%% -behaviour(cowboy_websocket_handler).

-include("block.hrl").

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
	Client = #client { pid = self(), name = "Guest" },
	lager:debug("Client: ~p", [self()]),
	mnesia:dirty_write(Client),
	{ok, Req, Client}.

%% Behandelt eingehende Text-Nachrichten des Websockets
websocket_handle({text, Msg}, Req, State) ->
	relay_message(Msg, State#client.name),
	{ok, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

%% Behandelt eingehende Nachrichten anderer Erlang-Prozesse
websocket_info({{name, Name}, {message, Message}}, Req, State) when erlang:is_list(Message) andalso erlang:is_list(Name) ->
	CompleteMessage = lists:flatten(io_lib:format("~s: ~s", [Name, Message])),
	{reply, {text, erlang:list_to_binary(CompleteMessage)}, Req, State};
websocket_info({{name, Name}, {message, Message}}, Req, State) when erlang:is_binary(Message) ->
	ListMessage = erlang:binary_to_list(Message),
	websocket_info({{name, Name}, {message, ListMessage}}, Req, State);
websocket_info({{name, Name}, {message, Message}}, Req, State) when erlang:is_binary(Name) ->
	ListName = erlang:binary_to_list(Name),
	websocket_info({{name, ListName}, {message, Message}}, Req, State).

%% Wird aufgerufen, wenn die Verbindung geschlossen wird
websocket_terminate(_Reason, _Req, _State) ->
	mnesia:dirty_delete(client, self()),
	ok.

%% Laedt alle Clients und startet die Nachrichtenvermittlung
relay_message(Msg, Name) ->
	Clients = mnesia:dirty_all_keys(client),
	relay_message(Msg, Name, Clients).

%% Leitet die Nachrichten an alle Clients weiter
relay_message(_Msg, _Name, []) ->
	ok;
relay_message(Msg, Name, [Client|Rest]) ->
	case Client =:= self() of
		false ->
			Client ! {{name, Name}, {message, Msg}};
		_ ->
			ok
	end,
	relay_message(Msg, Name, Rest).
