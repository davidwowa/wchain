-module(ws_handler).

-include("block.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% Wird beim initialen Verbindungsaufbau aufgerufen und upgradet das Protokoll auf einen Websocket
init({tcp, http}, _Req, _Opts) ->
%% 	mnesia:del_table_copy(player, wchain@mac),
	Players = mnesia:dirty_all_keys(player),
	lager:info("on init : players in mnesia : ~p ", [Players]),
	{upgrade, protocol, cowboy_websocket}.

%% Wird aufgerufen, sobald der Websocket erstellt wurde
websocket_init(_TransportName, Req, _Opts) ->
	
	{CookieVal, Req2} = cowboy_req:cookies(Req),
	
	lager:info("~p", [CookieVal]),
	
	Player = #player { pid = self(), uuid = uuid:to_string(uuid:v4()), score=0 },
	lager:info("new player : ~p ~p", [Player#player.uuid, Player#player.pid]),
	mnesia:dirty_write(Player),
	NewClientMessage = lists:flatten(io_lib:format("60:~s:~p:~p", [Player#player.uuid, Player#player.score, 1])),
	lager:info("new player ~p ", [NewClientMessage]),
	relay_message(NewClientMessage, Player#player.uuid),
	{ok, Req, Player}.

%% Behandelt eingehende Text-Nachrichten des Websockets
websocket_handle({text, Msg}, Req, State) ->
	
	MessageNr = string:sub_string(binary_to_list(Msg), 1, 2),
	lager:info("incomming message : ~p", [MessageNr]),
	if
		MessageNr == "10"->lager:info("generate uuid"),
						   UUID = uuid:to_string(uuid:v4()),
						   UUIDasMessage = lists:flatten(io_lib:format("10:~s", [UUID])),
						   relay_message_uuid(UUIDasMessage, State#player.uuid);
		MessageNr == "20"->lager:info("mine hash"),
						   String = binary_to_list(Msg),
						   Idx = string:cspan(String, ":"),
						   UUID = string:sub_string(String, Idx, string:len(String)),
						   {Hash, Count} = my_sha:generate_sha(UUID, crypto:hash_init(md5), 0),
						   Player = #player {pid=State#player.pid, uuid = State#player.uuid, score = Count, current_uuid=UUID},
						   mnesia:dirty_write(Player),
						   RelayMessage = lists:flatten(io_lib:format("20:~s:~w", [Hash, Count])),
						   relay_message(RelayMessage, State#player.uuid);
		MessageNr == "30"->lager:info("reset"),
						   RelayMessage = lists:flatten(io_lib:format("30:~s", ["RESET"])),
						   relay_message(RelayMessage, State#player.uuid);
		MessageNr == "40"->lager:info("stop"),
						   RelayMessage = lists:flatten(io_lib:format("40:~s", ["STOP"])),
						   relay_message(RelayMessage, State#player.uuid);
		MessageNr == "50"->lager:info("winner"),
						   Winner = "YOU",
						   RelayMessage = lists:flatten(io_lib:format("50:~s", [Winner])),
						   relay_message(RelayMessage, State#player.uuid);
		MessageNr == "60"->lager:info("update palyers information");
		true -> lager:info("unknown message")
	end,
	{ok, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

%% traverse_table_and_show(Table_name)->
%% 	Iterator =  fun(Rec,_)->
%% 						io:format("~p~n",[Rec]),
%% 						[]
%% 				end,
%% 	case mnesia:is_transaction() of
%% 		true -> mnesia:foldl(Iterator,[],Table_name);
%% 		false -> 
%% 			Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
%% 			mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
%% 	end.

%% Behandelt eingehende Nachrichten anderer Erlang-Prozesse
websocket_info({{uuid, Name}, {message, Message}}, Req, State) when erlang:is_list(Message) andalso erlang:is_list(Name) ->
	CompleteMessage = lists:flatten(io_lib:format("~s", [Message])),
	{reply, {text, erlang:list_to_binary(CompleteMessage)}, Req, State};
websocket_info({{uuid, Name}, {message, Message}}, Req, State) when erlang:is_binary(Message) ->
	ListMessage = erlang:binary_to_list(Message),
	websocket_info({{uuid, Name}, {message, ListMessage}}, Req, State);
websocket_info({{uuid, Name}, {message, Message}}, Req, State) when erlang:is_binary(Name) ->
	ListName = erlang:binary_to_list(Name),
	websocket_info({{uuid, ListName}, {message, Message}}, Req, State).

%% Wird aufgerufen, wenn die Verbindung geschlossen wird
websocket_terminate(_Reason, _Req, State) ->
	lager:info("delete client ~p ", [State]),
	mnesia:dirty_delete(player, self()),
	DeleteClientMessage = lists:flatten(io_lib:format("60:~s:~p:~p", [State#player.uuid, State#player.score, 0])),
	relay_message(DeleteClientMessage, State#player.uuid),
	ok.

%% Laedt alle Clients und startet die Nachrichtenvermittlung
relay_message(Msg, Name) ->
	Players = mnesia:dirty_all_keys(player),
	relay_message(Msg, Name, Players).

%% Leitet die Nachrichten an alle Clients weiter
relay_message(_Msg, _Name, []) ->
	ok;
relay_message(Msg, Name, [Player|Rest]) ->
	Player ! {{uuid, Name}, {message, Msg}},
	relay_message(Msg, Name, Rest).

%% send messages with generated uuid
relay_message_uuid(Msg, Name) ->
	Players = mnesia:dirty_all_keys(player),
	relay_message_uuid(Msg, Name, Players).

relay_message_uuid(_Msg, _Name, []) ->
	ok;
relay_message_uuid(Msg, Name, [Player|Rest]) ->
	case Player =:= self() of
		false ->
			Player ! {{uuid, Name}, {message, Msg}};
		_ ->
			ok
	end,
	relay_message(Msg, Name, Rest).