-module(ws_handler).

-include_lib("stdlib/include/qlc.hrl").
-include("block.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([find_winner/0]).
-export([show_all_players/0]).

%% Wird beim initialen Verbindungsaufbau aufgerufen und upgradet das Protokoll auf einen Websocket
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

%% Wird aufgerufen, sobald der Websocket erstellt wurde
websocket_init(_TransportName, Req, _Opts) ->
	
	{CookieVal, _} = cowboy_req:cookie(<<"uuid">>, Req),	
	
	Player = #player { pid = self(), init_uuid = CookieVal, uuid = CookieVal, current_uuid = CookieVal, score=0, current_score=0 },
	mnesia:dirty_write(Player),
	NewClientMessage = lists:flatten(io_lib:format("60:~s:~p", [Player#player.init_uuid, 1])),
	lager:info("new player ~p ", [NewClientMessage]),
	show_all_players(),
	relay_message(NewClientMessage, Player#player.init_uuid),
	{ok, Req, Player}.

%% Behandelt eingehende Text-Nachrichten des Websockets
websocket_handle({text, Msg}, Req, State) ->
	MessageNr = string:sub_string(binary_to_list(Msg), 1, 2),
	lager:info("incomming message : ~p", [Msg]),
	if
		MessageNr == "10"->
						   generate_new_uuids();
		MessageNr == "20"->
						   mine_hash(),	
						   find_winner();
		MessageNr == "30"->lager:info("reset"),
						   generate_new_uuids(),
						   RelayMessage = lists:flatten(io_lib:format("30:~s", ["RESET"])),
						   relay_message(RelayMessage, State#player.init_uuid);
		MessageNr == "40"->lager:info("stop"),
						   RelayMessage = lists:flatten(io_lib:format("40:~s", ["STOP"])),
						   relay_message(RelayMessage, State#player.init_uuid);
		true -> lager:error("unknown message")
	end,
	{ok, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

%% http://stackoverflow.com/questions/7763745/best-way-to-print-out-mnesia-table
show_all_players()->
	lager:info("players in game..."),
	Iterator =  fun(Rec,_)-> lager:info("~p~n",[Rec]),[]end,
	case mnesia:is_transaction() of
		true -> mnesia:foldl(Iterator,[],player);
		false -> 
			Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
			mnesia:activity(transaction,Exec,[{Iterator,player}],mnesia_frag)
	end.

generate_new_uuids()->
	lager:info("generate new uuids..."),
	Iterator =  fun(Player,_) ->  
						UUID = uuid:to_string(uuid:v4()),
						UUIDasMessage = lists:flatten(io_lib:format("10:~s:~s", [Player#player.init_uuid, UUID])),
						Player2 = Player#player {uuid = UUID},
						mnesia:dirty_write(Player2),
						relay_message(UUIDasMessage, Player#player.init_uuid),
						[]end,
	case mnesia:is_transaction() of
		true -> mnesia:foldl(Iterator,[],player);
		false -> 
			Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
			mnesia:activity(transaction,Exec,[{Iterator,player}],mnesia_frag)
	end.

mine_hash()->
	lager:info("mine..."),
	Iterator =  fun(Player,_) ->  
						{Hash, Count} = my_sha:generate_hash(Player#player.uuid, crypto:hash_init(md5), 0),
						NewScore = Player#player.score + Count,
						Player2 = Player#player {current_hash = Hash, current_score = Count, score = NewScore },
						mnesia:dirty_write(Player2),
						RelayMessage = lists:flatten(io_lib:format("20:~s:~s:~p:~p", [Player2#player.init_uuid, Hash, Player2#player.score, Count])),
						relay_message(RelayMessage, Player2#player.init_uuid),
						[]end,
	case mnesia:is_transaction() of
		true -> mnesia:foldl(Iterator,[],player);
		false -> 
			Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
			mnesia:activity(transaction,Exec,[{Iterator,player}],mnesia_frag)
	end.

find_winner()->
	lager:info("find winner..."),
	Query = fun() ->
					qlc:e(
					  qlc:q([X || X <- mnesia:table(player)])
						 )
			end,
	{atomic, Records} = mnesia:transaction(Query),
	Player = find_min_score(Records),
	RelayMessage = lists:flatten(io_lib:format("50:~s", [Player#player.init_uuid])),
	relay_message(RelayMessage, Player#player.init_uuid).

find_min_score([H|T]) -> find_min_score(H, T).

find_min_score(Winner, []) -> Winner;
find_min_score(Winner, [H|T]) -> 
%% 	lager:info("winner -> ~p ", [Winner#player.score]),
	if H#player.score =< Winner#player.score
		-> find_min_score(H, T);
	true -> find_min_score(Winner, T)
	end.
		
%% Behandelt eingehende Nachrichten anderer Erlang-Prozesse
websocket_info({{init_uuid, Name}, {message, Message}}, Req, State) when erlang:is_list(Message) andalso erlang:is_list(Name) ->
	CompleteMessage = lists:flatten(io_lib:format("~s", [Message])),
	{reply, {text, erlang:list_to_binary(CompleteMessage)}, Req, State};
websocket_info({{init_uuid, Name}, {message, Message}}, Req, State) when erlang:is_binary(Message) ->
	ListMessage = erlang:binary_to_list(Message),
	websocket_info({{init_uuid, Name}, {message, ListMessage}}, Req, State);
websocket_info({{init_uuid, Name}, {message, Message}}, Req, State) when erlang:is_binary(Name) ->
	ListName = erlang:binary_to_list(Name),
	websocket_info({{init_uuid, ListName}, {message, Message}}, Req, State).

%% Wird aufgerufen, wenn die Verbindung geschlossen wird
websocket_terminate(_Reason, _Req, State) ->
	lager:info("delete client ~p ", [State]),
	mnesia:dirty_delete(player, self()),
	DeleteClientMessage = lists:flatten(io_lib:format("60:~s:~p:~p", [State#player.init_uuid, State#player.score, 0])),
	relay_message(DeleteClientMessage, State#player.init_uuid).

%% Laedt alle Clients und startet die Nachrichtenvermittlung
relay_message(Msg, Name) ->
	Players = mnesia:dirty_all_keys(player),
	relay_message(Msg, Name, Players).

%% Leitet die Nachrichten an alle Clients weiter
relay_message(_Msg, _Name, []) ->
	ok;
relay_message(Msg, Name, [Player|Rest]) ->
	Player ! {{init_uuid, Name}, {message, Msg}},
	relay_message(Msg, Name, Rest).

%% send messages with generated uuid
%% relay_message_uuid(Msg, Name) ->
%% 	Players = mnesia:dirty_all_keys(player),
%% 	relay_message_uuid(Msg, Name, Players).
%% 
%% relay_message_uuid(_Msg, _Name, []) ->
%% 	ok;
%% relay_message_uuid(Msg, Name, [Player|Rest]) ->
%% 	case Player =:= self() of
%% 		false ->
%% 			Player ! {{init_uuid, Name}, {message, Msg}};
%% 		_ ->
%% 			ok
%% 	end,
%% 	relay_message(Msg, Name, Rest).