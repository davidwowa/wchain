"use strict";
var socket = new WebSocket("ws://localhost:5555/hashchain");

function init() {
	console.log("init client");
	if (!("WebSocket" in window)) {
		// console.log("websockets not supported");
		document.getElementById("status").innerHTML = "websockets are not supported";
		document.getElementById("status").className = "status_block_error";
	} else {
		// console.log("websockets are supported");
		document.getElementById("status").innerHTML = "websockets are supported";
		document.getElementById("status").className = "status_block_ok";
	}
};

init();

socket.onopen = function() {
	document.getElementById("status").innerHTML = "connection is open";
	document.getElementById("status").className = "status_block_ok";
};

// callback-Funktion wird gerufen, wenn eine neue Websocket-Nachricht eintrifft
socket.onmessage = function(messageEvent) {
	var message = messageEvent.data;
	if (message.substring(0, 2) == "10") {
		console.log("get random uuid " + message);
		document.getElementById("dataUUID0").innerHTML = message.substring(3,
				message.length);
	
		var div = document.createElement("div");
		div.id = "uuid" + message;
		div.className = "clients_block_uuid";
		div.innerHTML = "<span class=\"msg\">" + message + " UUID</span>";
		document.getElementById("clients").appendChild(div);
	} else if (message.substring(0, 2) == "20") {
		document.getElementById("buttonUuid").disabled = true;
		document.getElementById("buttonReset").disabled = false;
		
		console.log("mined hash " + message);
		document.getElementById("dataHash0").innerHTML = message.substring(3,
				message.length);

		var div = document.createElement("div");
		div.id = "mine" + message;
		div.className = "clients_block_hash";
		div.innerHTML = "<span class=\"msg\">" + message + " HASH</span>";
		document.getElementById("clients").appendChild(div);
	} else if (message.substring(0, 2) == "30") {
		console.log("reset " + message);
		document.getElementById("buttonUuid").disabled = false;
		document.getElementById("buttonReset").disabled = false;
		document.getElementById("buttonStop").disabled = false;
		document.getElementById("buttonMine").disabled = false;
		
		var div = document.createElement("div");
		div.id = "reset" + message;
		div.className = "clients_block_reset";
		div.innerHTML = "<span class=\"msg\">" + message + " RESET</span>";
		document.getElementById("clients").appendChild(div);
	} else if (message.substring(0, 2) == "40") {
		console.log("stop " + message);
		document.getElementById("buttonUuid").disabled = true;
		document.getElementById("buttonReset").disabled = true;
		document.getElementById("buttonStop").disabled = true;
		
		var div = document.createElement("div");
		div.id = "stop" + message;
		div.className = "clients_block_stop";
		div.innerHTML = "<span class=\"msg\">" + message + " STOP</span>";
		document.getElementById("clients").appendChild(div);
	} else if (message.substring(0, 2) == "50") {
		console.log("winner " + message);
		
		var div = document.createElement("div");
		div.id = "winner" + message;
		div.className = "clients_block_hash";
		div.innerHTML = "<span class=\"msg\">" + message + " WINNER</span>";
		document.getElementById("clients").appendChild(div);
	
	} else if (message.substring(0, 2) == "60") {
		console.log("update all clients " + message);
		var div = document.createElement("div");
		div.id = "test" + messageEvent.data;
		if (message.substring(message.length - 1, message.length) == "1") {
			div.className = "clients_block_online";
			div.innerHTML = "<span class=\"msg\">" + message + " ONLINE</span>";
		} else {
			div.className = "clients_block_offline";
			div.innerHTML = "<span class=\"msg\">" + message
					+ " OFFLINE</span>";
		}
		document.getElementById("clients").appendChild(div);
	} else {
		console.log("unknown message " + message);
	}
};

// callback-Funktion wird gerufen, wenn eine Fehler auftritt
socket.onerror = function(errorEvent) {
	// console.log("Error! Die Verbindung wurde unerwartet geschlossen");
	document.getElementById("status").innerHTML = "<p><span style=\"color: red;\">error! connection lost</span></p>";
};

socket.onclose = function(closeEvent) {
	// console.log('Die Verbindung wurde geschlossen --- Code: ' +
	// closeEvent.code
	// + ' --- Grund: ' + closeEvent.reason);
	document.getElementById("status").innerHTML = 'Connection closed, Code: '
			+ closeEvent.code + ' reason: ' + closeEvent.reason + '</span></p>';
	document.getElementById("status").className = "status_block_error";
};

// game logic
function generateUUID() {
	console.log("Generate uuid");
	if (socket.readyState == socket.OPEN) {
		console.log("10: get random UUID");
		socket.send("10");
		
		var div = document.createElement("div");
		div.className = "clients_block_uuid";
		div.innerHTML = "<span class=\"msg\">SEND GENERATE UUID</span>";
		document.getElementById("clients").appendChild(div);
	} else {
		console.log('websocket is not connected');
	}
}

function mine() {
	console.log("mine");
	if (socket.readyState == socket.OPEN) {
		console.log("mine");
		socket.send("20: " + document.getElementById("dataUUID0").innerHTML);
		document.getElementById("buttonMine").disabled = true;
	
		document.getElementById("buttonReset").disabled = false;
		var div = document.createElement("div");
		div.className = "clients_block_hash";
		div.innerHTML = "<span class=\"msg\">BEGINNING WITH MINING</span>";
		document.getElementById("clients").appendChild(div);
	} else {
		console.log('websocket is not connected');
	}
}

function reset() {
	console.log("reset");
	if (socket.readyState == socket.OPEN) {
		console.log("reset");
		socket.send("30");
		document.getElementById("buttonUuid").disabled = false;
		document.getElementById("buttonReset").disabled = false;
		document.getElementById("buttonStop").disabled = false;
		document.getElementById("buttonMine").disabled = false;
		
		var div = document.createElement("div");
		div.className = "clients_block_reset";
		div.innerHTML = "<span class=\"msg\">MY RESET</span>";
		document.getElementById("clients").appendChild(div);
	} else {
		console.log('websocket is not connected');
	}
}

function stop() {
	console.log("reset");
	if (socket.readyState == socket.OPEN) {
		console.log("reset");
		socket.send("40");
		document.getElementById("buttonUuid").disabled = true;
		document.getElementById("buttonReset").disabled = true;
		document.getElementById("buttonStop").disabled = true;
		
		var div = document.createElement("div");
		div.className = "clients_block_stop";
		div.innerHTML = "<span class=\"msg\">MY STOP</span>";
		document.getElementById("clients").appendChild(div);
	} else {
		console.log('websocket is not connected');
	}
}