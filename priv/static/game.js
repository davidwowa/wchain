"use strict";
var socket = new WebSocket("ws://localhost:5555/hashchain");

var my_uuid = guid();
var my_cookie = "uuid=" + my_uuid;

function init() {
	console.log("init client with uuid " + my_uuid);
	document.cookie = my_cookie;
	if (!("WebSocket" in window)) {
		document.getElementById("status").innerHTML = "websockets are not supported";
		document.getElementById("status").className = "status_block_error";
	} else {
		document.getElementById("status").innerHTML = "websockets are supported";
		document.getElementById("status").className = "status_block_ok";
	}
	// set inital data
	document.getElementById("youUUID").innerHTML = my_uuid;
	document.getElementById("dataUUID0").innerHTML = my_uuid
};

init();

socket.onopen = function() {
	document.getElementById("status").innerHTML = "connection ok";
	document.getElementById("status").className = "status_block_ok";
};

// callback-Funktion wird gerufen, wenn eine neue Websocket-Nachricht eintrifft
socket.onmessage = function(messageEvent) {
	var message = messageEvent.data;
	if (message.substring(0, 2) == "10") {
		console.log(message);
		if (my_uuid.valueOf() == message.substring(3, 39).valueOf()) {
			document.getElementById("dataUUID0").innerHTML = message.substring(
					(message.length - 36), message.length);
		}
		// --
		var div = document.createElement("div");
		div.id = "uuid" + message;
		div.className = "clients_block_uuid";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime() + "|"
				+ message + "</span>";
		// document.getElementById("clients").appendChild(div);
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else if (message.substring(0, 2) == "20") {
		document.getElementById("buttonUuid").disabled = true;
		document.getElementById("buttonReset").disabled = false;
		console.log(message);
		document.getElementById("dataHash0").innerHTML = message.substring(3,
				message.length);
		// --
		var div = document.createElement("div");
		div.id = "mine" + message;
		div.className = "clients_block_hash";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime() + "|"
				+ message + "</span>";
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else if (message.substring(0, 2) == "30") {
		console.log(message);
		document.getElementById("buttonUuid").disabled = false;
		document.getElementById("buttonReset").disabled = false;
		document.getElementById("buttonStop").disabled = false;
		document.getElementById("buttonMine").disabled = false;
		// --
		var div = document.createElement("div");
		div.id = "reset" + message;
		div.className = "clients_block_reset";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime() + "|"
				+ message + "</span>";
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else if (message.substring(0, 2) == "40") {
		console.log(message);
		document.getElementById("buttonUuid").disabled = true;
		document.getElementById("buttonReset").disabled = true;
		document.getElementById("buttonStop").disabled = true;
		// --
		var div = document.createElement("div");
		div.id = "stop" + message;
		div.className = "clients_block_stop";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime() + "|"
				+ message + "</span>";
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else if (message.substring(0, 2) == "50") {
		console.log(message);
		// --
		document.getElementById("buttonUuid").disabled = true;
		document.getElementById("buttonReset").disabled = false;
		document.getElementById("buttonStop").disabled = true;
		document.getElementById("buttonMine").disabled = true;

		var div = document.createElement("div");
		div.id = "winner" + message;
		div.className = "winner";

		if (my_uuid == message.substring(3, message.length)) {
			div.innerHTML = "<span class=\"msg\">!!! YOU WIN !!!</span>";
		} else {
			div.innerHTML = "<span class=\"msg\"> Winner is : "
					+ message.substring(3, message.length) + "</span>";
		}
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else if (message.substring(0, 2) == "60") {
		console.log(message);
		var div = document.createElement("div");
		div.id = "client_update" + messageEvent.data;
		if (message.substring(message.length - 1, message.length) == "1") {
			div.className = "clients_block_online";
			div.innerHTML = "<span class=\"msg\">" + new Date().getTime() + "|"
					+ message + " ONLINE</span>";
		} else {
			div.className = "clients_block_offline";
			div.innerHTML = "<span class=\"msg\">" + new Date().getTime() + "|"
					+ message + " OFFLINE</span>";
		}
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else {
		div.className = "clients_block_offline";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime() + "|"
				+ message + " UNKNOWN </span>";

		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
		console.log("unknown message " + message);
	}
};

// callback-Funktion wird gerufen, wenn eine Fehler auftritt
socket.onerror = function(errorEvent) {
	document.getElementById("status").innerHTML = "<p><span style=\"color: red;\">error! connection lost</span></p>";
};

socket.onclose = function(closeEvent) {
	document.getElementById("status").innerHTML = 'Connection closed, Code: '
			+ closeEvent.code + ' reason: ' + closeEvent.reason + '</span></p>';
	document.getElementById("status").className = "status_block_error";
};

// game buttons
function generateUUID() {
	console.log("Generate uuid");
	if (socket.readyState == socket.OPEN) {
		console.log("10: get random UUID");
		socket.send("10");

		var div = document.createElement("div");
		div.className = "clients_block_uuid";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime()
				+ " GENERATE UUID...</span>";
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else {
		console.log('websocket is not connected');
	}
}

function mine() {
	console.log("mine");
	if (socket.readyState == socket.OPEN) {
		console.log("20: mine");
		socket.send("20: " + document.getElementById("dataUUID0").innerHTML);
		document.getElementById("buttonMine").disabled = true;

		document.getElementById("buttonReset").disabled = false;
		var div = document.createElement("div");
		div.className = "clients_block_hash";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime()
				+ " MINING...</span>";
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else {
		console.log('websocket is not connected');
	}
}

function reset() {
	console.log("reset");
	if (socket.readyState == socket.OPEN) {
		console.log("30: reset");
		socket.send("30");
		document.getElementById("buttonUuid").disabled = false;
		document.getElementById("buttonReset").disabled = false;
		document.getElementById("buttonStop").disabled = false;
		document.getElementById("buttonMine").disabled = false;

		var div = document.createElement("div");
		div.className = "clients_block_reset";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime()
				+ " RESET...</span>";
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else {
		console.log('websocket is not connected');
	}
}

function stop() {
	console.log("reset");
	if (socket.readyState == socket.OPEN) {
		console.log("40: reset");
		socket.send("40");
		document.getElementById("buttonUuid").disabled = true;
		document.getElementById("buttonReset").disabled = true;
		document.getElementById("buttonStop").disabled = true;

		var div = document.createElement("div");
		div.className = "clients_block_stop";
		div.innerHTML = "<span class=\"msg\">" + new Date().getTime()
				+ " STOP</span>";
		document.getElementById("log").insertBefore(div,
				document.getElementById("log").firstChild);
	} else {
		console.log('websocket is not connected');
	}
}

// http://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
function guid() {
	function s4() {
		return Math.floor((1 + Math.random()) * 0x10000).toString(16)
				.substring(1);
	}
	return s4() + s4() + '-' + s4() + '-' + s4() + '-' + s4() + '-' + s4()
			+ s4() + s4();
}