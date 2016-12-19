"use strict";
var socket = new WebSocket("ws://localhost:5555/websocket");

function init() {
	console.log("init client");
	if (!("WebSocket" in window)) {
//		console.log("websockets not supported");
		document.getElementById("status").innerHTML = "<p><span style=\"color: red;\">websockets are not supported </span></p>";
	} else {
//		console.log("websockets are supported");
		document.getElementById("status").innerHTML = "<p><span style=\"color: green;\">websockets are supported </span></p>";
	}
};

init();

// from
// callback-Funktion wird gerufen, wenn die Verbindung erfolgreich aufgebaut
// werden konnte
socket.onopen = function() {
//	console.log("Verbindung wurde erfolgreich aufgebaut");
	document.getElementById("status").innerHTML = "<p><span style=\"color: green;\">connection is ok</span></p>";
};

// callback-Funktion wird gerufen, wenn eine neue Websocket-Nachricht eintrifft
socket.onmessage = function(messageEvent) {
	console.log(messageEvent.data);
};

// callback-Funktion wird gerufen, wenn eine Fehler auftritt
socket.onerror = function(errorEvent) {
//	console.log("Error! Die Verbindung wurde unerwartet geschlossen");
	document.getElementById("status").innerHTML = "<p><span style=\"color: red;\">error! connection lost</span></p>";
};

socket.onclose = function(closeEvent) {
//	console.log('Die Verbindung wurde geschlossen --- Code: ' + closeEvent.code
//			+ ' --- Grund: ' + closeEvent.reason);
	document.getElementById("status").innerHTML = '<p><span style=\"color: red;\">Connection closed --- Code: '
			+ closeEvent.code + ' reason: ' + closeEvent.reason + '</span></p>';
};