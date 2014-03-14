function Address(name) {
	this.name = name;
	this.sessions = [];
	this.calls = [];
}

function PeerConnection(address, session) {
	this.address = address;
	this.session = session;
}
PeerConnection.prototype.onOffer = function (signal) {}
PeerConnection.prototype.onAnswer = function (signal) {}
PeerConnection.prototype.onConnect = function () {}
PeerConnection.prototype.onPlay = function () {}
PeerConnection.prototype.onHold = function () {} 
PeerConnection.prototype.onDisconnect = function () {}

// Signals

function Signals() {
	var keys = localGetJSON("Collections", "Sessions") || [],
		sessions = this.sessions = {};
	keys.map(function(key){
		sessions[key] = true;
	});
	this.sockets = localGetJSON("Collections", "Sockets") || {};
	this.installation = localGet("Signals", "installation");
	this.localStream = null;
	this.calls = {};
}
Signals.prototype.openSession = function(key) {
	var session;
	if (this.sessions[key]) {
		session = new Session(key);
	} else {
			session = new Session(key);
		session.store();
		localSetJSON("Collections", "Sessions", Object.keys(this.sessions));
	}
	return session;
}
Signals.prototype.openSocket = function(uri) {
	var socket = new Socket(uri);
	if (!this.sockets[socket.baseURI]) {
		this.sockets[socket.baseURI] = uri;
		localSetJSON("Signals", "sockets", this.sockets);
	}
	return socket.connect(this);
}
Signals.prototype.onError = function(socket, error) {
	socket.log(error);
	// alert the user. 
}
Signals.prototype.onDisconnect = function(socket) {
	socket.log("disconnected");
	// alert the user, reconnect. 
}
Signals.prototype.onConnect = function(socket) {
	socket.log("connected");
	// alert the user. 
}
Signals.prototype.onReady = function(socket) {
	socket.log("ready");
	// alert the user. 
}
Signals.prototype.onSignal = function(socket, message) {
	socket.log(JSON.stringify(message));
	// dispatch offers and answers.
}
Signals.prototype.call = function(address, session) {
	var call = this.calls[address] = new PeerConnection(address, session);
	return call;
}
function signals_install(key) {
	var session =  new Session(key),
		uriParts = getURIParts(window.document.baseURI),
	 	sockets = {};
	localSet("Signals","installation", key);
	localSetJSON("Collections", "Sessions", [key]);
	session.store();
	sockets[uriParts.host+uriParts.port] = key;
	localSetJSON("Signals", "sockets", sockets);
	return session;
}
function signals_open(key) { // the main entry point
	var session, s = new Signals();
	if (s.installation == null) {
		session = signals_install(key);
		s.installation = key;
	} else {
		session = s.sessions[s.installation];
	}
	// session.connect();
	return s;
}
function signals_clean() {
	var key = localGet("Signals","installation");
	localStorage.removeItem("Signals/Collections/Sessions");
	localStorage.removeItem("Signals/Signals/Sockets");
	localStorage.removeItem("Signals/Signals/installation");
	localStorage.removeItem("Signals/Sessions/" + key);
	// localStorage.removeItem("Signals/Sockets/" + key);
}
function signals_post(path, name, from, message) {
	var xhr = new XMLHttpRequest(), 
		json = JSON.stringify(message),
		uri = path+"/"+name+"/"+from;
	xhr.open("POST", uri, true);
	xhr.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
	xhr.send(json);
	xhr.onreadystatechange = function(evt) {
		if(xhr.readyState==4) {
			(log||onresponse)(xhr.status);
		}
	};
	// 
	console.log("POST "+uri+" "+ json.length.toString() + " chars");
    console.log(json);
}
// tests

window.signals = {
	version: "0.0.1",
	open: signals_open,
	clean: signals_clean,
	post: signals_post
};
