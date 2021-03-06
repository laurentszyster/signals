/* signals.js 

	JavaScript API for Signals

	Requires: Modernizr-2.6.2, sjcl

*/

!(function (window, navigator, Modernizr, webrtc, jsbn) {
	// Test the browser first ...
	function log (line) {
		console.log(line);
	}
	if(!(Modernizr.localstorage && webrtc(window, navigator, log))) {
		return; // here redirect ...
	}
	// LocalStorage and WebRTC supported, let's roll with scope declarations
	var CURVE = jsbn.ec.curve("secp256r1");

	// Private functions first ...
	function pass() {}
	function getURIParts(uri) {
		var m = uri.match(/^(http|https):\/\/([^\/:]+)(:[^\/]+)?(.+)$/);
		return {
			protocol: m[1],
			host: m[2],
			port: (m[3] || ":80"),
			path: (m[4] || "/")
		};
	}
	function getWSParts(uri) {
		var m = uri.match(/^(ws|wss):\/\/([^\/:]+)(:[^\/]+)?\/sockets\/(.+)$/);
		return {
			protocol: m[1],
			host: m[2],
			port: (m[3] || ":80"),
			key: m[4]
		};
	}
	function localSet(collection, key, string) {
		return window.localStorage.setItem(["Signals", collection, key].join("/"), string);
	}
	function localGet(collection, key) {
		return window.localStorage.getItem(["Signals", collection, key].join("/"));
	}
	function localSetJSON(collection, key, value) {
		var s = JSON.stringify(value);
		return localSet(collection, key, s);
	}
	function localGetJSON(collection, key) {
		return JSON.parse(localGet(collection, key)); // in effect ...||"null"))
	}

	// Then prototypes ...

	// Session
	function Session(key) {
		this.id = key;
		var json = localGetJSON("Sessions", key);
		if (json) {
			this.privateKeys = json.privateKeys;
			this.publicKey = json.publicKey;
			this.sharedSecret = json.sharedSecret;
		} else {
			this.privateKeys = jsbn.ec.generate(CURVE);
			this.publicKey = null;
		}
		this.challenge = jsbn.ec.random(CURVE).toString();
	}
	Session.prototype.store = function() {
		localSetJSON("Sessions", this.id, this);
	}
	Session.prototype.identify = function (publicKey) {
		var x = publicKey[0], y = publicKey[1];
		this.publicKey = publicKey;
		this.sharedSecret = jsbn.ec.computeKey(CURVE, this.privateKeys.v, x, y);
		this.store();
	}
	Session.prototype.sign = function(challenge) {
		return jsbn.hmac(this.sharedSecret, challenge);
	}
	Session.prototype.verify = function (signature) {
		return this.sign(this.challenge) == signature;
	}
	Session.prototype.isIdentified = function () {
		return this.publicKey != null;
	}
	Session.prototype.authenticate = function (message) {
		if (this.verify(message.signature)) {
			return {"signature": this.sign(message.challenge)};
		} else {
			return "unauthorized";
		}
	}
	function Socket(uri) {
		uri = getWSParts(uri);
		this.session = new Session(uri.key);
		this.baseURI = uri.protocol + "://" + uri.host + uri.port + "/sockets/";
		this.ws = null;
	}
	Socket.prototype.toString = function() {
		return this.baseURI;
	}
	Socket.prototype.log = function(line) {
		log(this.toString() + " " + line)
	}
	Socket.prototype.connect = function(signaling) {
		var ws, self=this;
		function socket_open() {
			self.ws = ws;
			if (self.session.isIdentified()) {
				ws.send(JSON.stringify({
					"challenge": self.session.challenge
					}));
				ws.onmessage = socket_authenticate;
			} else {
				var P = self.session.privateKeys;
				ws.send(JSON.stringify({
					"publicKey": [P.x, P.y]
					}));
				ws.onmessage = socket_identify;
			}
		}
		function socket_exception(e) {
			self.log(e);
			ws.onmessage = pass;
			ws.close();
		}
		function socket_identify(evt) {
			try {
				signaling.onConnect(self);
				self.session.identify(JSON.parse(evt.data).publicKey);
				ws.onmessage = socket_authorized;
				signaling.onReady(self);
			} catch (e) {
				socket_exception(e)
			}
		}
		function socket_authenticate(evt) {
			try {
				signaling.onConnect(self);
				var req = JSON.parse(evt.data),
					res = self.session.authenticate(req);
				if (res == "unauthorized") {
					signaling.onUnauthorized(self);
					ws.onmessage = pass;
					ws.close();
				} else {
					ws.send(JSON.stringify(res));
					ws.onmessage = socket_authorized;
					signaling.onReady(self);
				}
			} catch (e) {
				socket_exception(e);
			}
		}
		function socket_authorized(evt) {
			try {
				var message = JSON.parse(evt.data);
			} catch (e) {
				message = {"error": e.toString()};
			}
			try {
				signaling.onSignal(self, message);
			} catch (e) {
				socket_exception(e);
			}
		}
		function socket_reconnect() {
			socket_close();
			self.connect();
		}
		function socket_close() {
			signaling.onDisconnect(self);
			signaling = self.ws = ws = null;
		}
		ws = new WebSocket(this.baseURI+this.session.id);
		ws.onclose = socket_close;
		ws.onopen = socket_open;
		return this;
	}
	Socket.prototype.isConnected = function() {
		return this.ws !== null;
	}
	//
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

})(window, navigator, Modernizr, webrtc, jsbn);