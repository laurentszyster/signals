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
