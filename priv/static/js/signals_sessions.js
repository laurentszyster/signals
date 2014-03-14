// TODO: use secp160r1 or move to SHA256

var CURVE = jsbn.ec.curve("secp256r1");

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
