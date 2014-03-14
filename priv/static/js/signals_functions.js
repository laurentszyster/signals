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