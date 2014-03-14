/* __import__.js 

Yeah, import this !

Just one step over requirejs capabilities, one step shorter of a new language.

Define a new namespace in lib/module.js:

	__provide__("module", function (scope){ 
		return {};
	}); 

Declare a new Proto function in lib/Proto.js:

	__provide__("Proto", function (scope){ 
		function Proto () {};
	}); 

Extend a packaged prototype with a new function in lib/Proto/prototype/fun.js:

	__provide__("Proto/prototype/fun", function (scope){ 
		return function fun () {};
	}); 

Maybe specify types:

	__provide__("module/factory", function (scope) {
		return function () {
			
		}
	}, {});

And, everywhere, you can now __require__ stuff before jumping into action.

	__require__(path, ...)
	Object.__require__(path, ...)
	Object.prototype.__require__(path, ...)

And still be able to just load and execute scripts synchronously:

	__import__(path, ...)


*/
var undefined, pass = function () {};
function isPrototype (o) { 
	return o === o.constructor.prototype;
}
__import__ = (function (GLOBALS) {
	var isWebWorker = (typeof importScripts !== "undefined"), 
		REQUIRED = {}, MANIFEST = [], LOADED = [], CURRENTSCRIPT;
	function importRoot (o) {
		return isPrototype(o) ? o.constructor.name + ".prototype" : (o.name || "this");
	}
	function importPath (o) {
		return o.__path__ ? o.__path__ : importRoot(o);
	}
	function providerName (o) {
		return isPrototype(o) ? o.constructor.name + ".prototype" : (o.name || "this");
	}
	function manifestLine (root, path, url) {
		return root + ".__provide__(" + JSON.stringify(path) + ", " + JSON.stringify(url) + ");";
	}
	function Package (parent, name, path) { 
		this.parent = parent;
		this.name = name;
		this.__package__ = path;
		this.__modules__ = {};
	}
	Object.prototype.__provide__ = function (fqn, module) {
		var self = this, 
			names = fqn.split('.'),
			T = names.length-1,
			path = [importRoot(this)],
			name;
		for (var i=0 ; i < T; i++) {
			name = names[i];
			path.push(name);
			if (!self.hasOwnProperty(name)) {
				if (!self.__modules__.hasOwnProperty(name)) {
					self.__modules__[name] = function () { return {}; }
				}
				self[name] = new Package(self, name, path.join("."));
			self = self[name];
		}
		name = names[i]
		if (!self.hasOwnProperty("__modules__")) {
			self.__modules__ = {};
		}
		self.__modules__[name] = module;
		if (typeof module === "function" && CURRENTSCRIPT !== undefined) {
			MANIFEST.push(manifestLine(path[0], fqn, CURRENTSCRIPT));
		}
	}
	function load (url) {
		CURRENTSCRIPT = url;
		if (isWebWorker) {
			importScripts([url]);
		} else {
			var request = new XMLHttpRequest();
			request.open('GET', url, false);
			request.send(null);
			if (request.status === 200) {
				(new Function(request.responseText)).apply(GLOBALS);
				LOADED.push(url);
			} else { // or fail ...
				throw Error(request.status.toString() + ', failed to load ' + url);
			}
		}
		CURRENTSCRIPT = undefined;
	}
	function require (scope, path, name, modules) {
		var qfn = path + "." + name;
		if (REQUIRED[qfn] === true) {
			throw Error("Circular requirements on " + qfn);
		}
		REQUIRED[qfn] = true;
		if (modules !== undefined && modules.hasOwnProperty(name)) {
			var module = modules[name];
			if (typeof module === "string") {
				delete modules[name];
				load(module); // load or fail ...
				if (modules[name] === undefined) {
					throw Error(path + ' requires ' + name + ' from ' + module);
				}
			}
			return modules[name](scope);
		} else {
			throw Error(path + ' requires ' + name);
		}
	}
	Object.prototype.__require__ = function __require__ () {
		var provider = providerName(this), 
			modules = this.__modules__, 
			names = arguments.length === 0 ? Object.keys(modules) : arguments,
			required = [];
		for (var name, required, i=0 ; name=names[i] ; i++) {
			if (!this.hasOwnProperty(name) || this[name].constructor === Package) {
				this[name] = require(this, provider, name, modules);
			}
			required.push(this[name]);
		}
		return required;
	}
	Package.prototype.__require__ = function () {
		var self = this, 
			name = this.name, 
			parent = this.parent, 
			modules = this.__modules__,
			path = this.__path__;
		delete parent[name];
		self = parent.__require__(name)[0];
		self.__modules__ = modules;
		self.__path__ = path;
		return Object.prototype.__require__.apply(self, arguments);
	}
	function __import__ (urls, callback) {
		if (isWebWorker) {
			return importScripts(urls, callback);
		}
		var count = urls.length, head = document.getElementsByTagName("head")[0];
		CURRENTSCRIPT = urls[0];
		urls.forEach(function (url, i) {
			var script = document.createElement("script");
			script.type = "text/javascript";
			script.charset = 'utf-8';
			script.async = false;
			script.src = url;
			script.addEventListener("load", function () {
				LOADED.push(url);
				CURRENTSCRIPT = urls[i+1];
				count--;
				if (count === 0) {
					(callback || pass)();
				}
			}, false)
			script.addEventListener("error", function () {
				CURRENTSCRIPT = urls[i+1];
				count--;
				if (count === 0) {
					(callback || pass)();
				}
			}, false)
			head.insertBefore(script, null);
		})
	}
	__import__.loaded = function () { 
		return LOADED;
	}
	__import__.manifest = function () { 
		return MANIFEST;
	}
	__import__.required = function () { 
		return REQUIRED;
	}
	__import__.bundle = function () {
		((new Function()).apply(GLOBALS);	
	}
	return __import__;
})(this)