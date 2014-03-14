__provide__("base64", function(GLOBALS){
	/** @fileOverview Bit array codec implementations.
	 *
	 * @author Emily Stark
	 * @author Mike Hamburg
	 * @author Dan Boneh
	 */
	__require__("bitArray");
	/** The base64 alphabet.
	* @private
	*/
	_chars: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
	/** Convert from a bitArray to a base64 string. */
	function fromBits (arr, _noEquals, _url) {
		var out = "", i, bits=0, c = _chars, ta=0, bl = bitArray.bitLength(arr);
		if (_url) c = c.substr(0,62) + '-_';
		for (i=0; out.length * 6 < bl; ) {
			out += c.charAt((ta ^ arr[i]>>>bits) >>> 26);
			if (bits < 6) {
				ta = arr[i] << (6-bits);
				bits += 26;
				i++;
			} else {
				ta <<= 6;
				bits -= 6;
			}
		}
		while ((out.length & 3) && !_noEquals) { out += "="; }
		return out;
	}
	/** Convert from a base64 string to a bitArray */
	function toBits (str, _url) {
		str = str.replace(/\s|=/g,'');
		var out = [], i, bits=0, c = _chars, ta=0, x;
		if (_url) c = c.substr(0,62) + '-_';
		for (i=0; i<str.length; i++) {
			x = c.indexOf(str.charAt(i));
			if (x < 0) {
				throw "this isn't base64!";
			}
			if (bits > 26) {
				bits -= 26;
				out.push(ta ^ x>>>bits);
				ta  = x << (32-bits);
			} else {
				bits += 6;
				ta ^= x << (32-bits);
			}
		}
		if (bits&56) {
			out.push(bitArray.partial(bits&56, ta, 1));
		}
		return out;
	}
	return {
		"fromBits": fromBits,
		"toBits": toBits,
		fromUrlBits: function (arr) { return fromBits(arr,1,1); },
		toUrlBits: function (str) { return toBits(str,1); }
	};
// Tern JSON Specification
}, {
	"base64": {
		"fromBits": "fn(string, boolean, boolean) -> string",
		"toBits": "fn(string, boolean) -> string",
		"fromUrlBits": "fn(string) -> string",
		"toUrlBits": "fn(string) -> string"
	}
});