!function ui(window, document) {
	var self, 
		signalsControl = document.getElementById("signalsControl");
	function trace(text) {
		console.log((performance.now()/1000).toFixed(3) + ": " + text);
	}
	function ui_local_media_ok(stream){
		trace("Received local stream");
		window.attachMediaStream(signalsControl, stream)
		signalsControl.style.display = "block";
		self.localStream = stream; // TODO ...
	}
	function ui_local_media_error(error){
		trace("navigator.getUserMedia error: " + error);
	}
	function ui_start(signaling, videoStream) {
		trace("Requesting local stream");
		window.getUserMedia({
			video: (videoStream||true), 
			audio: true
		}, ui_local_media_ok, ui_local_media_error);
	}
	self = window.ui = {
		start: ui_start,
		localStream: null
	}
}(window, document);
