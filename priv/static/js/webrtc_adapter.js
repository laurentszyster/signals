function webrtc(window, navigator, log) {
  if (navigator.mozGetUserMedia) {
    log("This appears to be Firefox");
    window.webrtcDetectedBrowser = "firefox";
    window.webrtcDetectedVersion =
                    parseInt(navigator.userAgent.match(/Firefox\/([0-9]+)\./)[1]);
    // The RTCPeerConnection object.
    window.RTCPeerConnection = window.mozRTCPeerConnection;
    // The RTCSessionDescription object.
    window.RTCSessionDescription = window.mozRTCSessionDescription;
    // The RTCIceCandidate object.
    window.RTCIceCandidate = window.mozRTCIceCandidate;
    // Get UserMedia (only difference is the prefix).
    // Code from Adam Barth.
    window.getUserMedia = navigator.mozGetUserMedia.bind(navigator);
    // Creates iceServer from the url for FF.
    window.createIceServer = function(url, username, password) {
      var iceServer = null;
      var url_parts = url.split(':');
      if (url_parts[0].indexOf('stun') === 0) {
        // Create iceServer with stun url.
        iceServer = { 'url': url };
      } else if (url_parts[0].indexOf('turn') === 0 &&
                 (url.indexOf('transport=udp') !== -1 ||
                  url.indexOf('?transport') === -1)) {
        // Create iceServer with turn url.
        // Ignore the transport parameter from TURN url.
        var turn_url_parts = url.split("?");
        iceServer = { 'url': turn_url_parts[0],
                      'credential': password,
                      'username': username };
      }
      return iceServer;
    };
    // Attach a media stream to an element.
    window.attachMediaStream = function(element, stream) {
      log("Attaching media stream");
      element.mozSrcObject = stream;
      element.play();
    };
    window.reattachMediaStream = function(to, from) {
      log("Reattaching media stream");
      to.mozSrcObject = from.mozSrcObject;
      to.play();
    };
    // Fake get{Video,Audio}Tracks
    window.MediaStream.prototype.getVideoTracks = function() {
      return [];
    };
    window.MediaStream.prototype.getAudioTracks = function() {
      return [];
    };
  } else if (navigator.webkitGetUserMedia) {
    log("This appears to be Chrome");
    window.webrtcDetectedBrowser = "chrome";
    window.webrtcDetectedVersion =
               parseInt(navigator.userAgent.match(/Chrom(e|ium)\/([0-9]+)\./)[2]);
    // Creates iceServer from the url for Chrome.
    window.createIceServer = function(url, username, password) {
      var iceServer = null;
      var url_parts = url.split(':');
      if (url_parts[0].indexOf('stun') === 0) {
        // Create iceServer with stun url.
        iceServer = { 'url': url };
      } else if (url_parts[0].indexOf('turn') === 0) {
        if (webrtcDetectedVersion < 28) {
          // For pre-M28 chrome versions use old TURN format.
          var url_turn_parts = url.split("turn:");
          iceServer = { 'url': 'turn:' + username + '@' + url_turn_parts[1],
                        'credential': password };
        } else {
          // For Chrome M28 & above use new TURN format.
          iceServer = { 'url': url,
                        'credential': password,
                        'username': username };
        }
      }
      return iceServer;
    };
    // The RTCPeerConnection object.
    window.RTCPeerConnection = window.webkitRTCPeerConnection;
    // Get UserMedia (only difference is the prefix).
    // Code from Adam Barth.
    window.getUserMedia = navigator.webkitGetUserMedia.bind(navigator);
    // Attach a media stream to an element.
    window.attachMediaStream = function(element, stream) {
      if (typeof element.srcObject !== 'undefined') {
        element.srcObject = stream;
      } else if (typeof element.mozSrcObject !== 'undefined') {
        element.mozSrcObject = stream;
      } else if (typeof element.src !== 'undefined') {
        element.src = URL.createObjectURL(stream);
      } else {
        log('Error attaching stream to element.');
      }
    };
    window.reattachMediaStream = function(to, from) {
      to.src = from.src;
    };
    // The representation of tracks in a stream is changed in M26.
    // Unify them for earlier Chrome versions in the coexisting period.
    if (!window.webkitMediaStream.prototype.getVideoTracks) {
      window.webkitMediaStream.prototype.getVideoTracks = function() {
        return this.videoTracks;
      };
      window.webkitMediaStream.prototype.getAudioTracks = function() {
        return this.audioTracks;
      };
    }
    // New syntax of getXXXStreams method in M26.
    if (!window.webkitRTCPeerConnection.prototype.getLocalStreams) {
      window.webkitRTCPeerConnection.prototype.getLocalStreams = function() {
        return this.localStreams;
      };
      window.webkitRTCPeerConnection.prototype.getRemoteStreams = function() {
        return this.remoteStreams;
      };
    }
  } else {
    log("Browser does not appear to be WebRTC-capable");    
    return false;
  }
  return true;
}