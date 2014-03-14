-record(signals_install, {
	key, 
	secret, 
	index_installed, 
	webworker_installed
	}).

-record(signals_session, {
	key,
	socket,
	curve,
	privateKeys,
	publicKey,
	sharedSecret,
	names
}).

-record(signals_name,{name, sessionKey}).