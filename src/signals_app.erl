%% @private
-module(signals_app).
-behaviour(application).

%% signals application.
-export([start/2]).
-export([stop/1]).

start(_Type, Args) ->
	ok = signals_db:open(),
	Host = <<"127.0.0.1">>,
	Address = {127,0,0,1},
	Port = 8080,
	Acceptors = 100,
	Dispatch = cowboy_router:compile([
		{Host, [
			{"/signals/:name/[...]", signals_post_handler, []},
			{"/sockets/:key", signals_websocket_handler, []},
			{"/", signals_root_handler, []},
			{"/installs/:install/index.html", signals_index_handler, []},
			{"/installs/:install/webworker.js", signals_webworker_handler, []},
			{"/installs/:install/cache.manifest", signals_manifest_handler, []},
			{"/static/[...]", cowboy_static, [
				{directory, {priv_dir, signals, [<<"static">>]}},
				{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			]}
		]}
	]),
	case cowboy:start_http(
		http, Acceptors, [{ip, Address}, {port, Port}], [{env, [{dispatch, Dispatch}]}]
	) of
		{error, _} ->
		    io:format("signals HTTP server could not be started -- "
			      "address ~p not available or port ~p probably in use~n", [Address, Port]),
		    init:stop();
		{ok, _Pid} ->
		    io:format("signals HTTP server listening on ~p:~p~n",[Address, Port]),
		    signals_sup:start_link()
    end.

stop(State) ->
    io:format("signals HTTP server stop~n"),
	ok = signals_db:close(),
    ok.
