 
 %% serves the web application webworker for each installation
-module(signals_webworker_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{ok, Req3} = signals_handle_webworker(Method, Req2),
	{ok, Req3, State}.

signals_handle_webworker(<<"GET">>, Req) ->
	%% if the application has been installed once from this URL, return 304 Not Modified
	{Install, _} = cowboy_req:binding(install, Req),
	case signals_db:get_install(Install) of
		not_found ->
			cowboy_req:reply(404, Req);
		{_, _, _, true} ->
			cowboy_req:reply(304, Req);
		{_, Secret, _, false} ->
			Headers = [
				{<<"content-type">>, <<"text/javascript">>},
				{<<"cache-control">>, <<"private">>},	
				{<<"etag">>, <<Install,".js">>}
			],
			{ok, Body} = signals_webworker_dtl:render([
				{install, Install},
				{secret, Secret}
			]),
			cowboy_req:reply(200, Headers, Body, Req)
	end;
signals_handle_webworker(_, Req) ->
	cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.