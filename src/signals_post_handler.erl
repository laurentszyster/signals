%% handle POST and returns JSON
-module(signals_post_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(HEADERS, [{<<"connection">>, <<"close">>}]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req2),
	{ok, Req3} = signals_handle_post(Method, HasBody, Req2),
	{ok, Req3, State}.

signals_handle_post(<<"POST">>, true, Req) ->
	{Name, _} = cowboy_req:binding(name, Req), 
	case signals_db:get_socket(Name) of 
		undefined ->
			cowboy_req:reply(204, ?HEADERS, <<>>, Req);
		not_found ->
			cowboy_req:reply(404, ?HEADERS, <<>>, Req);
		Socket ->
			{[From], _} = cowboy_req:path_info(Req),
			{ok, Json, Req2} = cowboy_req:body(Req), 
			Socket ! {signal, Json, From},
			cowboy_req:reply(200, ?HEADERS, <<>>, Req2)
	end; 
signals_handle_post(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, ?HEADERS, <<>>, Req);
signals_handle_post(_, _, Req) ->
	cowboy_req:reply(405, ?HEADERS, <<>>, Req).

terminate(_Reason, _Req, _State) ->
	ok.