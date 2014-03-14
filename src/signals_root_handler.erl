%% serves the root of Signals: splash screen with the service state.
-module(signals_root_handler).

-export([init/3]).
-export([allowed_methods/0]).
-export([content_types_provided/2]).
-export([signals_root_html/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods() -> [<<"GET">>].

content_types_provided(Req, State) ->
	{[{<<"text/html">>, signals_root_html}], Req, State}.

signals_root_html(Req, State) -> 
	{Host, Req2} = cowboy_req:host(Req),
	{ok, Body} = signals_root_dtl:render([
		{hostname, Host}
	]),
	{Body, Req2, State}.