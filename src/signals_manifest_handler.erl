%% serves the web application cache manifest for each installation
-module(signals_manifest_handler).

-export([init/3]).
-export([allowed_methods/0]).
-export([content_types_provided/2]).
-export([signals_manifest_txt/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods() -> [<<"GET">>].

content_types_provided(Req, State) ->
	{[{<<"text/cache-manifest">>, signals_manifest_txt}], Req, State}.

signals_manifest_txt(Req, State) ->
	%% returns a cache manifest
	{Install, _} = cowboy_req:binding(install, Req), 
	{ok, Body} = signals_manifest_dtl:render([
		{install, Install}
	]),
	{Body, Req, State}.
