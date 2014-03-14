-module(signals_websocket_handler).
-include("signals.hrl").
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	{Key, _} = cowboy_req:binding(key, Req),
	io:fwrite("signals_websocket_handler:websocket_init/3 ~p~n", [Key]), 
	%% link the session to this websocket's pid
	case signals_db:get_session(Key) of
		not_found ->
			{shutdown, Req, undefined};
		Session -> 
			{ok, Req, {unauthorized, Session}}
	end.

websocket_handle({text, Msg}, Req, {Auth, Session}) ->
	io:fwrite("signals_websocket_handler:websocket_handle/3 ~p~n", [Msg]), 
	%% handle client's key, authentication and challenge or close on state error
	case signals_mochijson2:decode(Msg) of
		{struct, Proplist} -> 
			handle_json(Auth, Session, Req, Proplist);
		_ ->
			io:fwrite("signals_websocket_handler:websocket_handle/3 Not a JSON Object~n"), 
			{shutdown, Req, {Auth, Session}}
	end
	;
websocket_handle(_Data, Req, State) ->
	%% drop binaries
	{ok, Req, State}.

websocket_info(Info, Req, State) ->
	case Info of
		{signal, Json, From} ->
			io:fwrite("signals_websocket_handler:websocket_info/3 ~p~n", []), 
			Msg = iolist_to_binary([
				<<"{\"signal\":">>, Json,
				<<",\"from\":">>, signals_mochijson2:encode(From),
				<<"}">>
			]),
			{reply, {text, Msg}, Req, State};
		_ -> 
			io:fwrite("signals_websocket_handler:websocket_info/3 Not a signal~n", []), 
			{ok, Req, State}
	end.

websocket_terminate(_Reason, Req, State) ->
	{Key, _} = cowboy_req:binding(key, Req),
	io:fwrite("signals_websocket_handler:websocket_terminate/3 ~p~n", [Key]), 
	%% unlink the session from this web socket
	case State of 
		{authorized, Session} ->
			signals_db:disconnect_session(Session),
			ok;
		_ ->
			ok 
	end.

%%

json_proplist(Properties) ->
	iolist_to_binary(signals_mochijson2:encode({struct, Properties})).

%% handshake

handle_json(authorized, Session, Req, Properties) ->
	io:fwrite("handle_json/3 ~p~n", [Properties]), 
	%% TODO, handle the rest of the protocol ...
	{ok, Req, {authorized, Session}};
handle_json(unauthorized, Session=#signals_session{publicKey=undefined}, Req, Properties) ->
	%% identify
	case proplists:get_value(<<"publicKey">>, Properties) of
		undefined -> 
			io:fwrite("handle_json/3 publicKey is undefined~n"), 
			{shutdown, Req, {unidentified, Session}};
		[XBs, YBs] when is_binary(XBs) and is_binary(YBs) ->
			%% transcode own Public Key from ECPoint to X and Y integers as JSON strings
			{ECPointA, _} = signals_session:get_private(Session),
			BitLength = (byte_size(ECPointA)-1)*4,
			<<4,XA:BitLength,YA:BitLength>> = ECPointA,
			Message = json_proplist([
				{publicKey, [
					list_to_binary(integer_to_list(XA)),
					list_to_binary(integer_to_list(YA))
					]}
			]),
			%% transcode the given X and Y integer strings into an ECPoint binary 
			XB = list_to_integer(binary_to_list(XBs)), 
			YB = list_to_integer(binary_to_list(YBs)), 
			io:fwrite("X : ~p~nY : ~p~n", [XB, YB]), 
			ECPointB = <<4,XB:BitLength,YB:BitLength>>,
			%% identify and connect
			Session1 = signals_session:identify(Session, ECPointB),
			Session2 = signals_db:connect_session(Session1, self()),
			io:fwrite("Connected : ~p~n", [Session2]), 
			{reply, {text, Message}, Req, {authorized, Session2}};
		Obj -> 
			io:fwrite("handle_json/3 not a valid publicKey ~p~n", [Obj]), 
			{shutdown, Req, {unauthorized, Session}}
	end;
handle_json(unauthorized, Session, Req, Properties) ->
	%% authenticate
	case proplists:get_value(<<"challenge">>, Properties) of
		undefined -> 
			io:fwrite("handle_json/3 challenge is undefined~n"), 
			{shutdown, Req, {unauthorized, Session}};
		ClientChallenge when is_binary(ClientChallenge) > 31 ->
			Challenge = signals_session:challenge(),
			Signature = signals_session:sign(Session, ClientChallenge),
			Message = json_proplist([
				{challenge, Challenge},
				{signature, Signature}
			]),
			{reply, {text, Message}, Req, {Challenge, Session}};
		Obj -> 
			io:fwrite("handle_json/3 not a valid challenge ~p~n", [Obj]), 
			{shutdown, Req, {unauthorized, Session}}
	end;
handle_json(Challenge, Session, Req, Properties) ->
	%% authorize
	case proplists:get_value(<<"signature">>, Properties) of
		undefined -> 
			io:fwrite("handle_json/3 signature is undefined~n"), 
			{shutdown, Req, {error, Session}};
		HexHash when is_binary(HexHash) ->
			case signals_session:verify(Session, Challenge, HexHash) of 
				true ->
					Session1 = signals_db:connect_session(Session, self()), 
					{ok, Req, {authorized, Session1}};
				false -> 
					io:fwrite("handle_json/3 incorrect signature~n"), 
					{shutdown, Req, {unauthorized, Session}}
			end;
		Obj -> 
			io:fwrite("handle_json/3 not a valid signature ~p~n", [Obj]), 
			{shutdown, Req, {unauthorized, Session}}
	end.
