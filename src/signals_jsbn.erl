-module(signals_jsbn).

-include_lib("public_key/include/public_key.hrl").

-record(test, {vA, xA, yA, vB, xB, yB, xS, curve}).

-export([test/0,test_data/0]).

test_data() ->
	{<<4,Xa:256,Ya:256>>, <<Va:256>>} = crypto:generate_key(ecdh, secp256r1),
	{<<4,Xb:256,Yb:256>>, <<Vb:256>>} = crypto:generate_key(ecdh, secp256r1),
	{Va,Xa,Ya,Vb,Xb,Yb}.

test_secret_X(PrivateA, PublicA, PrivateB, PublicB, _Xs, Curve) ->
	SecretA = crypto:compute_key(ecdh, PublicB, PrivateA, Curve),
	io:fwrite("Secret A: ~p~n", [SecretA]),
	SecretB = crypto:compute_key(ecdh, PublicA, PrivateB, Curve),
	io:fwrite("Secret B: ~p~n", [SecretB]),
	ok.

test_public_point(V, _X, _Y, Curve) ->
	{Public, Private} = crypto:generate_key(ecdh,Curve,V),
	io:fwrite("Public: ~p~n", [Public]),
	{Public, Private}.

test_case({Va, Xa, Ya, Vb, Xb, Yb, Xs, Curve}) ->
	io:fwrite("Test case for curve ~p~n", [Curve]),
	{PublicA, PrivateA} = test_public_point(Va, Xa, Ya, Curve),
	{PublicB, PrivateB} = test_public_point(Vb, Xb, Yb, Curve),
	test_secret_X(PrivateA, PublicA, PrivateB, PublicB, Xs, Curve),
	ok.

test() ->
	Fun = fun(Data) -> 
		test_case(Data) 
	end,
	lists:map(Fun, test_data()). 

%point_to_public(R, S) when is_list(R) and is_list(S) ->
%	X = list_to_integer(R),
%	Y = list_to_integer(S),
%	ok.


