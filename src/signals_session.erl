-module(signals_session).

-include("signals.hrl").

-export([create/1]).
-export([identify/2]).
-export([challenge/0]).
-export([sign/2]).
-export([verify/3]).
-export([add_name/2]).

-export([is_identified/1]).
-export([get_names/1]).
-export([get_id/1]).
-export([get_public/1]).
-export([get_private/1]).
-export([get_curve/1]).

-define(HASH, sha).
-define(CURVE, secp256r1).
-define(CHALLENGE_BYTES, 32).

key(Name) ->
	signals_hex:from_bin(crypto:hmac(?HASH, Name, crypto:rand_bytes(32))).

create(Name) ->
	Key = key(Name),
	PrivateKeys = crypto:generate_key(ecdh, ?CURVE),
	#signals_session{
		key=Key,
		curve=?CURVE,
		privateKeys=PrivateKeys,
		names=[Name]
	}.

curveBitLength(secp256r1) -> 
	256.

identify(Session, PublicKey) ->
	Curve = get_curve(Session),
	BitLength = curveBitLength(Curve),
	{_, PrivateKey} = get_private(Session),
	<<Secret:BitLength>> = crypto:compute_key(ecdh, PublicKey, PrivateKey, Curve),
	Session#signals_session{
		publicKey=PublicKey,
		sharedSecret=list_to_binary(integer_to_list(Secret))
	}.

add_name(Session, Name) ->
	Names=[Name|Session#signals_session.names],
	Session#signals_session{names=Names}.

challenge() ->
	base64:encode(crypto:rand_bytes(?CHALLENGE_BYTES)).

sign(Session, Data) ->
	Key = Session#signals_session.sharedSecret,
	signals_hex:from_bin(crypto:hmac(?HASH, Key, Data)).

verify(Session, Data, HexHash) ->
	case sign(Session, Data) of
		HexHash -> true;
		_ -> false
	end.

%% Accessors

is_identified(Session) ->
	Session#signals_session.publicKey==undefined.

get_names(Session) ->
	Session#signals_session.names.

get_id(Session) ->
	Session#signals_session.key.

get_private(Session) ->
	Session#signals_session.privateKeys.

get_public(Session) ->
	Session#signals_session.publicKey.

get_curve(Session) ->
	Session#signals_session.curve.