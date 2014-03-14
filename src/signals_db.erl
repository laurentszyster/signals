-module(signals_db).

-include("signals.hrl").

-export([open/0, open/1, close/0]).
-export([create_install/1]).
-export([get_install/1]).
-export([write_session/1]).
-export([get_session/1]).
-export([name_session/2]).
-export([connect_session/2]).
-export([disconnect_session/1]).
-export([get_socket/1]).

%% utils

-define(CREATE_TABLE(Table, NodesList),create_table(Table, record_info(fields, Table), NodesList)).
create_table(Name, Attributes, NodesList) ->
	case mnesia:create_table(Name, [{disc_copies, NodesList}, {attributes, Attributes}]) of
		{atomic, ok} ->
		    io:format("signals_db:create_table/3 ~p created~n", [Name]);
		{aborted, {already_exists, _}} ->
		    io:format("signals_db:create_table/3 ~p already exists~n",[Name])
	end.

create_index(Name, AttributeName) ->
	case mnesia:add_table_index(Name, AttributeName) of
		{atomic, ok} ->
		    io:format("signals_db:create_index/2 ~p.~p created~n", [Name, AttributeName]);
		{aborted, {already_exists, _, _}} ->
		    io:format("signals_db:create_index/2 ~p.~p already exists~n",[Name, AttributeName])
	end.

%% API

-spec open() -> ok.
open()->
	open([node()]).

-spec open([node()]) -> ok.
open(NodesList) ->
	case mnesia:create_schema(NodesList) of
		ok ->
		    io:format("signals_db:open/1 schema created~n");
		{error, {NodeAddress, {already_exists, _}}} ->
		    io:format("signals_db:open/1 schema already exists at node:~p~n",[NodeAddress])
	end,
	ok = mnesia:start(),
	ok = ?CREATE_TABLE(signals_session, NodesList),
	ok = ?CREATE_TABLE(signals_name, NodesList),
	ok = ?CREATE_TABLE(signals_install, NodesList),
	ok = mnesia:wait_for_tables([signals_install, signals_name, signals_session], 5000),
    io:format("signals_db:open/1 ok~n"),
    ok.

-spec close() -> ok.
close() -> 
	%% hangs application:stop/1    
	%% 
	%% ok = mnesia:stop(),
	%%                    
	%% see: http://stackoverflow.com/questions/3477489/stopping-of-erlang-app-hangs-when-mnesia-stopped-from-within-the-program
	%%
    io:format("signals_db:close/0 ok~n"),
	ok.

%% create a new installation record, a new session record and bind them to a name if it is not
%% already bound to a session.  
-spec create_install(binary()) -> 
	#signals_install{}.
create_install(Name) ->
	Fun = fun() ->
		case mnesia:read(signals_name, Name) of 
			[_Lock] ->
				already_exists;
			[] -> 
				Session = signals_session:create(Name),
				Key = Session#signals_session.key,
				mnesia:write(signals_session, Session, write),
				mnesia:write(signals_name, #signals_name{name=Name, sessionKey=Key}, write),
				Install = #signals_install{key=Key},
				mnesia:write(signals_install, Install, write),
				{Install, Session}
		end
	end,
	mnesia:activity(transaction, Fun).

%% get an existing installation record by key
-spec get_install(binary()) -> 
	#signals_install{}.
get_install(Key) ->
	Fun = fun() ->
		case mnesia:read(signals_install, Key) of 
			[Install] -> Install;
			[] -> not_found
		end
	end,
	mnesia:activity(transaction, Fun).

%% write a session record
-spec write_session(#signals_session{}) -> 
	#signals_session{}.
write_session(Session) ->
	Fun = fun() -> 
		mnesia:write(signals_session, Session, write)
	end,
	mnesia:activity(transaction, Fun).

%% get a session record by key
-spec get_session(binary()) -> 
	[]|[#signals_session{}].
get_session(Key) ->
	Fun = fun() ->
		case mnesia:read(signals_session, Key) of 
			[Session] ->
				Session;
			_ ->
				not_found
		end
	end,
	mnesia:activity(transaction, Fun).

%% connect a session to its websocket process
-spec connect_session(#signals_session{}, pid()) -> 
	#signals_session{}.
connect_session(Session, Pid) ->
	Fun = fun() ->
		Session1 = Session#signals_session{socket=Pid},
		mnesia:write(signals_session, Session1, write),
		Session1
	end,
	mnesia:activity(transaction, Fun).

%% add a name to a session
-spec name_session(#signals_session{}, binary()) -> 
	access_denied | #signals_session{}.
name_session(Session, Name) ->
	Fun = fun() ->
		case mnesia:read(signals_name, Name) of
			[] ->
				mnesia:write(signals_name, {Name, Session#signals_session.key}, write),
				signals_session:add_name(Session, Name);
			_ -> 
				access_denied
		end
	end,
	mnesia:activity(transaction, Fun).

%% disconnect a session from its websocket process
-spec disconnect_session(#signals_session{}) -> 
	#signals_session{}.
disconnect_session(Session) ->
	Fun = fun() ->
		Session2 = Session#signals_session{socket=undefined},
		mnesia:write(signals_session, Session2, write),
		Session2
	end,
	mnesia:activity(transaction, Fun).

%% get the a connection's websocket process by name
-spec get_socket(binary()) -> 
	pid() | undefined.
get_socket(Name) ->
	Fun = fun() ->
		case mnesia:read(signals_name, Name) of 
			[#signals_name{sessionKey=Key}] ->
				case mnesia:read(signals_session, Key) of
					[#signals_session{socket=Pid}] ->
						Pid
				end;
			_ -> not_found
		end
	end,
	mnesia:activity(transaction, Fun).
