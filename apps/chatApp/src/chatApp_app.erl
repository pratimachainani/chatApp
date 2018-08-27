%%%-------------------------------------------------------------------
%% @doc chatApp public API
%% @end
%%%-------------------------------------------------------------------

-module(chatApp_app).

-behaviour(application).

-define(HOST,"localhost").
-define(PORT,1883).
-define(CLIENT_ID,"client_id").

%% Application callbacks
-export([start/2, stop/1]).
-export([connect/2, connect/5, joinGroup/1, sendMessage/2]).


-record(state, {recipient           :: pid(),
                name                :: atom(),
                host = "localhost"  :: inet:ip_address() | string(),
                port = 1883         :: inet:port_number(),
                subscribers = []    :: list(),
                ping_reqs   = []    :: list(),
                keepalive           :: emqttc_keepalive:keepalive() | undefined,
                logger              :: gen_logger:logmod()}).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    chatApp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

connect(Username,Password) ->
  {ok,C} = emqttc:start_link([{host,?HOST},{port,?PORT},{username,Username},{password, Password},{client_id, ?CLIENT_ID}, {logger,console,info}]),
  emqttc:subscribe(C, <<"/topic">>, 1),
  {ok, #state{recipient = C}}.

connect(Host, Port, Username, Password, ClientId) ->
%%  {ok,C} = io:format("Client[~s] connecting to MQTT Broker running at, ~s:~s with credentials {~s,~s}", [ClientId, Host, Port, Username, Password]),
{ok,C} = emqttc:start_link([{host,Host},{port,Port},{username,Username},{password, Password},{client_id, ClientId}, {logger,console,info}]),
emqttc:subscribe(C, <<"/topic">>, 1),
  {ok, #state{recipient = C}}.

joinGroup(GroupName) -> 
%%====== join a group with groupName essentially translates to client_id subscribing to a topic(GroupName)=========
%%====== check if connection to broker is established, Y: subscribe and print msg to console, N: print error message that user has to wait/retry and call connect
emqttc:subscribe(#state.recipient, GroupName, qos0).
%%io:format('This will allow user to subscribe to a topic, if for some reason it`s not possible, user might have to retry').

sendMessage(GroupName,Message) -> 
%%======= sendMessage essentially translates to publishing a message(Message) to a topic(GroupName)========
%%======= check if connection to broker is established, Y: publish and print msg to console, N: print error message that user has to wait/rety and call connect
 emqttc:publish(#state.recipient, GroupName, Message, [{qos, 0}]).
%%  io:format('This will allow user to publish a message to a topic, if for some reason it`s not possible, user might have to retry').

%%isConnected() -> io:format('state: ~p', #state).

%%====================================================================
%% Internal functions
%%====================================================================
