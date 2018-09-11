%%%-------------------------------------------------------------------
%% @doc chatApp public API
%% @end
%%%-------------------------------------------------------------------

-module(chatApp_app).

-behaviour(gen_server).

-define(CLIENT_ID, "client_id").

%% Application callbacks
-export([start/2, stop/1]).
-export([connect/5, joinGroup/1, sendMessage/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


-record(state, {recipient :: pid(),
  name :: atom(),
  host = "localhost" :: inet:ip_address() | string(),
  port = 1883 :: inet:port_number(),
  subscribers = [] :: list(),
  ping_reqs = [] :: list(),
  keepalive :: emqttc_keepalive:keepalive() | undefined,
  logger :: gen_logger:logmod()}).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    chatApp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

connect(Host, Port, Username, Password, ClientId) ->
%%  {ok,C} = io:format("Client[~s] connecting to MQTT Broker running at, ~s:~s with credentials {~s,~s}", [ClientId, Host, Port, Username, Password]),
  {ok, C} = emqttc:start_link([
    {host, Host},
    {port, Port},
    {username, Username},
    {password, Password},
    {client_id, ClientId},
    {logger, console, info}]),
  emqttc:subscribe(C, <<"/topic">>, 1),
  {ok, #state{recipient = C}}.

joinGroup(GroupName) ->
  emqttc:subscribe(C = #state.recipient, GroupName, qos0),
  {ok, #state{recipient = C}}.

sendMessage(GroupName, Message) ->
  io:format("C: ~p", [#state.recipient]),
  emqttc:publish(#state.recipient, list_to_binary([GroupName]), Message, [{qos, 1}]),
  erlang:send_after(3000, self(), publish).

init(_Args) ->
  {ok, C} = emqttc:connect(<<"pratima">>, <<"password">>),
  emqttc:subscribe(C, <<"/topic">>, 1),
  {ok, #state{recipient = C}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

%% Publish Messages
handle_info(publish, State = #state{recipient = C}) ->
  Payload = list_to_binary(["hello...", integer_to_list(2)]),
  emqttc:publish(C, <<"TopicA">>, Payload, [{qos, 1}]),
  erlang:send_after(3000, self(), publish),
  {noreply, State};

%% Publish Messages
handle_info({Topic, Message}, State = #state{recipient = C}) ->
  io:format("Publishing message: ~p to topic: ~p ", [Message, Topic]),
  Payload = list_to_binary(Message),
  BinaryTopic = list_to_binary(Topic),
  emqttc:publish(C, BinaryTopic, Payload, [{qos, 1}]),
  erlang:send_after(3000, self(), publish),
  {noreply, State};

%% Receive Messages
handle_info({publish, Topic, Payload}, State) ->
  io:format("Message from ~s: ~p~n", [Topic, Payload]),
  {noreply, State};

%% Client connected
handle_info({mqttc, C, connected}, State = #state{recipient = C}) ->
  io:format("Client ~p is connected~n", [C]),
  emqttc:subscribe(C, <<"TopicA">>, 1),
  emqttc:subscribe(C, <<"TopicB">>, 2),
  self() ! publish,
  {noreply, State};

%%Let's try subscribe
handle_info({subscribe, Topic}, State = #state{recipient = C}) ->
  io:format("Subscribing to topic ~p ", [Topic]),
  emqttc:subscribe(C, list_to_binary(Topic), 1),
  {noreply, State};

%% Client disconnected
handle_info({mqttc, C, disconnected}, State = #state{recipient = C}) ->
  io:format("Client ~p is disconnected~n", [C]),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.