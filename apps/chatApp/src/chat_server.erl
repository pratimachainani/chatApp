-module(chat_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(CLIENT_ID, "client_id").

-export([start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([subscribe/3, publish/4]).

-record(state, {
  recipient :: pid(),
  host = "localhost" :: inet:ip_address() | string(),
  port = 1883 :: inet:port_number(),
  seq
}).

start(_StartType, _StartArgs) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  chatApp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

init(_Arg0) ->
  {ok, C} = emqttc:start_link([
    {host, "localhost"},
    {port, 1883},
    {client_id, ?CLIENT_ID},
    {username, <<"pratima">>},
    {password, <<"password">>}]),

  %% The pending subscribe
%%  emqttc:subscribe(C, <<"TopicA">>, 1),
  {ok, #state{recipient = C, seq = 1}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call({subscribe, Topic, QoS}, _From, State = #state{recipient = C}) ->
  emqttc:subscribe(C, list_to_binary([Topic]), QoS),
  {reply, ok, State};

handle_call({publish, Topic, Message, QoS}, _From, State = #state{recipient = C}) ->
  emqttc:publish(C, list_to_binary([Topic]), list_to_binary([Message]), QoS),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({mqttc, C, connected}, State = #state{recipient = C}) ->
  io:format("Client ~p is connected~n", [C]),
  {noreply, State};

handle_info({subscribe, Topic, QoS}, State = #state{recipient = C}) ->
  emqttc:subscribe(C, list_to_binary([Topic]), QoS),
  {noreply, State};

%% Receive Messages
handle_info({publish, Topic, Payload}, State) ->
  io:format("Message from ~s: ~p~n", [Topic, Payload]),
  {noreply, State}.

%subscribe
subscribe(Pid, Topic, Qos) ->
  io:format("In subscribe, Topic: ~p, QoS: ~p Pid: ~p ~n", [Topic, Qos, Pid]),
  gen_server:call(?MODULE, {subscribe, Topic, Qos}).

%publish
publish(Pid, Topic, Message, Qos) ->
  io:format("In publish, Topic: ~p, Message: ~p QoS: ~p Pid: ~p ~n", [Topic, Message, Qos, Pid]),
  gen_server:call(?MODULE, {publish, Topic, Message, Qos}).