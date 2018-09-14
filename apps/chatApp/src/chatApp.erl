-module(chatApp).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(CLIENT_ID, "<<pc_client_id>>").

-export([start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([subscribe/2, publish/3]).

-record(state, {
  recipient :: pid(),
  host = "localhost" :: inet:ip_address() | string(),
  port = 1883 :: inet:port_number()
}).

start(_StartType, _StartArgs) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

init(_Arg0) ->
  {ok, C} = emqttc:start_link([
    {host, "localhost"},
    {port, 1883},
    {username, <<"pratima">>},
    {password, <<"password">>}]),

  {ok, #state{recipient = C}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({subscribe, Topic, QoS}, State = #state{recipient = C}) ->
  emqttc:subscribe(C, list_to_binary([Topic]), QoS),
  {noreply, State};

handle_cast({publish, Topic, Message, QoS}, State = #state{recipient = C}) ->
  emqttc:publish(C, list_to_binary([Topic]), list_to_binary([Message]), QoS),
  {noreply, State}.

handle_info({mqttc, C, connected}, State = #state{recipient = C}) ->
  io:format("Client ~p is connected~n", [C]),
  {noreply, State};

handle_info({publish, Topic, Payload}, State) ->
  io:format("Message from ~s: ~p~n", [Topic, Payload]),
  {noreply, State}.

subscribe(Topic, Qos) ->
  gen_server:cast(?MODULE, {subscribe, Topic, Qos}).

publish(Topic, Message, Qos) ->
  gen_server:cast(?MODULE, {publish, Topic, Message, Qos}).