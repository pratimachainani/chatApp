%%%-------------------------------------------------------------------
%% @doc chatApp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chatApp_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  io:format("In chatApp_sup:start_link"),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  io:format("In chatApp_sup:init"),
  SupervisorFlags = #{strategy => one_for_all, intensity => 0, period => 5},
  ChildSpecs = [#{id => chatApp, start => {chatApp, start, [[],[]]}, restart => permanent}],
  {ok, {SupervisorFlags, ChildSpecs}}.
