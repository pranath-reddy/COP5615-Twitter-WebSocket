%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-4 Part-2
%% Mock Twitter-Engine Websockets

-module(project4_ws_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
