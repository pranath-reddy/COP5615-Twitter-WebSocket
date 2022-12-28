%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-4 Part-2
%% Mock Twitter-Engine Websockets

-module(project4_ws_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", server_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    project4_ws_sup:start_link().

stop(_State) ->
	ok.
