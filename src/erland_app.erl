-module(erland_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", erland_websocket, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(erland_web, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),

    erland_sup:start_link().

stop(_State) ->
    ok.
