-module(erland_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Create /tmp/erland if not exists
    case filelib:is_dir("/tmp/erland") of
        false -> file:make_dir("/tmp/erland");
        true -> noop
    end,

    % Start web server
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
