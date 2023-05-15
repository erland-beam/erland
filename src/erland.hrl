%%% @doc Macros for Erland

%% ---------------------------------------------------
%% Following macros are for building response payload.
%% ---------------------------------------------------
-define(PAYLOAD_OK(Id),
    jiffy:encode(#{
        <<"id">> => Id,
        <<"type">> => 0
    })
).

-define(PAYLOAD_ERR(Id, Message),
    jiffy:encode(#{
        <<"id">> => Id,
        <<"type">> => 1,
        <<"data">> => Message
    })
).

-define(PAYLOAD_PACKET(Id, Message),
    jiffy:encode(#{
        <<"id">> => Id,
        <<"type">> => 2,
        <<"data">> => Message
    })
).

%% ----------------------------------------
%% Following macros are for Erlang manager.
%% ----------------------------------------
-define(MODULE_REBAR_CONFIG,
    "{erl_opts, [no_debug_info]}.\n"
    "{escript_incl_apps, [testing]}.\n"
    "{escript_main_app, testing}.\n"
    "{escript_name, testing}.\n"
    "{profiles, [{test, [{erl_opts, [debug_info]}]}]}.\n"
).

-define(MODULE_FILE_HEADER,
    "-module(testing).\n"
    "-export([main/1]).\n"
    "\n"
).

-define(MODULE_APP_SRC,
    "{application, testing, [\n"
    "  {description, \"Erland playground template for rebar3\"},\n"
    "  {vsn, \"0.0.0\"},\n"
    "  {registered, []},\n"
    "  {applications, [~s]},\n"
    "  {env, []},\n"
    "  {modules, []},\n"
    "  {licenses, []},\n"
    "  {links, []}\n"
    "]}."
).

-define(FILE_RUN_SH,
    "export COMPILED_BINARY=./_build/default/bin/testing;\n"
    "rm \\$COMPILED_BINARY >/dev/null 2>&1;\n"
    "rebar3 do compile, escriptize || { exit 1; };\n"
    "\\$COMPILED_BINARY;"
).

%% ----------------------------------------------------
%% Following macros are shared with more than one file.
%% ----------------------------------------------------
-define(PLAYGROUND_PATH(Name), ["/tmp/erland/" | binary_to_list(Name)]).
