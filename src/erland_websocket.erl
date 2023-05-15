-module(erland_websocket).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

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

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Raw}, State) ->
    Request = jiffy:decode(Raw, [return_maps]),
    erland_request:handle(Request, self()),

    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({{command, _Command}, Id, ok}, State) ->
    {[{text, ?PAYLOAD_OK(Id)}], State};
websocket_info({{command, create}, Id, {error, unique}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Not allowed to take this name">>),
    {[{text, Payload}], State};
websocket_info({{command, create}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Playground with given name already exists">>),
    {[{text, Payload}], State};
websocket_info({{command, set}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Failed to set file contents due to issues in filesystem">>),
    {[{text, Payload}], State};
websocket_info({{command, run}, Id, {data, Data}}, State) ->
    Payload = ?PAYLOAD_PACKET(Id, Data),
    {[{text, Payload}], State};
websocket_info({{command, run}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"App failed to run">>),
    {[{text, Payload}], State};
websocket_info({{command, delete}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Failed to delete playground">>),
    {[{text, Payload}], State};
websocket_info({fallback, Id}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Playground with given name not exists">>),
    {[{text, Payload}], State};
websocket_info(_Info, State) ->
    {ok, State}.
