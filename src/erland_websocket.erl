%%% @doc Erland WebSocket server.
-module(erland_websocket).
-behaviour(cowboy_websocket).

-include("./erland.hrl").

-export([init/2]).
-export([
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

%% --------------------------------------
%% Following functions are for WebSocket.
%% --------------------------------------

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Raw}, State) ->
    % Send request to handlers.
    Request = jiffy:decode(Raw, [return_maps]),
    erland_request:handle(Request, self()),

    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

%% --------------------------------------------------------
%% Following functions are for receiving handler responses.
%% --------------------------------------------------------
websocket_info({{command, _Command}, Id, ok}, State) ->
    {[{text, ?PAYLOAD_OK(Id)}], State};
%% -------------------------------------------------------
%% Following functions are for receiving "create" responses.
%% -------------------------------------------------------
websocket_info({{command, create}, Id, {error, unique}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Not allowed to take this name">>),
    {[{text, Payload}], State};
websocket_info({{command, create}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Playground with given name already exists">>),
    {[{text, Payload}], State};
%% ---------------------------------------------------------------
%% Following functions are for receiving "set" (update) responses.
%% ---------------------------------------------------------------
websocket_info({{command, set}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Failed to set file contents due to issues in filesystem">>),
    {[{text, Payload}], State};
%% ------------------------------------------------------
%% Following functions are for receiving "run" responses.
%% ------------------------------------------------------
websocket_info({{command, run}, Id, {data, Data}}, State) ->
    Payload = ?PAYLOAD_PACKET(Id, Data),
    {[{text, Payload}], State};
websocket_info({{command, run}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"App failed to run">>),
    {[{text, Payload}], State};
%% ---------------------------------------------------------
%% Following functions are for receiving "delete" responses.
%% ---------------------------------------------------------
websocket_info({{command, delete}, Id, {error, _Reason}}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Failed to delete playground">>),
    {[{text, Payload}], State};
%% -------------------------------------------------
%% Following functions are for receiving edge cases.
%% -------------------------------------------------
websocket_info({fallback, Id}, State) ->
    Payload = ?PAYLOAD_ERR(Id, <<"Playground with given name not exists">>),
    {[{text, Payload}], State};
websocket_info(_Info, State) ->
    {ok, State}.
