%%%-------------------------------------------------------------------
%%% @author islamov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2017 11:16
%%%-------------------------------------------------------------------
-module(tcp_handler_SUITE).
-author("isalmov").
-include_lib("common_test/include/ct.hrl").

%% API
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         normal_operation_test/1,
         master_close_test/1,
         additional_close_test/1
        ]).

all() ->
    [
     normal_operation_test,
     master_close_test,
     additional_close_test
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

-define(Packet1, <<"abcd">>).
-define(Packet2, <<"wxyz">>).
-define(Packet3, <<"1234">>).
-define(Packet4, <<"7890">>).
-define(Packet5, <<"KLMN">>).
-define(Packet6, <<"OPQR">>).

normal_operation_test(_Config) ->
    %% Open sockets
    Listeners = listen([7771, 7772, 7773]),
    {ok, DeviceSocket1} = gen_tcp:connect("127.0.0.1", 7770, [binary, {active, false}]),
    Sockets1 = accept(Listeners),
    {ok, DeviceSocket2} = gen_tcp:connect("127.0.0.1", 7770, [binary, {active, false}]),
    Sockets2 = accept(Listeners),

    %% Send packet1 from device1
    ok = gen_tcp:send(DeviceSocket1, ?Packet1),
    [ {ok, ?Packet1} = gen_tcp:recv(Socket, 0, 500) || Socket <- Sockets1 ],
    {error, timeout} = gen_tcp:recv(DeviceSocket1, 0, 0),

    %% Send packet2 from device2
    ok = gen_tcp:send(DeviceSocket2, ?Packet2),
    [ {ok, ?Packet2} = gen_tcp:recv(Socket, 0, 500) || Socket <- Sockets2 ],
    {error, timeout} = gen_tcp:recv(DeviceSocket2, 0, 0),

    %% Send packet3 to device1
    ok = gen_tcp:send(hd(Sockets1), ?Packet3),
    {ok, ?Packet3} = gen_tcp:recv(DeviceSocket1, 0, 500),
    [ {error, timeout} = gen_tcp:recv(Socket, 0, 0) || Socket <- Sockets1 ],

    %% Send packet4 to device2
    ok = gen_tcp:send(hd(Sockets2), ?Packet4),
    {ok, ?Packet4} = gen_tcp:recv(DeviceSocket2, 0, 500),
    [ {error, timeout} = gen_tcp:recv(Socket, 0, 0) || Socket <- Sockets2 ],

    %% Send packet5 from additional to device1
    ok = gen_tcp:send(hd(tl(Sockets1)), ?Packet5),
    {error, timeout} = gen_tcp:recv(DeviceSocket1, 0, 100),
    [ {error, timeout} = gen_tcp:recv(Socket, 0, 0) || Socket <- Sockets1 ],

    %% Send packet6 from additional to device2
    ok = gen_tcp:send(hd(tl(Sockets2)), ?Packet6),
    {error, timeout} = gen_tcp:recv(DeviceSocket1, 0, 100),
    [ {error, timeout} = gen_tcp:recv(Socket, 0, 0) || Socket <- Sockets2 ],

    %% Close sockets
    ok = gen_tcp:close(DeviceSocket1),
    [ {error, closed} = gen_tcp:recv(Socket, 0, 500) || Socket <- Sockets1 ],
    ok = gen_tcp:close(DeviceSocket2),
    [ {error, closed} = gen_tcp:recv(Socket, 0, 500) || Socket <- Sockets2 ],
    ok.

master_close_test(_Config) ->
    %% Open sockets
    Listeners = listen([7771, 7772, 7773]),
    {ok, DeviceSocket} = gen_tcp:connect("127.0.0.1", 7770, [binary, {active, false}]),
    Sockets = accept(Listeners),
    [MasterSocket | _] = Sockets,

    %% Close master socket
    ok = gen_tcp:close(MasterSocket),
    {error, closed} = gen_tcp:recv(DeviceSocket, 0, 1000),
   [ {error, closed} = gen_tcp:recv(Socket, 0, 1000) || Socket <- Sockets ],
    ok.

additional_close_test(_Config) ->
    %% Open sockets
    Listeners = listen([7771, 7772, 7773]),
    {ok, DeviceSocket} = gen_tcp:connect("127.0.0.1", 7770, [binary, {active, false}]),
    Sockets = accept(Listeners),
    [_ | AddSockets] = Sockets,

    %% Close first additional socket
    ok = gen_tcp:close(hd(AddSockets)),

    %% Send packet from device
    ok = gen_tcp:send(DeviceSocket, ?Packet1),
    [{ok, ?Packet1}, {error, closed}, {ok, ?Packet1}] = [ gen_tcp:recv(Socket, 0) || Socket <- Sockets ],
    {error, timeout} = gen_tcp:recv(DeviceSocket, 0, 0),

    %% Close sockets
    ok = gen_tcp:close(DeviceSocket),
    [ {error, closed} = gen_tcp:recv(Socket, 0, 1000) || Socket <- Sockets ],
    ok.

listen(Ports) ->
    [ Socket || {ok, Socket} <- [ gen_tcp:listen(Port, [binary,{active, false}, {reuseaddr,true}]) || Port <- Ports ] ].

accept(Listeners) ->
    [ element(2, gen_tcp:accept(Listener, 100)) || Listener <- Listeners ].



