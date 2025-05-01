-module(pingpong).
-export([ping/0, pong/0]).

ping() ->
    PongPid = spawn(pingpong, pong, []),
    PongPid ! {ping, self()},
    receive
        pong -> io:format("Received pong!~n")
    end.

pong() ->
    receive
        {ping, From} ->
            io:format("Pong received ping.~n"),
            From ! pong
    end.