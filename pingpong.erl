-module(pingpong).
-export([inicio/0,ping/2,pong/0,duerme/1]).

ping(0, Pong_PID) ->
    Pong_PID ! terminado,
    io:format("Ping termino~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},

    receive 
        pong -> io:format("Ping recibe pong~n", []),
        ping(N-1, Pong_PID)   
    after 
        100 -> io:format("Pong tardo mucho~n", [])
             
    end.
   

pong()->
    receive
        terminado -> io:format("Pong termino~n", []);
        {ping, Ping_PID} ->
            io:format("Pong recibio ping~n", []),
            duerme(50),
            Ping_PID ! pong,
            pong()
        end.

inicio() -> 
          Pong_PID = spawn(pingpong, pong, []),
           spawn(pingpong, ping, [3,Pong_PID]).

duerme(T) ->
      receive
          after T ->
              true
      end.