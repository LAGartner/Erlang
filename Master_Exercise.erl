% Ejercicio 16

% Luis Gartner - A00227224
% Luis Chapa - A01282564 
% Jose Carlos - A00822554
%-----------------------------------------------------------------------------------------------------------

-module(dist).
-export([inicio/0, maestro/0, crea_esclavo/1, para_esclavo/2, termina/0, esclavo/1]).
-m([lists,math,io,maps]).

inicio() ->
    register(maestro,spawn(dist, maestro, [])),
    io:format("maestro creado ~n").

maestro() -> 
    N = 1,
    L = maps:new(),
    maestro(N,L).

maestro(N,L) ->

    receive 
        
        {crea, Nodo, Nombre} ->
        
        case spawn_link(Nodo, dist, esclavo, [N]) of
        
        error ->  io:format("el nodo ~p no existe ~n",[Nodo]),
                  maestro(N, L);

        throw ->  io:format("el nodo ~p no existe ~n",[Nodo]),
                  maestro(N, L);

        exit ->  io:format("el nodo ~p no existe ~n",[Nodo]),
                  maestro(N, L);

         A ->   io:format("esclavo ~p creado en nodo ~p ~n",[N,Nombre]), 
                 maestro(N+1, maps:put(N,A,L))
        
        end;

        {manda, Mensaje, NUM} ->

        case maps:is_key(NUM,L) of

           true ->  

            if 
                Mensaje == muerto ->
                        PID = maps:get(NUM,L), 
                        PID ! {recibe, Mensaje},
                       maestro(N,  maps:remove(NUM,L) );

                Mensaje =/= muerto ->
                        PID = maps:get(NUM,L), 
                        PID ! {recibe, Mensaje},
                        maestro(N, L)
            end;

          false ->  io:format("El esclavo ~w no existe ~n",[NUM]),

          maestro(N, L)

        end;

        {termina} ->
            Fun = fun(_,PID) -> PID ! {recibe, muerto} end, 
            maps:map(Fun,L),
            io:format("El maestro termino ~n")
            
    end.

esclavo(NP) ->
    receive         
        {recibe, Mensaje} ->
        case Mensaje of
             muerto ->  io:fwrite("El esclavo ~p ha muerto~n",[NP]); 
             Mensaje ->   io:fwrite("El esclavo ~p recibio el mensaje ~w~n", [NP, Mensaje]),
                    esclavo(NP)
            end
        end.

crea_esclavo(Nodo) ->
    % Nodo + el HOST, ESTE CAMBIA POR COMPUTADORA
    N = list_to_atom( atom_to_list(Nodo) ++ "@CRIMSON" ),
     maestro ! {crea, N, Nodo}.
     
para_esclavo(Mensaje, N) ->
     maestro ! {manda, Mensaje, N}.
     
termina() -> 
    maestro ! {termina}.

