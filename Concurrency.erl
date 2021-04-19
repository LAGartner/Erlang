% Ejercicio 15 - Lenguajes de Programación

-module(conc).
-export([prueba_suma/0, suma/0, registra/0, prueba_registra/0, centro/2, estrella/3, proceso/2, inicio/0]).
-m([lists,math,io,maps]).

%--------------------------------------------------------------------------------------------------------------------
prueba_suma() ->
   P = spawn(conc, suma, []),
   prueba_suma(5, P).

prueba_suma(N, P) when N > 0 ->
   P ! {suma, N, self()},
   receive
       {respuesta, S} ->
           io:format("Acumulado ~w~n", [S]),
           prueba_suma(N-1, P)
   end;

prueba_suma(_, _) ->
    io:format("Terminé mi trabajo~n").

%--------------------------------------------------------------------------------------------------------------------
% Programa 1. metodo suma() , acumula la suma de los numeros de acuerdo a como vienen por el mensaje
% Primera iteracion de la suma es el numero solo, y se envia como tal
suma() -> 
    receive 
        {suma, N, P} ->
        P ! {respuesta, N},
        suma(N)
    end.
% segunda iteracion en adelante, ya tenemos alguien con quien sumarlo, esta funcion se sigue repitiendo y espera a ser llamada
suma(A) ->
    receive 
        {suma, N, P} ->
        P ! {respuesta, N+A},
        suma(N+A)
    end.
%--------------------------------------------------------------------------------------------------------------------
% Programa 2. metodo prueba_registra() , envia una secuencia de ordenes ala funcion registra.
prueba_registra() ->
   P= spawn(conc, registra, []),
   prueba_registra(5, P).

prueba_registra(N, P) ->

  case N of
  5 -> P ! {registra,'Pablo'},
       io:format("Pablo registrado~n"),
       prueba_registra(N-1,P);

  4 -> P ! {registra,'Juana'},
       io:format("Juana registrada~n"),
       prueba_registra(N-1,P);

  3 ->  P ! {lista, self()},
  receive 
      {registrados, L} ->
      io:fwrite("La lista de alumnos es ~w~n", [L]),
      prueba_registra(N-1,P)
    end;

  2 -> P ! {busca,'Pablo',self()},
  receive 
    {encontrado,si} ->
      io:format("Pablo si se encontro~n"),
       prueba_registra(N-1,P);
    {encontrado,no} ->
      io:format("Pablo no se encontro~n"),
       prueba_registra(N-1,P)
    end;

  1 -> P ! {busca,'Juana',self()},
  receive 
    {encontrado,si} ->
      io:format("Juana si se encontro~n"),
       prueba_registra(N-1,P);
    {encontrado,no} ->
      io:format("Juana no se encontro~n"),
       prueba_registra(N-1,P)
    
    end;
   
  0 ->  io:format("Termina el programa~n")
  
    end.


%--------------------------------------------------------------------------------------------
% Programa 2. metodo registra() , manejador de nombres de alumnos en una lista y buscador de existencia
% Primera iteracion, se crea la lista vacia para almacenar los nombres, y empieza a recibir mensajes
registra() ->
   L = [],
   receive
        {registra, Nombre} ->
            registra( L ++ [Nombre] );
        % Como en la primera iteracion no hay nombres, se devuelve un no encontrado y se manda la lista
        % a la funcion que si toma una lista como argumento
        {busca,_,P} -> 
            E = 'no',
            P ! {encontrado,E},
            registra(L);

        {lista, P} ->
            P ! {registrados, L},
             registra(L)
    end.

% Segunda iteracion en adelante, aqui ya se mantiene la lista de nombres, simplemente espera a los mensajes y responde si es necesario
registra(L) ->
     receive
         %como solo registra un nombre ala lista, no es necesario devolver mensajes, solo almacenar el bombre en la lista
        {registra, Nombre} ->
            case lists:member(Nombre,L) of
            false -> registra( L ++ [Nombre]);
            true -> registra(L)
            end;

        {busca,Nombre,P} ->
            case lists:member(Nombre,L) of
            false -> E = 'no'; 
            true -> E = 'si'
            end,
            P ! {encontrado, E},
            registra(L);

        {lista, P} ->
            P ! {registrados, L},
        registra(L)
             
        end.

%--------------------------------------------------------------------------------------------
% Programa 3. programa estrella(). crea N procesos que durante M vueltas enviaran un Mensaje a un centro que verificara
% sus datos, y los procesos contestaran con su identificador numerico

% Funcion centro: se encarga de ser el "Buzon" donde todos los procesos envian su informacion de cada vuelta
centro(M, Mensaje) ->
    receive
        % Recibe los datos de un proceso X:Numero de proceso, N:Numero de vuelta, P:PID del proceso
        {X,N,P} ->
            if 
                % Si ya se acabo sus vueltas se le envia un mensaje de finalizacion al proceso
                N > M -> P ! {terminado},
                centro(M,Mensaje);
                % Si todavia le quedan vueltas al proceso, le envia un mensaje "Continuar" con su PID e
                % imprime sus datos, esperando un mensaje de respuesta del procso
                N =< M -> P ! {continuar, self()},
                io:format("p: ~p - n: ~p - m: ~w ~n",[X,N,Mensaje]),
                centro(M, Mensaje) 
            end
        % se espera un milisegundo a ver si no hay mas mensajes en el buzon, y cierra dicho buzon despues,
        % terminando efectivamente el programa
        after 1 -> io:format("Termina el programa~n")
        end.

%Funcion estrella/3 , esta funcion es llamada con los parametros siguientes
% N:Numero de procesos a crear, M:Numero de vueltas disponibles para cada proceso, Mensaje:texto a enviarse entre si
estrella(N,M,Mensaje) ->
    % Se crea el "Puente" con el centro, ya que solo hay uno, no es necesario utilizar otra funcion Spawn
    % Se guarda su direccion en PIDC
    PIDC = spawn(conc, centro, [M,Mensaje]),
    % como sabemos que debe haber al menos un proceso intercambiando informacion con el centro
    % creamos ese punto de inicio.
    Inicio = 1,
    % Se llama a la funcion del mismo nombre con mas parametros, el inicio obligatorio "1", el numero de procesos real (N >= 1)
    % el mensaje y el "Puente" con el centro
    estrella(Inicio,N,Mensaje,PIDC).

%Funcion estrella/4, aqui es donde crearemos recursivamente nuestros procesos, asignandoles un identificador numerico
estrella(N,Max,Mensaje,P) ->
  
  % Si aun no llegamos al limite establecido "de 1 proceso a N procesos" continuamos
  case N =< Max of 
         
         % Creamos un PID para el proceso de numero "N" con su primera iteracion lista
         true -> PID = spawn(conc, proceso, [N,1]),
                % Despertamos a nuestra funcion centro, enviandole el proceso, su primera iteracion y su PID
                 P ! {N,1,PID},
                % Llamamos recursivamente a nuestra funcion con un nuevo identificador para crear mas procesos
                 estrella(N+1,Max,Mensaje,P);
        % Cuando llegamos al limite del numero de procesos a crear, se envia un mensaje para empezar la solucion.
         false -> io:format("Procesos creados, Inicia programa Estrella ~n")
end.
   
% Funcion proceso, esta misma se comunica con el centro, contestando con su numero y enviando su informacion
% recibe su numero de proceso "P" y la vuelta en la que va "N"
proceso(P,N) ->
    % Espera a que el centro le envie un mensaje
    receive 
        % Si centro quiere que continue, el proceso responde con su ID, aqui termina su vuelta
        {continuar,I} -> io:format("Contesto el proceso ~p~n",[P]),
             S = N+1, 
            % nuevamente llama al centro con una vuelta adicional
             I ! {P,S,self()},
             % Se llama a si mismo para volver a esperar su turno 
             proceso(P,S);
         % Si centro reconoce que ya no le queda vueltas, envia un terminado, lo que detiene al proceso con gracia.   
        {terminado} ->
             io:format("Proceso terminado : ~p~n", [P] )    
    end.

inicio() -> 
    io:format(">> Programa 1 ~n"),
    prueba_suma(),
     io:format(">> Programa 2 ~n"),
    prueba_registra(),
     io:format(">> Programa 3 ~n"),
    estrella(5,3,'Hola Mundo').

