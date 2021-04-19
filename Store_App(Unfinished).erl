
% Tarea6 - Lenguajes de Programación

% Programa de tienda, explicacion y usos al final del documento %

-module(tarea6).
-export([abre_tienda/0, tienda/0, subscribir_socio/1, elimina_socio/1, crea_pedido/2, acepta_pedido/2, rechaza_pedido/2, lista_existencias/0 
        ,socio/1,registra_producto/2, elimina_producto/1,modifica_producto/2, cierra_tienda/0, lista_socios/0, pedidos_en_proceso/0, pedidos_atendidos/0]).
-m([lists,math,io,maps]).

abre_tienda() ->

    register(tienda,spawn(tarea6, tienda, [])),
    
    io:format("¡Bienvenido, La tienda esta abierta! ~n").

tienda() ->
  % Lista socios ( Lista de nombres de los socios registrados )
    LS = [],

    % Lista productos ( lista de tuplas con el formato {Producto, CantidadPedida} ).
    LP = [],
            
    % Lista pedidos en proceso ( ID del pedido - nombre del socio )  
    LPP = maps:new(),
            
    % Lista pedidos atendidos ( Nombre del socio - Numero de pedidos aceptados )
    LPA = maps:new(),
    
    % Numero de pedidos (Iniciamos en cero y va creciendo linealmente )
    NP = 1,
    
    % Lista de nodos de socios conectados ({nombre,PID}).
    LN = [],
    
    tienda(LS,LP,LPP,LPA,NP, LN).

tienda(LS,LP,LPP,LPA,NP,LN) ->

    receive
        % Cuando deseamos inscribir a un nuevo socio
        {subscribir_socio, Socio} ->
          io:format("Solicitud de subscripcion de ~w ~n", [Socio]),
            Nodo = nodo(Socio),
            monitor_node(Nodo, true),
            Pid = spawn(Nodo, tarea6, socio, [Socio]),
		  receive
		      {nodedown, Nodo} -> 
			    io:format("nodo ~w no existe~n", [Socio]),
			    tienda(LS,LP,LPP,LPA,NP, LN)
			 after 0 -> 
			    io:format("Socio ~w conectado en nodo ~w~n",
			     [Socio, Socio]),
			       monitor_node(Nodo, false)
             end,
              
            %se verifica si el nombre esta en nuestra lista
            case lists:member(Socio,LS) of
              % Si no esta, se puede añadir
              false ->  io:format("El socio ~w se acaba de suscribir ~n", [Socio]),
                         tienda(LS ++ [Socio],LP,LPP,maps:put(Socio,0,LPA),NP,LN ++ [{Socio, Pid}]);
              % de lo contrario no
              true ->  io:format("El socio ~w ya existe, intente de nuevo ~n", [Socio]),
                       tienda(LS,LP,LPP,LPA,NP, LN)
            end;
        
        % Cuando deseamos eliminar a un socio
        {elimina_socio, Socio} ->
        %se verifica si el nombre esta en nuestra lista
            case lists:member(Socio,LS) of
                % Si no esta, termina
              false ->  io:format("El socio ~w no existe, intente de nuevo ~n", [Socio]),
                         tienda(LS,LP,LPP,LPA,NP, LN);

                 % Elimina al socio de la lista de nombres (LS)
              true ->  io:format("El socio ~w ha sido eliminado ~n", [Socio]),
                       case maps:is_key(Socio,LPP) of
                           true -> A = maps:filter( fun(Key,S) -> S /= Socio end , LPP),
                                  io:format("Se eliminaron los pedidos en proceso de socio ~n"),
                                  tienda(lists:delete(Socio, LS),LP,A,LPA,NP,LN);
                           false -> io:format("El socio no tiene pedidos en proceso, eliminacion terminada ~n"),
                           tienda(lists:delete(Socio, LS),LP,LPP,LPA,NP,LN)
                       end
                       
            end;

        {crea_pedido, Socio, ListaDeProductos} -> 
     
         case lists:member(Socio,LS) of
                % Si no esta, termina
              false ->  io:format("El socio ~w no existe, intente de nuevo ~n", [Socio]),
                         tienda(LS,LP,LPP,LPA,NP, LN);

                 % Elimina al socio de la lista de nombres (LS)
              true ->  
                
             ListaPed = lists:flatten(lists:map( fun({ProdL,CL}) -> lists:filter(fun ({ProdT,CT}) ->  (ProdL == ProdT) and (CT-CL >= 0)  end ,LP) end,ListaDeProductos)),
            
            % Verificar si estan disponibles los productos en existencia

            % responder con la lista de productos ajustada

                io:format("Lista del pedido ~p ~n", [NP]),
                lists:map(fun ({Nombre,Existencia}) -> 
                       io:format("~w --- ~p ~n",[Nombre,Existencia]) end, ListaPed),
               
                tienda(LS,LP,maps:put(NP,Socio,LPP),LPA,NP+1,LN)
                   
            % se manda a pedidos en proceso y ahi se quedara hasta que se acepte
                       
            end;

              
        {acepta_pedido, Socio, Pedido} ->

                   io:format("El socio ~w a aceptado el pedido ~p ~n",[Socio,Pedido]),

                   %Valor actual de la cantidad de pedidos aceptados por ese socio
                   D = maps:get(Socio,LPA),

                   % se elimina el pedido en proceso de esa lista
                   % Se suma uno mas al value de pedidos en proceso
                   % donde Key es el nombre del socio en cuestion y el value es el numero de pedidos aceptado
                   % La tienda se vuelve a llamar 

                   tienda(LS,LP,maps:remove(Pedido,LPP), maps:update(Socio,D+1,LPA),NP,LN);

        {rechaza_pedido, Socio, Pedido} ->    
                  io:format("El socio ~w a rechazado el pedido ~p ~n",[Socio,Pedido]),
                   % el pedido se elimina de la lista en procesos pendientes
                   % La tienda se vuelve a llamar
                   tienda(LS,LP, maps:remove(Pedido,LPP), LPA,NP,LN);

        {registra_producto, Producto,Cantidad} -> 
              io:format("Se ha solicitado el registrar un producto : ~w -- con cantidad : ~p ~n",[Producto,Cantidad]),
              % Si existe el producto en la lista, se ignora
              Map = maps:from_list(LP),
              case maps:is_key(Producto, Map) of
                  % Si no existe el producto a registrar en la lista, se añade
                    true -> tienda(LS,LP, LPP, LPA,NP,LN);
                    false -> tienda(LS,LP ++ [{Producto,Cantidad}],LPP,LPA,NP,LN)
               end;

        {elimina_producto, Producto } ->
           Map = maps:from_list(LP),
             case maps:is_key(Producto,Map) of
                % Si no esta, termina
          false ->  io:format("El producto ~w no existe, intente de nuevo ~n",[Producto]),
                   tienda(LS,LP,LPP,LPA,NP, LN);
                 % Elimina el producto de la lista (LP)
              true ->  io:format("El Producto ~w ha sido eliminado ~n", [Producto]),
                 tienda(LS, maps:to_list(maps:remove(Producto, LP)) ,LPP,LPA,NP, LN)      
            end;

        {modifica_producto, Producto, Cantidad} ->

               % Convertimos la lista a un mapa para su mejor manejo
               Map = maps:from_list(LP),

            % verificamos si el producto existe en la lista/mapa
          case maps:is_key(Producto,Map) of
                % Si no esta, termina
          false ->  io:format("El producto ~w no existe, intente de nuevo ~n",[Producto]),
                   tienda(LS,LP,LPP,LPA,NP, LN);
                % Si lo esta , modifica el producto de la lista/mapa
          true ->   
              A = maps:get(Producto,Map),
              if  A < Cantidad ->  io:format("La cantidad excede las existencias, intente de nuevo ~n") ,
                                      tienda(LS,LP,LPP,LPA,NP, LN);

                                    % Se modifica si la cantidad no excede existencias, y se vuelve a convertir a lista para su manejo simple
                  A >= Cantidad -> io:format("El Producto ~w ha sido modificado con cantidad ~p ~n", [Producto,A+Cantidad]),
                                    tienda(LS, maps:to_list(maps:update(Producto, A + Cantidad )) ,LPP,LPA,NP,LN)
                      end
               end;

        {lista_existencias} ->
            io:format("Nombre Producto --- Existencia ~n"),
            lists:map(fun ({Nombre,Existencia}) -> 
                       io:format("~w --- ~p",[Nombre,Existencia]) end, LP),
                      tienda(LS,LP,LPP,LPA,NP, LN);
        

        {lista_socios} ->

            io:format("Lista de socios ----- ~n"),
            lists:map(fun (X) -> 
                       io:format("~w ",[X]) end, LS),
                tienda(LS,LP,LPP,LPA,NP, LN);
        
        {pedidos_en_proceso} ->
            
            ListaP = maps:to_list(LPP),

            io:format("Pedidos en proceso ----- ~n"),
            io:format("ID del Pedido -- Socio ~n"),

            lists:map(fun ({Pid,Socio}) -> 
                       io:format("~p       ~w ",[Pid,Socio]) end, ListaP),
                     tienda(LS,LP,LPP,LPA,NP, LN);
           
           % Imprimir los pedidos enteros ( Nombre socio , num pedido , pedido en si )
        
        {pedidos_atendidos} ->
          
            ListaP = maps:to_list(LPA),

            io:format("Pedidos Atendidos ----- ~n"),
            io:format("Socio -- Pedidos Atendidos ~n"),

            lists:map(fun ({Socio,Cant}) -> 
                       io:format("~w       ~p ",[Socio,Cant]) end, ListaP),

                      tienda(LS,LP,LPP,LPA,NP, LN);
           

         termina -> 
	       
            lists:map(fun({_, Epid}) -> Epid ! fuera end, LN)
    
    end.

% Manejo de los socios
socio(Nombre) ->
   receive
      fuera ->
	     io:format("El socio ~w se fue ~n", [Nombre]),
	     socio(Nombre)
   end.

subscribir_socio(Socio) ->

    {tienda, nodo(tienda)} ! {subscribir_socio, Socio}.

elimina_socio(Socio) ->

    {tienda, nodo(tienda)} ! {elimina_socio, Socio}.

lista_socios() ->

    {tienda, nodo(tienda)} ! {lista_socios}.

% Manejo de los pedidos

crea_pedido(Socio, ListaDeProductos) ->
     
    {tienda, nodo(tienda)} ! {crea_pedido, Socio, ListaDeProductos}.

acepta_pedido(Socio,Pedido) ->

    {tienda, nodo(tienda)} ! {acepta_pedido, Socio, Pedido}.

rechaza_pedido(Socio,Pedido) ->
     
    {tienda, nodo(tienda)} ! {rechaza_pedido, Socio, Pedido}.

lista_existencias() ->

    {tienda, nodo(tienda)}  ! {lista_existencias}.

pedidos_en_proceso() ->
     
    {tienda, nodo(tienda)} ! {pedidos_en_proceso}.

pedidos_atendidos() ->

    {tienda, nodo(tienda)} ! {pedidos_atendidos}.

% Manejo de los productos

registra_producto(Producto,Cantidad) ->

    {tienda, nodo(tienda)} ! {registra_producto, Producto, Cantidad}.

elimina_producto(Producto) ->

    {tienda, nodo(tienda)} ! {elimina_producto, Producto}.

modifica_producto(Producto, Cantidad) ->

    {tienda, nodo(tienda)} ! {modifica_producto, Producto, Cantidad}.

% Terminar todos los procesos con gracia con cerrar_tienda

% Nombre del servidor, despues del @ se pone el nombre de TU maquina
nodo(Nombre) -> list_to_atom(atom_to_list(Nombre)++"@CRIMSON").

cierra_tienda() ->
 {tienda, nodo(tienda)} ! termina,
   'La tienda ha cerrado'.

% Proceso de trabajo - Ejemplificado %

% Paso 1 : Crea en una terminal el nodo "tienda" ( erl -sname tienda ) %

% Paso 2 : en otras terminales crea los nodos con los NOMBRES DE LOS SOCIOS QUE USARAS EN EL SISTEMA ( erl -sname "nombresocio" ) %

% Paso 3 : inicia el sistema con el comando abre_tienda(). en el nodo tienda %

% Paso 4 : usa el comando modulo:subscribir_socio("nombresocio") para generar una conexion al nodo y poder trabajar remotamente %

% Paso 5 : desde el nodo del socio puedes utilizar los comandos especiales de el (como elimina_socio(), lista_socios()) %

% Paso 6 : Puedes registrar tus productos con el comando registra_producto(producto,cantidad), intentalo con "gatos" y "3" como valores %

% Paso 7 : intenta eliminar los productos o modificarlos con los comandos elimina_producto y modifica_producto %

% Paso 8 : cuando generes tus pedidos con el comando crea_pedido(Nombresocio, [lista de tuplas con tu producto, cantidad] estos se registran al nombre de el con
% mapas %

% Paso 9 : puedes aceptar o rechazar pedidos creados utilizando acepta_pedido(socio,pedido) o rechaza_pedido(socio,pedido) y estos se manipulan o eliminad dependiendo de lo que hagas%

% Paso 10 : puedes listar socios, asi como los pedidos con los comandos hablados %

% Paso Final : Cuando termines de usar el servicio de tienda , utiliza el comando cierra_tienda(), esta mandara mensajes a los socios para que se retiren y acabara con los
% procesos conectados a ella%.

%Gracias por su atencion y pasa bien!%