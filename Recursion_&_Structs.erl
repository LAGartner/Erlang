% Ejercicio 13 - Lenguajes de Programación

% Luis Gartner - A00227224
% Luis Chapa - A01282564
% Jose Carlos - A00822554

-module(a00227224_a00822554_a01282564_ejercicio14).
-export([mayor/3,suma/1,negativos/1,filtra/2,impares/1]).
-m([lists,math,io,maps]).

%funcion mayor, dados 3 argumentos numericos, regresa el mayor
mayor(X,Y,Z) -> 
    if 
      X >= Y, X >= Z -> X;
      Y >= X, Y >= Z -> Y;
       true -> Z
    end.       

%funcion suma, dada la sumatoria de 0 a N , utiliza la funcion (2K-1).
suma(0) -> -1;
suma(X) -> (X*2)-1 + suma(X-1).

%Negativos, dada una lista de numeros enteros, regresa los numeros negativos de la lista.
negativos([]) -> [];
negativos([H|T]) ->
      if H < 0 -> [H] ++ negativos(T);
         true -> negativos(T)
        end.


%Funcion filtra, usando la notacion de funciones, aplica la funcion "filter" manualmente a una lista de datos
filtra(_,[]) -> [];
filtra(F,[H|T]) ->
  case F(H) of
   true ->  [H] ++ filtra(F,T);
   false -> filtra(F,T)
  end.

%Funcion impares, usando funciones de orde superor elimina todos los elementos que no sean impares (pares)
impares([]) -> [];
impares(L) -> lists:map( fun(X)-> lists:filter(fun (Y) -> Y rem 2 > 0 end, X) end, L).



            


