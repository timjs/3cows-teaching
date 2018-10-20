module Main


import iTasks



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task Int
main =
  enterInformation "Enter the first number" [] >>= \num1 ->
  enterInformation "Enter the second number" [] >>= \num2 ->
  viewInformation "The sum of those numbers is" [] (num1 + num2)



// Boilerplate /////////////////////////////////////////////////////////////////


Start :: *World -> *World
Start world = startEngine main world
