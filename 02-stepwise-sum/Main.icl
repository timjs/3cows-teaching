module Main


import iTasks



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task Int
main =
  enterInformation "Enter your own age" [] >>= \first ->
  enterInformation "Enter your spouse's age" [] >>= \second ->
  viewInformation "The total age is" [] (first + second)



// Boilerplate /////////////////////////////////////////////////////////////////


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world
