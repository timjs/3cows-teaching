module Main


import iTasks



// Tasks ///////////////////////////////////////////////////////////////////////

step1 =
  enterInformation "Enter the first number" [] >>?
  //( button name, guard       , continuation )
  [ ( "Continue", \first -> ..., \first -> ...)
  ]


step2 first =
  enterInformation "Enter the second number" [] >>?
  //( button name, guard       , continuation )
  [ ( "Continue", \second -> ..., \second -> ...)
  ]


step3 =
  let
    sum = first + second
  in
  viewInformation "The sum of those numbers is" [] sum


main :: Task Int
main =



// Boilerplate /////////////////////////////////////////////////////////////////


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world
