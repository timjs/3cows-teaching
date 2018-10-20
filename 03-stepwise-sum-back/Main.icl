module Main


import iTasks



// Tasks ///////////////////////////////////////////////////////////////////////


step1 :: Int Int -> Task Int
step1 first second =
  updateInformation "Enter the first number" [] first >>?
    [ ( "Continue", ok, \first_new -> step2 first_new second )
    ]


step2 :: Int Int -> Task Int
step2 first second =
  updateInformation "Enter the second number" [] second >>?
    [ ( "Go back", ok, \second_new -> step1 first second_new )
    , ( "Continue", ok, \second_new -> step3 first second_new )
    ]


step3 :: Int Int -> Task Int
step3 first second =
  viewInformation "The sum of those numbers is" [] second >>?
    [ ( "Go back", ok, \_ -> step1 first second )
    , ( "Finish", ok, \_ -> return (first + second) )
    ]


main :: Task Int
main =
  step1 0 0



// Boilerplate /////////////////////////////////////////////////////////////////


Start :: *World -> *World
Start world = startEngine main world


ok :== const True


// External step (>>*)
(>>?) infixl 1 //:: Task a -> List ( String, a -> Bool, a -> Task b ) -> Task b
(>>?) task options :== task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)
