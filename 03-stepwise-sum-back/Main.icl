module Main


import iTasks



// Tasks ///////////////////////////////////////////////////////////////////////


step1 :: Int Int -> Task Int
step1 first second =
  updateInformation "Enter the first number" [] first >>?
    [ ( "Continue", const True, \first_new -> step2 first_new second )
    ]


step2 :: Int Int -> Task Int
step2 first second =
  updateInformation "Enter the second number" [] second >>?
    [ ( "Go back", const True, \second_new -> step1 first second_new )
    , ( "Continue", const True, \second_new -> step3 first second_new )
    ]


step3 :: Int Int -> Task Int
step3 first second =
  viewInformation "The sum of those numbers is" [] second >>?
    [ ( "Go back", const True, const (step1 first second) )
    , ( "Finish", const True, const (return (first + second)) )
    ]


main :: Task Int
main =
  step1 0 0



// Boilerplate /////////////////////////////////////////////////////////////////


Start :: *World -> *World
Start world = startEngine main world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)
