module Main


import iTasks



// Tasks ///////////////////////////////////////////////////////////////////////


step1 :: Task Int
step1 =
  enterInformation "Enter your own age" [] >>?
    [ ( "Continue", \first -> first >= 0, \first -> step2 first )
    ]


step2 :: Int -> Task Int
step2 first =
  enterInformation "Enter your spouse's age" [] >>?
    [ ( "Continue", \second -> first <= second, \second -> step3 first second )
    ]


step3 :: Int Int -> Task Int
step3 first second =
  let
    sum = first + second
  in
  viewInformation "The total age is" [] sum >>?
    [ ( "Finish", const True, const (ready sum) )
    ]


ready :: Int -> Task Int
ready sum = viewInformation "Ready! We calculated" [] sum


main :: Task Int
main =
  step1



// Boilerplate /////////////////////////////////////////////////////////////////


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)
