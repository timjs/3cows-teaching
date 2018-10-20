module Main


import iTasks


/*

# Exercises

## Exercise 3

Complement the template below

*/



// Tasks ///////////////////////////////////////////////////////////////////////


step1 :: Int Int -> Task Int
step1 first second =
  updateInformation "Enter the first number" [] first >>?
    [ ( "Continue", const True, \first_new -> step2 first_new second )
    ]


step2 :: Int Int -> Task Int
step2 first second =
  updateInformation "Enter the second number" [] second >>?
    [ ( "Go back", /* condition 1 */, \second_new -> /* task 1 */)
    , ( "Continue", /* condition 2 */, \second_new -> /* task 2 */)
    ]


step3 :: Int Int -> Task Int
step3 first second =
  viewInformation "The sum of those numbers is" [] second >>?
    [ /* action 1 */
    , /* action 2 */
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
