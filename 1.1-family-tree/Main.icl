module Main


import iTasks


/*

# Exercises

## Exercise 1

Add a `dateOfBirth` field to the `Person` type.
Compile, what changed?


## Exercise 2

Complement this example with the famility tree as we did during the lecture.

*/



// Types ///////////////////////////////////////////////////////////////////////


:: Person =
  { firstName :: String
  , lastName :: String
  , gender :: Gender
  }


:: Gender
  = Male
  | Female
  | Other


// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task Person
main =
  enterInformation "Enter your family details" []



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Person, Gender//, Family


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world
