module Main


import iTasks


// Types ///////////////////////////////////////////////////////////////////////


/*
Add a `dateOfBirth` field to the `Person` type.
Compile, what changed?
*/
:: Person =
  { firstName :: String
  , lastName :: String
  , gender :: Gender
  }


:: Gender
  = Male
  | Female
  | Other


/*
Complement this example with the same famility tree data type
as we did during the lecture.
*/



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task Person
main =
  enterInformation "Enter your family details" []



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Person, Gender//, Family


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world
