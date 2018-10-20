module Main


import iTasks



// Types ///////////////////////////////////////////////////////////////////////


:: Person =
  { firstName :: String
  , lastName :: String
  , gender :: Gender
  , dateOfBirth :: Date
  }


:: Gender
  = Male
  | Female


:: Family =
  { person :: Person
  , spouse :: Maybe Person
  , children :: [Family]
  }



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task Family
main =
  enterInformation "Enter your family details" [] >>= \family ->
  viewInformation "You entered this information" [] family



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Person, Gender, Family


Start :: *World -> *World
Start world = startEngine main world
