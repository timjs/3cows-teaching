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
  | Other


:: Family =
  { person :: Person
  , spouse :: Maybe Person
  , children :: [Family]
  }



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task Family
main =
  enterInformation "Enter your family details" []



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Person, Gender, Family


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world
