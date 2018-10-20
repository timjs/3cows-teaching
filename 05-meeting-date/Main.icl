module Main


import iTasks



// Types ///////////////////////////////////////////////////////////////////////


:: User :== String


:: DateOption =
  { date :: Date
  , hour :: Int
  }


:: MeetingOption =
  { users :: [String]
  , date :: DateOption
  }



// Stores //////////////////////////////////////////////////////////////////////


users :: Shared [User]
users =
 [ "Rinus"
 , "Peter"
 , "Mart"
 , "Tim"
 ]



// Helpers /////////////////////////////////////////////////////////////////////


initTable :: [Date] -> [MeetingOption]
initTable dates =
  [ { users = [], date = date } \\ date <- dates ]


updateTable :: [Int] [MeetingOption] -> [MeetingOption]
updateTable indices options =
  [ { option & users = if (isMember j indices) [user : option.users] option.users }
  \\ j <- [0..] & option <- options
  ]



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task MeetingOption
main =
  defineMeetingPurpose >>= \purpose ->
  selectDatesToPropose >>= \dates ->
  selectAttendencees >>= \others ->
  askOthers purpose dates others >>= \options ->
  selectMeetingDate options >>= \chosen ->
  viewInformation "Date chosen:" [] chosen


defineMeetingPurpose :: Task String
defineMeetingPurpose =
  enterInformation "What is the purpose of the meeting?" []


selectDatesToPropose :: Task [DateOption]
selectDatesToPropose =
  enterInformation "Select the date(s) and time you propose to meet..." []


selectAttendencees :: Task [User]
selectAttendencees =
  enterMultipleChoiceWithShared ("Who do you want to invite for the meeting?")
    [ChooseFromCheckGroup id] users


askOthers :: String [DateOption] [User] -> Task MeetingOption
askOthers purpose dates others =
  withShared (initTable dates) $ \table ->
    allTasks [ ( user, purpose ) @: askOne (toString user) \\ user <- others ]


askOne user table =
  viewSharedInformation "Current Responses:" [] table
    ||-
  enterMultipleChoice "Select the date(s) you can attend the meeting:"
    [ChooseFromGrid (\i -> dates!!i)] [0..length dates - 1] >>= \indices ->
  upd (updateTable indices) table


selectMeetingDate :: (Shared [MeetingOption]) -> Task MeetingOption
selectMeetingDate table =
  enterChoiceWithShared "Select the date for the meeting:" [ChooseFromGrid id] table



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask DateOption, MeetingOption


Start :: *World -> *World
Start world = startEngine main world
