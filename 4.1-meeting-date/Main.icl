module Main


import iTasks



// Types ///////////////////////////////////////////////////////////////////////


:: Name :== String


:: DateOption =
  { day :: Date
  , time :: Time
  }


:: MeetingOption =
  { users :: [Name]
  , moment :: DateOption
  }


:: Table :== [MeetingOption]



// Helpers /////////////////////////////////////////////////////////////////////


elem :: a [a] -> Bool | iTask a
elem x []     = False
elem x [y:ys]
  | x === y   = True
  | otherwise = False



// Stores //////////////////////////////////////////////////////////////////////


users :: [Name]
users =
  [ "Rinus"
  , "Peter"
  , "Mart"
  , "Tim"
  ]



// Stores //////////////////////////////////////////////////////////////////////

initTable :: [DateOption] -> Table
initTable dates =
  [ { users = [], moment = moment } \\ moment <- dates ]


updateTable :: Name [DateOption] Table -> Table
updateTable name selected table =
  [ { option
    & users =
        if (elem option.moment selected)
          [name : option.users]
          option.users
    }
  \\ option <- table
  ]


getDate :: MeetingOption -> DateOption
getDate meeting = meeting.moment



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task MeetingOption
main =
  defineMeetingPurpose >>= \purpose ->
  selectDatesToPropose >>= \dates ->
  selectAttendencees >>= \others ->
  askOthers purpose dates others >>= \options ->
  selectMeetingDate purpose options >>= \chosen ->
  viewInformation "Date chosen:" [] chosen


defineMeetingPurpose :: Task String
defineMeetingPurpose =
  enterInformation "What is the purpose of the meeting?" []


selectDatesToPropose :: Task [DateOption]
selectDatesToPropose =
  enterInformation "Enter the moment you'd like to meet..." []


selectAttendencees :: Task [Name]
selectAttendencees =
  enterMultipleChoice "Who do you want to invite for the meeting?" [] users


askOthers :: String [DateOption] [Name] -> Task Table
askOthers purpose dates others =
  withShared (initTable dates) (\table ->
    allTasks [ ask purpose name table \\ name <- others ] >>= \_ ->
    get table
  )


ask :: String Name (Shared Table) -> Task [DateOption]
ask purpose name table =
  viewSharedInformation ( name, "Current Responses" ) [] table ||- (
    enterMultipleChoiceWithShared
      ("Select the moment(s) you can attend for " +++ purpose)
      [] table >>= \options ->
    let moments = map getDate options in
    table $= updateTable name moments >>- \_ ->
    viewInformation "You selected" [] moments
  )


selectMeetingDate :: String Table -> Task MeetingOption
selectMeetingDate purpose table =
  enterChoice ("Select the moment for " +++ purpose) [] table



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask DateOption, MeetingOption


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


($=) infixr 2 :: (ReadWriteShared r w) (r -> w) -> Task w | iTask r & iTask w
($=) share fun = upd fun share
