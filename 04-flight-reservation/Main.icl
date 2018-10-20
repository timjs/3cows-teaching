module Main


import iTasks



// Helpers /////////////////////////////////////////////////////////////////////


remove :: a [a] -> [a] | iTask a
remove x [y:ys]
  | x === y   = ys
  | otherwise = [y : remove x ys]
remove x []   = []


removeElems :: [a] [a] -> [a] | iTask a
removeElems []     ys = ys
removeElems [x:xs] ys = removeElems xs (remove x ys)



// Data ////////////////////////////////////////////////////////////////////////


:: Nationality
  = Dutch
  | Portugues
  | Hongarian


:: Passenger =
  { firstName :: String
  , lastName :: String
  , nationality :: Nationality
  , age :: Int
  }


:: Flight
  = ToAmsterdam
  | ToLisbon
  | ToBudapest


:: Row   :== Int
:: Chair :== Char
:: Seat  = Seat Row Chair


:: Booking =
  { passengers :: [Passenger]
  , flight :: Flight
  , seats :: [Seat]
  }



// Stores //////////////////////////////////////////////////////////////////////


freeSeatStore :: Shared [Seat]
freeSeatStore =
  sharedStore "Free seats" [ Seat r p \\ r <- [1..4], p <- ['A'..'D'] ]



// Checks //////////////////////////////////////////////////////////////////////


valid :: Passenger -> Bool
valid p = /* when is a person valid? */


adult :: Passenger -> Bool
adult p = /* when is a person an adult? */



// Tasks ///////////////////////////////////////////////////////////////////////


enterPassengers :: Task [Passenger]
enterPassengers =
  enterInformation "Passenger details" [] >>?
    [ ( "Continue"
      , \passengers -> /* check for one adult, valid ages and at least one passenger */
      , return
      )
    ]


enterFlight :: Task Flight
enterFlight =
  enterInformation "Flight details" [] >>?
    [ ( "Continue", const True, return ) ]


chooseSeats :: Int -> Task [Seat]
chooseSeats n =
  enterMultipleChoiceWithShared "Pick a seat" [] freeSeatStore >>?
    [ ( "Continue"
      , \seats -> /* check if a right number of seats is selected */
      , \seats ->
          freeSeatStore $= /* use one of the helpers above */ >>- \_ ->
          return seats
      )
    ]


makeBooking :: -> Task Booking
makeBooking =
  /* create a combination of tasks which does `enterFlight` and `enterPassengers` in parallel */ >>- \( flight, passengers ) ->
  /* and then lets the user choose the right amount of seats with `chooseSeats` */ >>- \seats ->
  viewInformation "Booking" [] { passengers = passengers, flight = flight, seats = seats }


main :: Task Booking
main =
  viewSharedInformation "Free seats" [] freeSeatStore ||- makeBooking




// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Seat, Flight, Booking, Passenger, Nationality


Start :: *World -> *World
Start world = startEngine main world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


($=) infixr 2 :: (ReadWriteShared r w) (r -> w) -> Task w | iTask r & iTask w
($=) share fun = upd fun share
