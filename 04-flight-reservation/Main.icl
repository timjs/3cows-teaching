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
valid p = p.age >= 0


adult :: Passenger -> Bool
adult p = p.age >= 18



// Tasks ///////////////////////////////////////////////////////////////////////


enterPassengers :: Task [Passenger]
enterPassengers =
  enterInformation "Passenger details" [] >>?
    [ ( "Continue"
      , \passengers -> all valid passengers && any adult passengers && not (isEmpty passengers)
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
      , \seats -> length seats == n
      , \seats -> freeSeatStore $= removeElems seats >>- \_ -> return seats
      )
    ]


makeBooking :: -> Task Booking
makeBooking =
  (enterFlight -&&- enterPassengers) >>- \( flight, passengers ) ->
  chooseSeats (length passengers) >>- \seats ->
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
