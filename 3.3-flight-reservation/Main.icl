module Main


import iTasks



// Types ///////////////////////////////////////////////////////////////////////


:: Passenger =
  { firstName :: String
  , lastName :: String
  , age :: Int
  }


:: Seat  = Seat Row Chair
:: Row   :== Int
:: Chair :== Char


:: Flight
  = ToAmsterdam
  | ToLisbon
  | ToBudapest


:: Booking =
  { passengers :: [Passenger]
  , flight :: Flight
  , seats :: [Seat]
  }



// Helpers /////////////////////////////////////////////////////////////////////


remove :: a [a] -> [a] | iTask a
remove x [y:ys]
  | x === y   = ys
  | otherwise = [y : remove x ys]
remove x []   = []


removeElems :: [a] [a] -> [a] | iTask a
removeElems []     ys = ys
removeElems [x:xs] ys = removeElems xs (remove x ys)



// Stores //////////////////////////////////////////////////////////////////////


initSeats :: [Seat]
initSeats =
  [ Seat r p \\ r <- [1..4], p <- ['A'..'C'] ]



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
    [ ( "Continue"
      , const True
      , return
      )
    ]


chooseSeats :: (Shared [Seat]) Int -> Task [Seat]
chooseSeats freeSeats n =
  enterMultipleChoiceWithShared "Pick a seat" [] freeSeats >>?
    [ ( "Pick"
      , \seats -> length seats == n
      , \seats ->
          freeSeats $= removeElems seats >>- \_ ->
          return seats
      )
    ]


makeBooking :: (Shared [Seat]) -> Task Booking
makeBooking freeSeats =
  (enterFlight -&&- enterPassengers) >>- \( flight, passengers ) ->
  chooseSeats freeSeats (length passengers) >>- \seats ->
  viewInformation "Booking" [] { passengers = passengers, flight = flight, seats = seats }


main :: Task [Booking]
main =
  withShared initSeats (\freeSeats ->
    viewSharedInformation "Free seats" [] freeSeats
      ||-
    (allTasks
      [ makeBooking freeSeats
      , makeBooking freeSeats
      , makeBooking freeSeats
      ] <<@ ArrangeHorizontal
    )
  )



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Seat, Flight, Booking, Passenger


Start :: *World -> *World
Start world = startEngine main world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


($=) infixr 2 :: (ReadWriteShared r w) (r -> w) -> Task w | iTask r & iTask w
($=) share fun = upd fun share
