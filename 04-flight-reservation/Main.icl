module Main


import iTasks



// Helpers /////////////////////////////////////////////////////////////////////


remove :: a (List a) -> List a | iTask a
remove x [y:ys]
  | x === y   = ys
  | otherwise = [y : remove x ys]
remove x []   = []


removeElems :: (List a) (List a) -> List a | iTask a
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
  { passengers :: List Passenger
  , flight :: Flight
  , seats :: List Seat
  }



// Stores //////////////////////////////////////////////////////////////////////


freeSeatStore :: Shared (List Seat)
freeSeatStore =
  share "Free seats" [ Seat r p \\ r <- [1..4], p <- ['A'..'D'] ]



// Checks //////////////////////////////////////////////////////////////////////


valid :: Passenger -> Bool
valid p = p.age >= 0


adult :: Passenger -> Bool
adult p = p.age >= 18



// Tasks ///////////////////////////////////////////////////////////////////////


enterPassengers :: () -> Task (List Passenger)
enterPassengers () =
  enter "Passenger details" >?>
    [ ( "Continue", all valid &&& any adult &&& not o empty, return ) ]


enterFlight :: () -> Task Flight
enterFlight () =
  enter "Flight details" >?>
    [ ( "Continue", ok, return ) ]


chooseSeats :: Int -> Task (List Seat)
chooseSeats n =
  select "Pick a seat" [] freeSeatStore >?>
    [ ( "Continue"
      , \(seats) -> length seats == n
      , \(seats) -> freeSeatStore $= removeElems seats >> return seats
      )
    ]


makeBooking :: () -> Task Booking
makeBooking () =
  ( enterFlight ()
      <&>
    enterPassengers ()
  ) >>= \( (flight), (passengers) ) ->
  chooseSeats (length passengers) >>= \(seats) ->
  //XXX We need record fields here!!!
  view "Booking" { passengers = passengers, flight = flight, seats = seats }


main :: Task Booking
main =
  //XXX What about using and forgetting?
  watch "Free seats" freeSeatStore &> makeBooking ()




// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Seat, Flight, Booking, Passenger, Nationality


Start :: *World -> *World
Start world = startEngine main world


:: List a :== [a]


ok :== const True


// External step (>>*)
(>>?) infixl 1 //:: Task a -> List ( String, a -> Bool, a -> Task b ) -> Task b
(>>?) task options :== task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)
