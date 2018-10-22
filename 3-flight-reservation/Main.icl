module Main


import iTasks
from StdEnv import undef



// Types ///////////////////////////////////////////////////////////////////////

/*
Add a `Passenger` data type containing a `firstName`, `lastName` and `age` field
op appropriate types.
*/
:: Passenger = Passenger


:: Seat  = Seat Row Chair
:: Row   :== Int
:: Chair :== Char


/*
Add a `Flight` data type, enumerating at least three possible destinations for
possible flights.
*/
:: Flight = Flight


/*
Add a `Booking` data type containing a list of passengers,
a flight and a list of selected seats.
*/
:: Booking = Booking



// Helpers /////////////////////////////////////////////////////////////////////


remove :: a [a] -> [a] | iTask a
remove x [y:ys]
  | x === y   = ys
  | otherwise = [y : remove x ys]
remove x []   = []


removeElems :: [a] [a] -> [a] | iTask a
removeElems xs ys =
  /*
  Define this function,
  which removes every element of the list `xs`
  from the list `ys`.
  */
  undef



// Stores //////////////////////////////////////////////////////////////////////


initSeats :: [Seat]
initSeats =
  [ Seat row chair \\ row <- [1..4], chair <- ['A'..'C'] ]



// Checks //////////////////////////////////////////////////////////////////////


valid :: Passenger -> Bool
valid p =
  /* when is a person valid? */
  undef


adult :: Passenger -> Bool
adult p =
  /* when is a person an adult? */
  undef



// Tasks ///////////////////////////////////////////////////////////////////////


/*
Define this task which gets a list of passenger details
from the user. Do not forget to check for the following conditions:
- the list can not be empty
- the list should contain at least one adult
- all passengers should be valid
*/
enterPassengers :: Task [Passenger]
enterPassengers =
  undef

/*
Define this task which asks the user to chooise a flight from a list.
*/
enterFlight :: Task Flight
enterFlight =
  undef


/*
Rework this task in such a way that:
- it takes the number of seats to be selected as an argument
- it is only possible to continue when the right aboumt of seats are selected
- the shared list of seats is updated rightly
*/
chooseSeat :: (Shared [Seat]) -> Task [Seat]
chooseSeat freeSeats =
  enterMultipleChoiceWithShared "Pick a seat" [] freeSeats >>?
    [ ( "Pick"
      , undef
      , \seats ->
          freeSeats $= undef >>- \_ ->
          viewInformation "You picked" [] seats
      )
    ]

/*
Define this task which makes a booking. It should
- first ask to enter a flight choice and the passenger details in parallel
  (use the -&&- combinator for this purpose);
- then let the user pick the right amount of seats;
- and finally give an overview of the created booking
*/
makeBooking :: (Shared [Seat]) -> Task Booking
makeBooking freeSeats =
  undef


main :: Task [Booking]
main =
  withShared initSeats (\freeSeats ->
    viewSharedInformation "Free seats" [] freeSeats
      ||-
    allTasks [makeBooking freeSeats, makeBooking freeSeats, makeBooking freeSeats]
  )




// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Seat, Passenger, Flight, Booking


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


($=) infixr 2 :: (ReadWriteShared r w) (r -> w) -> Task w | iTask r & iTask w
($=) share fun = upd fun share
