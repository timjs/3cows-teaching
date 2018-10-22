module Main


import iTasks



// Types ///////////////////////////////////////////////////////////////////////


:: Seat  = Seat Row Chair
:: Row   :== Int
:: Chair :== Char



// Helpers /////////////////////////////////////////////////////////////////////


remove :: a [a] -> [a] | iTask a
remove x [y:ys]
  | x === y   = ys
  | otherwise = [y : remove x ys]
remove x []   = []



// Stores //////////////////////////////////////////////////////////////////////


initSeats :: [Seat]
initSeats =
  [ Seat row chair \\ row <- [1..4], chair <- ['A'..'C'] ]



// Tasks ///////////////////////////////////////////////////////////////////////


chooseSeat :: (Shared [Seat]) -> Task Seat
chooseSeat freeSeats =
  enterChoiceWithShared "Pick a seat" [] freeSeats >>?
    [ ( "Pick"
      , const True
      , \seat ->
          freeSeats $= remove seat >>- \_ ->
          viewInformation "You picked" [] seat
      )
    ]


main :: Task [Seat]
main =
  withShared initSeats (\freeSeats ->
    viewSharedInformation "Free seats" [] freeSeats
      ||-
    allTasks [chooseSeat freeSeats, chooseSeat freeSeats, chooseSeat freeSeats]
  )




// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Seat


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


($=) infixr 2 :: (ReadWriteShared r w) (r -> w) -> Task w | iTask r & iTask w
($=) share fun = upd fun share
