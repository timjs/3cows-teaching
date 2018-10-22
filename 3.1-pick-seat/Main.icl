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


freeSeatStore :: Shared [Seat]
freeSeatStore =
  sharedStore "Free seats"
    [ Seat row chair \\ row <- [1..4], chair <- ['A'..'C'] ]



// Tasks ///////////////////////////////////////////////////////////////////////


chooseSeat :: Task Seat
chooseSeat =
  enterChoiceWithShared "Pick a seat" [] freeSeatStore >>?
    [ ( "Pick"
      , const True
      , \seat ->
          freeSeatStore $= remove seat >>- \_ ->
          viewInformation "You picked" [] seat
      )
    ]


main :: Task [Seat]
main =
  viewSharedInformation "Free seats" [] freeSeatStore
    ||-
  allTasks [chooseSeat, chooseSeat, chooseSeat]




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
