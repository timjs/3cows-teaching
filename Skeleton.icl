module Main


import iTasks



// Basic combinators ///////////////////////////////////////////////////////////


:: List a :== [a]
empty :== isEmpty


(|||) infixr 2 //:: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) l r x :== l x || r x


(&&&) infixr 3 //:: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) l r x :== l x && r x



// Editors //


view  label value :== viewInformation label [] value
edit  label value :== updateInformation label [] value
enter label       :== enterInformation label []



// Shares //


share  label value     :== sharedStore label value
watch  label     store :== viewSharedInformation label [] store
change label     store :== updateSharedInformation label [] store
select label default store :== updateMultipleChoiceWithShared label [] store default


($=) infixr 2 //:: ReadWriteShared r w -> (r -> w) -> Task w
($=) share fun :== upd fun share



// Steps //


ok :== const True


// Internal step (>>*)
(>>>) infixl 1 //:: Task a -> List ( a -> Bool, a -> Task b ) -> Task b
(>>>) task options :== task >>* map trans options
where
  trans ( p, t ) = OnValue (ifValue p t)


// Internal bind (>>~)
(>>=) infixl 1 //:: Task a -> (a -> Task b) -> Task b
(>>=) task cont = task >>> [ ( ok, cont ) ]


// Internal ignore (>>|)
(>>) infixl 1 //:: Task a -> Task b -> Task b
(>>) task next = task >>= \_ -> next


// External step (>>*)
(>?>) infixl 1 //:: Task a -> List ( String, a -> Bool, a -> Task b ) -> Task b
(>?>) task options :== task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


// External bind (>>=)
(>>?) infixl 1 //:: Task a -> (a -> Task b) -> Task b
(>>?) task cont :== task >?> [ ( "Continue", ok, cont ) ]


// External ignore (>>= \_ -> )
(>?) infixl 1 //:: Task a -> (a -> Task b) -> Task b
(>?) task next :== task >?= \_ -> next



// Parallels and Choices //


// Internal choice
(<|>) infixr 3 //:: Task a -> Task a -> Task a
(<|>) :== (-||-)


// External choice
(<?>) infixr 3 //:: Task a -> Task a -> Task a
(<?>) fst snd :== return () >?> [ ( "First" , ok, fst ), (  "Second", ok, snd ) ]


// Parallel
(<&>) infixr 4 //:: Task a -> Task b -> Task ( a, b )
(<&>) :== (-&&-)


// Parallel preference
(<&) infixl 4 //:: Task a -> Task b -> Task a
(<&) :== (-||)
(&>) infixr 4 //:: Task a -> Task b -> Task b
(&>) :== (||-)


// Fail
fail :: Task a
fail = transform (\_ -> NoValue) (return ())



// Helpers /////////////////////////////////////////////////////////////////////



// Data ////////////////////////////////////////////////////////////////////////



// Stores //////////////////////////////////////////////////////////////////////



// Checks //////////////////////////////////////////////////////////////////////



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task ...
main =
  ...



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask ...


Start :: *World -> *World
Start world = startEngine main world
