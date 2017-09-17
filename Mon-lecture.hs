module MonadFun where
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

instance Functor Hopefully where
    fmap = liftM
 
instance Applicative Hopefully where
    pure  = return
    (<*>) = ap

instance Functor (State s) where
    fmap = liftM
 
instance Applicative (State s) where
    pure  = return
    (<*>) = ap



{- 
   Let's write an interpretor for a simple expression language with the following 
   in-memory representation: 
-}
data Exp = Plus  Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div   Exp Exp
         | Const Float
      deriving (Show)


{- Some example terms -}
expA = (Div (Const 12)
            (Plus (Const 4) (Const 2)))

buggyExpA = Plus (Const 4) 
                 (Div (Const 2) (Const 0))

buggyExpB = (Div (Plus (Const 4) (Const 2)) 
                 (Times (Const 0) (Const 10)))



{- Original, simple interpreter -}
eval :: Exp -> Float
eval (Plus  e1 e2) = (eval e1) + (eval e2)
eval (Minus e1 e2) = (eval e1) - (eval e2)
eval (Times e1 e2) = (eval e1) * (eval e2)
eval (Div   e1 e2) = (eval e1) / (eval e2)
eval (Const i)     = i

answerA = eval expA
answerBugA = eval buggyExpA
answerBugB = eval buggyExpB



-- Suppose we want to improve this code by detecting attempts to divide by zero.
-- First, we need a type to record whether there is an error:

data Hopefully a = Ok a | Error String
 deriving Show



{-- 
 Original interpreter extended to check for division by zero.
--}
eval1 :: Exp -> Hopefully Float
eval1 (Plus  e1 e2) = 
  case eval1 e1 of 
    Error s -> Error s
    Ok v1 -> case eval1 e2 of
              Error s -> Error s
              Ok v2 -> Ok (v1 + v2)
eval1 (Minus e1 e2) = 
  case eval1 e1 of 
    Error s -> Error s
    Ok v1 -> case eval1 e2 of
              Error s -> Error s
              Ok v2 -> Ok (v1 - v2)
eval1 (Times e1 e2) = 
  case eval1 e1 of 
    Error s -> Error s
    Ok v1 -> case eval1 e2 of
              Error s -> Error s
              Ok v2 -> Ok (v1 * v2)
eval1 (Div   e1 e2) = 
  case eval1 e1 of 
    Error s -> Error s
    Ok v1 -> case eval1 e2 of
              Error s -> Error s
              Ok v2 -> if v2 == 0 then Error "Division by zero" else Ok (v1 / v2)
eval1 (Const i)     = Ok i

answer1A = eval1 expA
answerBug1A = eval1 buggyExpA
answerBug1B = eval1 buggyExpB















{-- 
    This solution exposes a lot of ugly plumbing.
    Every time an expression evalutes to Error, the Error 
    propagates to the final result.

    We can abstract this behavior to a higher-order function.
--}

ifOKthen :: Hopefully a -> (a -> Hopefully b) -> Hopefully b
e `ifOKthen` k = 
  case e of 
    Ok v -> k v
    Error s -> Error s
 


{-- Interpreter that uses the `ifOKthen` combinator --}
eval2 :: Exp -> Hopefully Float
eval2 (Plus  e1 e2) = 
   (eval2 e1) `ifOKthen` (\v1 -> 
   (eval2 e2) `ifOKthen` (\v2 -> 
   Ok (v1 + v2)))
eval2 (Minus e1 e2) = 
   (eval2 e1) `ifOKthen` (\v1 -> 
   (eval2 e2) `ifOKthen` (\v2 -> 
   Ok (v1 - v2)))
eval2 (Times e1 e2) =
   (eval2 e1) `ifOKthen` (\v1 -> 
   (eval2 e2) `ifOKthen` (\v2 -> 
   Ok (v1 * v2)))
eval2 (Div   e1 e2) = 
   (eval2 e1) `ifOKthen` (\v1 -> 
   (eval2 e2) `ifOKthen` (\v2 -> 
   if v2 == 0 then Error "Division by Zero" else Ok (v1 / v2)))
eval2 (Const i)     = Ok i

answer2A = eval2 expA
answerBug2A = eval2 buggyExpA
answerBug2B = eval2 buggyExpB









{-
   Note the similarity between the type for Ok and ifOKthen and
   the types for return and bind for the IO Monad:

     Ok       :: a -> Hopefully a
     ifOKthen :: Hopefully a -> (a -> Hopefully b) -> Hopefully b

     return   :: a -> IO a
     (>>=)    :: IO a -> (a -> IO b) -> IO b

   This similarity isn't an accident.  Ok and ifOKthen form a monad
   just like return and (>>=) for the IO Monad.  Because this structure
   is recurring,  Haskell provides a type constructor class Monad:  

     class Monad m where
       return :: a -> m a
       (>>=)  :: m a -> (a -> m b) -> m b

   By making Hopefully an instance of the type constructor class Monad, 
   we can use the do notation for Hopefully just like we use it for IO.  
-} 
instance Monad Hopefully where 
  return = Ok
  (>>=)  = ifOKthen
 

{-- Monadic error-checking interpreter --}   
eval3 :: Exp -> Hopefully Float
eval3 (Plus  e1 e2) = do
  { v1 <- eval3 e1
  ; v2 <- eval3 e2
  ; return (v1 + v2)
  }
eval3 (Minus e1 e2) = do
  { v1 <- eval3 e1
  ; v2 <- eval3 e2
  ; return (v1 - v2)
  }
eval3 (Times e1 e2) = do 
  { v1 <- eval3 e1
  ; v2 <- eval3 e2
  ; return (v1 * v2)
  }
eval3 (Div   e1 e2) = do 
  { v1 <- eval3 e1
  ; v2 <- eval3 e2
  ; if v2 == 0 then Error "Division by Zero" else return (v1 / v2)
  }
eval3 (Const i)     = return i




answer3A = eval3 expA
answerBug3A = eval3 buggyExpA
answerBug3B = eval3 buggyExpB










{-- 
  Suppose we wanted to modify our original evaluator to count the number of divisions.
  We're going to need to thread the count through all the compuations.
  There's a monad for that: the State monad.
  Let's assume the State monad already exists.
  Including operations 
    'get' for retrieving the state and
    'put' for setting the state.

    get :: State Int Int
    put :: Int -> State Int ()

  In the type of 'get', the first Int is the type of the state being stored.
  The second Int is the type of value returned when invoking the get action.
  If you think  about what 'get' is doing, you can see why these two types
  must be the same.  
--}

evalCD :: Exp -> State Int Float
evalCD (Plus  e1 e2) = do 
  { v1 <- evalCD e1
  ; v2 <- evalCD e2
  ; return (v1 + v2)
  }
evalCD (Minus e1 e2) = do
  { v1 <- evalCD e1
  ; v2 <- evalCD e2
  ; return (v1 - v2)
  } 
evalCD (Times e1 e2) = do
  { v1 <- evalCD e1
  ; v2 <- evalCD e2
  ; return (v1 * v2)
  } 
evalCD (Div   e1 e2) = do
  { v1 <- evalCD e1
  ; v2 <- evalCD e2
  ; count <- get
  ; put (count + 1)
  ; return (v1 / v2)
  } 
evalCD (Const i)     = return i


-- We can't use evalCD to evaluate our examples yet because we still have to define the state monad.











{- 
   So, let's define the State monad 
   First, we need the type for the monad:
-}
data State s a = ST {runState :: s -> (a,s)}
{- 
   This datatype declaration introduces the following two functions:
     runState :: State s a -> s -> (a, s)
     ST       :: (s -> (a, s)) -> State s a 
-}

instance Monad (State s) where 
  return = returnState
  (>>=)  = bindState

-- return :: Monad m => a -> m a
returnState :: a -> State s a
returnState e = ST (\origState -> (e,origState))

-- bind :: Monad m => m a -> ( a -> m b ) -> m b
bindState :: State s a -> (a -> State s b) -> State s b
bindState st1 k = ST ( \origState -> 
                          let (newValue, newState)     = runState st1 origState
                              (finalValue, finalState) = runState (k newValue) newState
                          in (finalValue, finalState))



-- Get the value of the state, leave state value unchanged
get :: State s s
get = ST (\origState -> (origState,origState))



-- make put's argument the new state, return the unit value
put :: s -> State s ()
put newState = ST (\origState -> ((),newState))


-- before update, the state has value s.
-- return s, replace s with f s.
update :: (s->s) -> State s s
update f = ST(\origState -> (origState, f origState))


-- Given that these operations are now defined, we can now use evalCD.
--   evalCD :: State Int Float
-- ( The float is the numeric value of the expression; 
--   the int is the count of the number of divisions.)

-- To execute the action of the State monad, we use the function
--    runState :: State s a -> s -> (a, s)
-- It takes an element of the monad, an initial state, and returns
-- a pair of the final answer and the final state.

answerCD = runState (evalCD expA) 0
-- (2.0,1)



evalC :: Exp -> Int -> (Int, Float)
evalC (Plus  e1 e2) s = 
  let (s1,v1) = evalC e1 s
      (s2,v2) = evalC e2 s1
  in (s, v1 + v2)
evalC (Minus e1 e2) s = 
  let (s1,v1) = evalC e1 s
      (s2,v2) = evalC e2 s1
  in (s, v1 - v2)
evalC (Times e1 e2) s = 
  let (s1,v1) = evalC e1 s
      (s2,v2) = evalC e2 s1
  in (s, v1 * v2)
evalC (Div e1 e2) s = 
  let (s1,v1) = evalC e1 s
      (s2,v2) = evalC e2 s1
  in (s2 + 1, v1 / v2)
evalC (Const i) s = (s, i)

(countCA,anwerCA) = evalC expA 0
-- (1,2.0)


