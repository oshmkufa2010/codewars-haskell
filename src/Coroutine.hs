{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Coroutine where

    import Control.Monad (ap, forever, (>=>))
    -- import Preloaded
    
    -- Preloaded contains the following:
    
    newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)
    
    data Command r u d a =
      Done a
      | Out d (Coroutine r u d a)
      | In (u -> Coroutine r u d a) deriving Functor
    
    -- Useful alias
    apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
    apply = runCoroutine
    
    instance Applicative (Coroutine r u d) where
      pure = return
      (<*>) = ap
    
    instance Monad (Coroutine r u d) where
      return x = Coroutine (\k -> k (Done x))
      -- (>>=) :: Coroutine r u d a -> (a -> Coroutine r u d b) -> Coroutine r u d b
      -- k :: Command r u d a -> r
      f >>= g  = Coroutine $ \k -> 
        runCoroutine f $ \case
          Done a -> runCoroutine (g a) k
          Out d nextCoroutine -> k $ Out d (nextCoroutine >>= g)
          In resume -> let command = In (resume >=> g) in k command

    -- k :: Command r u d a -> r
    (>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
    p1 >>> p2 = Coroutine $ \k ->
      runCoroutine p2 $ \case
        Done a -> k (Done a)
        Out d nextP2 -> k (Out d (p1 >>> nextP2))
        In resume -> runCoroutine p1 $ \case 
          Done a -> k (Done a)
          Out m nextP1 -> runCoroutine (nextP1 >>> resume m) k
          In p1Resume -> k (In (\u -> p1Resume u >>> p2))

    -- It might be useful to define the following function
    pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
    pipe2 c f = Coroutine $ \k ->
      runCoroutine c $ \case
        Done a -> k (Done a)
        Out m nextCoroutine -> runCoroutine (nextCoroutine >>> f m) k
        In resume -> k (In (\u -> pipe2 (resume u) f))

    
    -- Library functions
    
    output :: a -> Coroutine r u a ()
    output v = Coroutine (\k -> k $ Out v $ return ())
    
    input :: Coroutine r v d v
    input = Coroutine (\k -> k (In $ \v -> return v))
    
    produce :: [a] -> Coroutine r u a ()
    produce = foldr (\x c -> Coroutine $ \k -> k $ Out x c) (return ())
    
    consume :: Coroutine [t] u t a -> [t]
    consume c = apply c $ \case
      Done _ -> [] 
      Out t nextCoroutine -> t : consume nextCoroutine
      -- In resume -> consume $ Coroutine $ \k -> k (In (produce . consume . resume ))
      In _ -> []
    
    filterC :: (v -> Bool) -> Coroutine r v v ()
    filterC p = Coroutine $ \k ->
      k $ In (\v -> if p v then Coroutine (\k' -> k' $ Out v (filterC p)) else filterC p) 
    
    limit :: Int -> Coroutine r v v ()
    limit n
      | n > 0 = Coroutine $ \k -> k $ In (\v -> Coroutine (\k' -> k' $ Out v (limit (n - 1))))
      | n <= 0 = Coroutine $ \k -> k $ In (\v -> return ())
    
    suppress :: Int -> Coroutine r v v ()
    suppress n
      | n > 0 = Coroutine $ \k -> k $ In (\v -> suppress (n - 1))
      | n <= 0 = Coroutine $ \k -> k $ In (\v -> Coroutine (\k' -> k' $ Out v (suppress 0)))
    
    add :: Coroutine r Int Int ()
    add = Coroutine $ \k ->
      k $ In $ \v1 -> Coroutine $ \k' -> k' $ In (\v2 -> Coroutine $ \k'' -> k'' $ Out (v1 + v2) add)
    
    duplicate :: Coroutine r v v ()
    duplicate = Coroutine $ \k ->
      k $ In $ \v -> Coroutine $ \k' -> k' $ Out v (Coroutine $ \k'' -> k'' $ Out v duplicate)
    
    mapC :: (v -> w) -> Coroutine r v w ()
    mapC f = Coroutine $ \k ->
      k $ In $ \v -> Coroutine $ \k' -> k' $ Out (f v) (mapC f)
       
    -- Programs
    -- 1. A program which outputs the first 5 even numbers of a stream.
    -- 2. A program which produces a stream of the triangle numbers 
    -- 3. A program which multiplies a stream by 2
    -- 4. A program which sums adjacent pairs of integers
    
    p1, p2, p3, p4 :: Coroutine r Int Int ()
    
    p1 = filterC even >>> limit 5
    p2 = produce [1..] >>> mapC (\n -> n * (n + 1) `div` 2)
    p3 = mapC (*2)
    p4 = (Coroutine $ \k -> k $ In (\v -> Coroutine $ \k' -> k' $ Out v duplicate)) >>> add
