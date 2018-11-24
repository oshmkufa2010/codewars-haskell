module FunctionalStreams where

    import Control.Arrow
    import Control.Applicative
    
    -- import Stream.Internal
    
    -- Defined in Stream.Internal:
    data Stream a = a :> Stream a
    infixr :>
    
    -- | Get the first element of a stream.
    headS :: Stream a -> a
    headS (x :> _) = x
    
    -- | Drop the first element of a stream.
    tailS :: Stream a -> Stream a
    tailS (_ :> xs) = xs
    
    
    -- {{{ Stream constructors
    
    -- | Construct a stream by repeating a value.
    repeatS :: a -> Stream a
    repeatS x = x :> repeatS x
    
    -- | Construct a stream by repeatedly applying a function.
    iterateS :: (a -> a) -> a -> Stream a
    iterateS f x = x :> iterateS f (f x)
    
    -- | Construct a stream by repeating a list forever.
    cycleS :: [a] -> Stream a
    cycleS xs = toStream $ concat $ repeat xs
        where toStream (y:ys) = y :> toStream ys
    
    -- | Construct a stream by counting numbers starting from a given one.
    fromS :: Num a => a -> Stream a
    fromS x = fromStepS x 1
    
    -- | Same as 'fromS', but count with a given step width.
    fromStepS :: Num a => a -> a -> Stream a
    fromStepS x s = iterateS (+ s) x
    
    -- }}}
    
    
    -- | Fold a stream from the left.
    foldrS :: (a -> b -> b) -> Stream a -> b
    foldrS f (x :> xs) = f x $ foldrS f xs
    
    -- | Filter a stream with a predicate.
    filterS :: (a -> Bool) -> Stream a -> Stream a
    filterS p (x :> xs)
        | p x = x :> xs'
        | otherwise = xs'
        where xs' = filterS p xs
            
    -- | Take a given amount of elements from a stream.
    takeS :: Int -> Stream a -> [a]
    takeS i s
        | i <= 0 = []
        | (x :> xs) <- s = x : takeS (i - 1) xs
    
    -- | Drop a given amount of elements from a stream.
    dropS :: Int -> Stream a -> Stream a
    dropS i s
        | i <= 0 = s
        | (x :> xs) <- s = dropS (i - 1) xs
    
    -- | Do take and drop simultaneous.
    splitAtS :: Int -> Stream a -> ([a], Stream a)
    splitAtS 0 s = ([], s)
    splitAtS i s
        | i <= 0 = ([], s)
        | (x :> xs) <- s = let (taked, remaining) = splitAtS (i - 1) xs in (x : taked, remaining)
    
    -- | Combine two streams with a function.
    zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
    zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys
    
    zipS :: Stream a -> Stream b -> Stream (a, b)
    zipS = zipWithS (,)
    
    instance Functor Stream where
        -- fmap :: (a -> b) -> Stream a -> Stream b
        fmap f (x :> xs) = f x :> fmap f xs
    
    instance Applicative Stream where
        -- pure :: a -> Stream a
        pure = repeatS
    
        -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
        (f :> fs) <*> xs = f (headS xs) :> (fs <*> xs)  
    
    -- | The stream of fibonacci numbers.
    fibS :: Stream Integer
    fibS = fibS' 0 1
        where fibS' x y = x :> (y :> fibS' (x + y) (x + y + y))
    
    -- | The stream of prime numbers.
    primeS :: Stream Integer
    primeS = sieve (2 :> fromStepS 3 2)
        where sieve (p :> ps) = p :> sieve (filterS (\x -> x `mod` p > 0) ps)
