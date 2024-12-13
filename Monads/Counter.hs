{-
This is the small example on how to write your own Monad that was sketched in
class on the December 12.
The goal is to create a Monad that "counts" the number of steps taken.
-}

{-
First we define a datatype that holds an arbitrary value, and a counter.
The first element is the counter Integer, the scond is the stored values.
-}

data Counter a = Counter Int a
  deriving (Show)

{-
To make "Counter" an instance of the Monad typeclass, we
need to implement two functions. The first, `return` puts
a value into the counter.  Since we didn't do any steps,
we will just initialize the counter to 0
-}

instance Monad Counter where
  return x = Counter 0 x

  {- The key function is the "Monadic bind" function.  It takes two arguments.
     The first argument is a counter containing some value `x`of type `a`.  You
     can see this as a counter that counted how many steps we took to build `x`.
     The second argument is a function that takes a value of type `a` and
     the computes some new value `y` of type `b`.  However, it has to return a new
     counter.  This means this function can count the number of steps it takes
     to build `y` from `x`.
     So in this case >>= has two tasks: it first needs to "unwrap" the value
     `x` from the counter (using pattern matching).
     Furthermore, it needs to compute the number of steps taken in total.
  -}
  -- (>>=) :: Counter a -> (a -> Counter b) -> Counter b
  (Counter n x) >>= f = case f x of
    (Counter m y) -> Counter (n + m ) y


-- We can also write a helper that gives us the count.
getCount :: Counter a -> Int
getCount (Counter n _) = n

-- You might have noticed, that so far we can't actually count anything, because
-- `return` always counts as 0 and >>= just sums the values (all 0) up.
-- Hence we need a helper that allows us to say some computation should be counted.
-- This simply means we put the value into a Counter with count 1.
count :: a -> Counter a
count x = Counter 1 x

-- Some examples:

-- Return "Counter 2 16"
a = return 5 >>= (\x -> count (x + 3)) >>= (\x -> count (x * 2))

-- Returns 2
b = getCount a

-- Returns "Counter 3 "hallo welt! :)"
c :: Counter String
c = do
  x <- return "hallo" -- we don't count this as a step
  y <- count $ x ++ " welt"
  z <- count $ y ++ "! "
  count (z ++ ":)")

-- Returns 2
d = getCount c

-- Technically, we also have to make Counter an instance
-- of the Functor and Applicative typeclass. Those are
-- defined below.

{-
We need to make Counter a member of the Functor typeclass.
To do so we implement the `fmap` function.  This function
applies a function to the value stored in our counter.
It doesn't change the count.
-}

instance Functor Counter where
  fmap f (Counter n x) = Counter n (f x)

{-
We also need to implement the Applicative typeclass.
To do so, we need to implement the `pure` function,
which "wraps" a value into a counter.
We also have to implement the `<*>` functon which
applies a function within a counter to a value in a counter.
-}

instance Applicative Counter where
  pure = Counter 0
  (Counter n f) <*> (Counter m x) = Counter (n + m) (f x)

