-- We qualify Prelude because many of our names conflict
import Prelude (($),(++),(==),(.))
import qualified Prelude as P

-- We can represent a number `n` as a list of `n` elements.
data Natural = Nul | Cons Natural

instance {-# OVERLAPPING #-} P.Show Natural where
  show = P.show . toInt

toInt :: Natural -> P.Int
toInt Nul = 0
toInt (Cons n) = (1)P.+(toInt n)

get :: P.Int -> Natural
get 0 = Nul
get x = Cons $ get $ P.pred x

-- Succession is the wrapping of the existing list in a Cons
succ :: Natural -> Natural
succ x = Cons x

-- Addition is the concatenation of the lists of two numbers
--
-- a + 0 = a
-- 0 + b = b
-- a + Cons(b) = Cons (a + b)
add :: Natural -> Natural -> Natural
add a Nul = a
add Nul b = b
add a (Cons b) = Cons (add a b)

-- Multiplication is the accumulation of a list in multiples of `a`, decrementing `b` until it is an
-- empty list
--
-- a * 0 = 0
-- 0 * b = 0
mul :: Natural -> Natural -> Natural
mul _ Nul = Nul
mul Nul _ = Nul
mul a b = mulFn a b Nul where mulFn a Nul accum = accum
                              mulFn a (Cons b) accum = add a $ mulFn a b accum

main :: P.IO ()
main = do
  P.putStrLn $ ("0 = "++)               $ P.show $ get 0
  P.putStrLn $ ("4 = "++)               $ P.show $ get 4
  P.putStrLn $ ("Succ(0) = 1 = "++)     $ P.show $ succ $ get 0
  P.putStrLn $ ("3 + 7 + 5 = 15 = "++)  $ P.show $ (get 3) `add` (get 7) `add` (get 5)
  P.putStrLn $ ("3 * 7 = 21 = "++)      $ P.show $ (get 3) `mul` (get 7)
