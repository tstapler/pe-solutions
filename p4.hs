import Control.Monad
main :: IO()
main = print(lrgst_palindrome(999))

digits n = map (\x -> read [x] :: Int) (show n)

combinations n = [ i * j | i <- [n,n-1..1], j <- [n,n-1..1]]

lrgst_palindrome x = maximum([ n | n <- combinations(x),  digits(n) == reverse(digits(n))])

