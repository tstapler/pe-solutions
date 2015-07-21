import Data.List.Ordered

main = print (lrg_prime_factor(600851475143))

primesTo :: Integer -> [Integer]
primesTo m = 2: sieve [3,5..m] where
    sieve (p:xs)
      | p *p > m = p : xs
      | otherwise = p: sieve (xs `minus` [p*p,p*p+2*p..])

lrg_prime_factor :: Integer -> Integer
lrg_prime_factor n = last(primesTo(n))
