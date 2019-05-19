primes = sieve[2..]
  where
  sieve (x:xs)=
    x:(sieve[y| y<-xs, mod y x > 0])
