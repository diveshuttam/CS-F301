divisors n = [x| x<-[1..n], (mod n x) == 0]
primes n = [x| x<- [1..n], (divisors x == [1,x])]
