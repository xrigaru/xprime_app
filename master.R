source("DivisibleBy2.R")
source("DivisibleBy5.R")
source("DivisibleBy3.R")
source("isDivisible.R")



DivisibleBy2(345)

isDivisible(40, c(3,5,7))


x<-unlist(lapply(enteros, DivisibleBy2))

enteros[!x]
