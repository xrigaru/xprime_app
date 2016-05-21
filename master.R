source("DivisibleBy2.R")
source("DivisibleBy5.R")
source("DivisibleBy3.R")
source("isDivisible.R")
source("isDivisible.R")
source("isMod6Prime.r")
source("isMod5.r")

DivisibleBy2(345)

isDivisible(40, c(3,5,7))

isMod6Prime(15678942577)

x<-unlist(lapply(enteros, DivisibleBy2))

enteros[!x]


#Filter list to obtain number mod 6 equal 1 or 5
sapply(lista,isMod6Prime)
mod6List = lista[sapply(lista,isMod6Prime)==TRUE]

mod6List = mod6List[sapply(mod6List, isMod5)!=TRUE]
