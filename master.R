
#clean workspace
rm(list = ls())
cat("\014") 

# load sources
#source("DivisibleBy2.R")
#source("DivisibleBy5.R")
#source("DivisibleBy3.R")
#source("isDivisible.R")
#source("isDivisible.R")
source("isMod6Prime.r")
source("isMod5.r")
source("isModuleN.r")

# initial cicle

# this is the initial variable of primer numbers generator
basePrime = c(2,3)
file_basePrime = "basePrime.csv"
logFile = "logFile.csv"
lengthBasePrime = 2

#write basePrime to txt
write.table(basePrime, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE);#rm(basePrime)
#lengthBasePrime = length(basePrime)

#initial headers of log file
logColNames = c("lowRangeNumber","highRangeNumber","lengthPTR", "lengthBP", "sqrtMR", "time", "date")

#initial range
lowRangeNumber = 2
highRangeNumber = 3

# move range up
temp=lowRangeNumber
lowRangeNumber = highRangeNumber
highRangeNumber = (temp+highRangeNumber)
rm(temp)

# set range numbers to analice
rangeAnalysis = (lowRangeNumber+1):highRangeNumber

# first fase to simplify range
rangeAnalysis = rangeAnalysis[sapply(rangeAnalysis,isMod6Prime)==TRUE]

#high value in range: get the square root of maximum value of range
sqrtMaxRange = round(sqrt(max(rangeAnalysis)))

#get the range of primes in basePrime vector low to sqrtMaxRange
#read.table(basePrime, header = FALSE, colClasses = "integer")
primeTestRange = basePrime[basePrime<=sqrtMaxRange]

#set rangeAnalysis to modify from use primeTestRange
rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, primeTestRange)==FALSE]

# write log file: initial record
recordLogFile = list(lowRangeNumber, highRangeNumber, length(primeTestRange), length(basePrime), sqrtMaxRange, Sys.time(), Sys.Date())
write.table(recordLogFile, file = logFile, append = FALSE, quote = FALSE, row.names = FALSE, sep = ";", col.names = logColNames)

#set new values to basePrime
basePrime = c(basePrime,rangeAnalysis)

#append basePrime to txt
write.table(basePrime, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)

# second cicle of calculations

# move range up
temp=lowRangeNumber
lowRangeNumber = highRangeNumber
highRangeNumber = (temp+highRangeNumber)
rm(temp)

# set range numbers to analice
rangeAnalysis = (lowRangeNumber+1):highRangeNumber

# first fase to simplify range
rangeAnalysis = rangeAnalysis[sapply(rangeAnalysis,isMod6Prime)==TRUE]

#high value in range: get the square root of maximum value of range
sqrtMaxRange = round(sqrt(max(rangeAnalysis)))

#get the range of primes in basePrime vector low to sqrtMaxRange
primeTestRange = basePrime[basePrime<=sqrtMaxRange]

#set rangeAnalysis to modify from use primeTestRange
rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, primeTestRange)==FALSE]

rangeAnalysis=rangeAnalysis[!is.na(rangeAnalysis)]

#append data to the log file
recordLogFile = list(lowRangeNumber, highRangeNumber, length(primeTestRange), length(basePrime), sqrtMaxRange, Sys.time(), Sys.Date())
write.table(recordLogFile, file = logFile, append = TRUE, quote = FALSE, row.names = FALSE, sep = ";", col.names = FALSE)

#set new values to basePrime
basePrime = c(basePrime,rangeAnalysis)

#append basePrime to txt
write.table(basePrime, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)

# make a loop of cicle of calculations

continue <- TRUE
conditionExitLoop = 5000

while (continue){
  
  # move range up
  temp=lowRangeNumber
  lowRangeNumber = highRangeNumber
  highRangeNumber = (temp+highRangeNumber)
  rm(temp)
  
  # set range numbers to analice
  rangeAnalysis = (lowRangeNumber+1):highRangeNumber
  
  # first fase to simplify range
  rangeAnalysis = rangeAnalysis[sapply(rangeAnalysis,isMod6Prime)==TRUE]
  
  #high value in range: get the square root of maximum value of range
  sqrtMaxRange = round(sqrt(max(rangeAnalysis)))
  
  #get the range of primes in basePrime vector low to sqrtMaxRange
  primeTestRange = basePrime[basePrime<=sqrtMaxRange]
  
  #set rangeAnalysis to modify from use primeTestRange
  for(i in seq(from=1, to=length(primeTestRange), by=1)) {
    rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, primeTestRange[i])==FALSE]
  }
  
  #clean NA to vector
  rangeAnalysis=rangeAnalysis[!is.na(rangeAnalysis)]
  
  #append data to the log file
  recordLogFile = list(lowRangeNumber, highRangeNumber, length(primeTestRange), length(basePrime), sqrtMaxRange, Sys.time(), Sys.Date())
  write.table(recordLogFile, file = logFile, append = TRUE, quote = FALSE, row.names = FALSE, sep = ";", col.names = FALSE)
  
  if(length(rangeAnalysis)==0){
    print("Stop: don't have more prime numbers")
    flush.console()
    break()
  }
  
  #set new values to basePrime
  basePrime = c(basePrime,rangeAnalysis)
  
  #append basePrime to txt
  write.table(basePrime, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)
  
  
  if(highRangeNumber > conditionExitLoop){
    continue <- FALSE
  }
}








