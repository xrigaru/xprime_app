
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

file_basePrime = "basePrime.csv"
logFile = "logFile.csv"
resumeBasePrime = "resumeBasePrime.csv"

#initial headers of log file
logColNames = c("lowRangeNumber","highRangeNumber","lengthBasePrimeRange", "lengthBasePrime", "sqrtMaxRange", "lengthRangeAnalysis", "Time", "Date")

# conditional to verify if exists file basePrime.csv
if(file.exists("basePrime.csv")){
  resumeBasePrimeRecord = read.table(resumeBasePrime, header = FALSE, colClasses = "integer")
  lengthBasePrime = resumeBasePrimeRecord[1,1]
  #lowRangeNumber = resumeBasePrimeRecord[1,2]
  #highRangeNumber = resumeBasePrimeRecord[1,3]
  flag = TRUE
  
}else{
  # initial cicle
  
  # this is the initial variable of primer numbers generator
  basePrime = c(2,3)
  lengthBasePrime = 2
  
  #write resumeBasePrime first time
  write.table(lengthBasePrime, file = resumeBasePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)
  
  #write basePrime to txt
  write.table(basePrime, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)
  lengthBasePrime = length(basePrime)
  rm(basePrime)
  
  #initial range
  lowRangeNumber = 2
  highRangeNumber = 3
  scopeRange = lowRangeNumber + highRangeNumber
  
  # set range numbers to analice
  rangeAnalysis = (highRangeNumber+1):scopeRange
  lengthRangeAnalysis = scopeRange - highRangeNumber
  
  # first fase to simplify range
  rangeAnalysis = rangeAnalysis[sapply(rangeAnalysis,isMod6Prime)==TRUE]

  #high value in range: get the square root of maximum value of range
  sqrtMaxRange = round(sqrt(max(rangeAnalysis)))
  
  #get the range of primes in basePrime vector low to sqrtMaxRange
  basePrime = read.table(file_basePrime, header = FALSE, colClasses = "integer", nrow = lengthBasePrime)
  basePrimeRange = basePrime[basePrime<=sqrtMaxRange]
  rm(basePrime)
  
  #set rangeAnalysis to modify from use basePrimeRange
  rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, basePrimeRange)==FALSE]
  
  # write log file: initial record
  recordLogFile = list(lowRangeNumber, highRangeNumber, length(basePrimeRange), lengthBasePrime, sqrtMaxRange, lengthRangeAnalysis, Sys.time(), Sys.Date())
  write.table(recordLogFile, file = logFile, append = FALSE, quote = FALSE, row.names = FALSE, sep = ";", col.names = logColNames)
  rm(logColNames)
  rm(recordLogFile)
  
  #set new values to basePrime
  #basePrime = c(basePrime,rangeAnalysis)
  lengthBasePrime = lengthBasePrime + length(rangeAnalysis)
  
  #append values of rangeAnalysis to file basePrime txt
  write.table(rangeAnalysis, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #write resumeBasePrime to actualize length
  write.table(c(lengthBasePrime, lowRangeNumber, highRangeNumber), file = resumeBasePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)
  
  #
  # second cicle of calculations
  #
  
  #actualize the values of iteration 
  lowRangeNumber = highRangeNumber
  highRangeNumber = max(rangeAnalysis)
  scopeRange = lowRangeNumber + highRangeNumber

  # set range numbers to analice
  rangeAnalysis = (highRangeNumber+1):scopeRange
  lengthRangeAnalysis = scopeRange - highRangeNumber
  
  # first fase to simplify range
  rangeAnalysis = rangeAnalysis[sapply(rangeAnalysis,isMod6Prime)==TRUE]
  
  #high value in range: get the square root of maximum value of range
  sqrtMaxRange = round(sqrt(max(rangeAnalysis)))
  
  #get the range of primes in basePrime vector low to sqrtMaxRange
  basePrime = read.table(file_basePrime, header = FALSE, colClasses = "integer", nrow = lengthBasePrime)
  basePrimeRange = basePrime[basePrime<=sqrtMaxRange]
  rm(basePrime)
  
  #set rangeAnalysis to modify from use basePrimeRange
  rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, basePrimeRange)==FALSE]
  
  rangeAnalysis=rangeAnalysis[!is.na(rangeAnalysis)]
  
  #append data to the log file
  recordLogFile = list(lowRangeNumber, highRangeNumber, length(basePrimeRange), lengthBasePrime, sqrtMaxRange, lengthRangeAnalysis, Sys.time(), Sys.Date())
  write.table(recordLogFile, file = logFile, append = TRUE, quote = FALSE, row.names = FALSE, sep = ";", col.names = FALSE)
  
  #set new values to basePrime
  #basePrime = c(basePrime,rangeAnalysis)
  lengthBasePrime = lengthBasePrime + length(rangeAnalysis)
  
  #append values of rangeAnalysis to file basePrime txt
  write.table(rangeAnalysis, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #write resumeBasePrime to actualize length
  write.table(c(lengthBasePrime, highRangeNumber, max(rangeAnalysis)), file = resumeBasePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)

}

# make a loop of cicle of calculations

continue <- TRUE
conditionExitLoop = 10000

while (continue){
  
  #actualize the values of iteration 
  if (flag){
    lowRangeNumber = resumeBasePrimeRecord[2,1]
    highRangeNumber = resumeBasePrimeRecord[3,1]
    flag = FALSE
  }else{
    lowRangeNumber = highRangeNumber
    highRangeNumber = max(rangeAnalysis)
  }
  
  scopeRange = lowRangeNumber + highRangeNumber
  
  # set range numbers to analice
  rangeAnalysis = (highRangeNumber+1):scopeRange
  lengthRangeAnalysis = scopeRange - highRangeNumber
  
  # first fase to simplify range
  rangeAnalysis = rangeAnalysis[sapply(rangeAnalysis,isMod6Prime)==TRUE]
  
  #high value in range: get the square root of maximum value of range
  sqrtMaxRange = round(sqrt(max(rangeAnalysis)))
  
  #get the range of primes in basePrime vector low to sqrtMaxRange
  basePrime = read.table(file_basePrime, header = FALSE, colClasses = "integer", nrow = round(lengthBasePrime/3)+1)
  basePrimeRange = basePrime[basePrime<=sqrtMaxRange]
  rm(basePrime)
  
  #set rangeAnalysis to modify from use basePrimeRange
  for(i in seq(from=1, to=length(basePrimeRange), by=1)) {
    rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, basePrimeRange[i])==FALSE]
  }
  
  #clean NA to vector
  rangeAnalysis=rangeAnalysis[!is.na(rangeAnalysis)]
  
  #append data to the log file
  recordLogFile = list(lowRangeNumber, highRangeNumber, length(basePrimeRange), lengthBasePrime, sqrtMaxRange, lengthRangeAnalysis, Sys.time(), Sys.Date())
  write.table(recordLogFile, file = logFile, append = TRUE, quote = FALSE, row.names = FALSE, sep = ";", col.names = FALSE)
  
  #test if exists new prime number in range analysis
  if(length(rangeAnalysis)==0){
    print("Stop: don't have more prime numbers")
    flush.console()
    break()
  }
  
  #set new values to basePrime
  #basePrime = c(basePrime,rangeAnalysis)
  lengthBasePrime = lengthBasePrime + length(rangeAnalysis)
  
  #append values of rangeAnalysis to file basePrime txt
  write.table(rangeAnalysis, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #write resumeBasePrime to actualize length
  write.table(c(lengthBasePrime, highRangeNumber, max(rangeAnalysis)), file = resumeBasePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)
  
  if(highRangeNumber > conditionExitLoop){
    continue <- FALSE
  }
}

