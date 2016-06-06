
#clean workspace
rm(list = ls())
cat("\014") 

#library of parallel work
library(doParallel)
registerDoParallel()

# load sources
source("isMod6Prime.r")
source("isMod5.r")
source("isModuleN.r")
source("getIncrementToBasePrimeLoad.r")

file_basePrime = "basePrime.csv"
logFile = "logFile.csv"
resumeBasePrime = "resumeBasePrime.csv"
flag = FALSE

#detect cores and create cluster with all cores
mycluster = makeCluster(max(1, detectCores()-1))

#register our cluster
registerDoParallel(mycluster)

#initial headers of log file
logColNames = c("lowRangeNumber","highRangeNumber","lengthBasePrimeRange", "lengthBasePrime", "sqrtMaxRange", "lengthRangeAnalysis", "Time", "Date", "nrowsReadBasePrime")

# conditional to verify if exists file basePrime.csv
if(file.exists("basePrime.csv")){
  resumeBasePrimeRecord = read.table(resumeBasePrime, header = FALSE, colClasses = "integer")
  lengthBasePrime = resumeBasePrimeRecord[1,1]
  flag = TRUE
  
  counterLoop = resumeBasePrimeRecord[4,1]
  nrowsReadBasePrime = 0
  
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
  basePrime = read.table(file_basePrime, header = FALSE, colClasses = "integer", nrows = lengthBasePrime)
  basePrimeRange = basePrime[basePrime<=sqrtMaxRange]
  rm(basePrime)
  
  #set rangeAnalysis to modify from use basePrimeRange
  rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, basePrimeRange)==FALSE]
  
  # write log file: initial record
  recordLogFile = list(lowRangeNumber, highRangeNumber, length(basePrimeRange), lengthBasePrime, sqrtMaxRange, length(rangeAnalysis), Sys.time(), Sys.Date(),0)
  write.table(recordLogFile, file = logFile, append = FALSE, quote = FALSE, row.names = FALSE, sep = ";", col.names = logColNames)
  rm(logColNames)
  rm(recordLogFile)
  
  #set new values to basePrime
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
  basePrime = read.table(file_basePrime, header = FALSE, colClasses = "integer", nrows = lengthBasePrime)
  basePrimeRange = basePrime[basePrime<=sqrtMaxRange]
  rm(basePrime)
  
  #set rangeAnalysis to modify from use basePrimeRange
  rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, basePrimeRange)==FALSE]
  
  rangeAnalysis=rangeAnalysis[!is.na(rangeAnalysis)]
  
  #append data to the log file
  recordLogFile = list(lowRangeNumber, highRangeNumber, length(basePrimeRange), lengthBasePrime, sqrtMaxRange, length(rangeAnalysis), Sys.time(), Sys.Date(),0)
  write.table(recordLogFile, file = logFile, append = TRUE, quote = FALSE, row.names = FALSE, sep = ";", col.names = FALSE)
  
  #set new values to basePrime
  lengthBasePrime = lengthBasePrime + length(rangeAnalysis)
  
  #append values of rangeAnalysis to file basePrime txt
  write.table(rangeAnalysis, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #write resumeBasePrime to actualize length
  write.table(c(lengthBasePrime, highRangeNumber, max(rangeAnalysis)), file = resumeBasePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)

  counterLoop = 3
  nrowsReadBasePrime = 0
}

# make a loop of cicle of calculations

continue <- TRUE
conditionExitLoop = 100000000
topBasePrimeRange = highRangeNumber

system.time(
  while (continue){
  
  #actualize the values of iteration 
  if (flag){
    lowRangeNumber = resumeBasePrimeRecord[2,1]
    highRangeNumber = resumeBasePrimeRecord[3,1]
    flag = FALSE
  }else{
    lowRangeNumber = topBasePrimeRange
    highRangeNumber = min(rangeAnalysis)
    topRangeNumber = max(rangeAnalysis)
    topBasePrimeRange = min(rangeAnalysis)
  }
  
  scopeRange = lowRangeNumber + highRangeNumber
  
  # set range numbers to analice
  topBasePrimeRange = max(rangeAnalysis)
  rangeAnalysis = (highRangeNumber+1):scopeRange
  rangeAnalysis = rangeAnalysis[rangeAnalysis > topRangeNumber]
  lengthRangeAnalysis = scopeRange - highRangeNumber
  
  # first fase to simplify range
  #rangeAnalysis = rangeAnalysis[sapply(rangeAnalysis,isMod6Prime)==TRUE]
  rangeAnalysis = rangeAnalysis[parSapply(mycluster, rangeAnalysis,isMod6Prime)==TRUE]
  
  #high value in range: get the square root of maximum value of range
  sqrtMaxRange = round(sqrt(max(rangeAnalysis)))
  
  #get the range of primes in basePrime vector low to sqrtMaxRange
  if(counterLoop > 12){
    #nrowsReadBasePrime = round(lengthBasePrime*0.5)-round(lengthBasePrime*(0.15*counterLoop))
    #counterLoop = counterLoop + 0.057
    nrowsReadBasePrime = round(getIncrementToBasePrimeLoad (counterLoop))
  }else{
    nrowsReadBasePrime = round(lengthBasePrime/3)+1
  }
  counterLoop = counterLoop + 1
  
  basePrime = read.table(file_basePrime, header = FALSE, colClasses = "integer", nrows = nrowsReadBasePrime)
  basePrimeRange = basePrime[basePrime<=sqrtMaxRange]
  rm(basePrime)
  
  #set rangeAnalysis to modify from use basePrimeRange
  for(i in seq(from=1, to=length(basePrimeRange), by=1)) {
    #rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, basePrimeRange[i])==FALSE]
    rangeAnalysis = rangeAnalysis[parSapply(mycluster, rangeAnalysis, FUN = isModuleN, basePrimeRange[i])==FALSE]
  }
  
  #clean NA to vector
  rangeAnalysis = rangeAnalysis[!is.na(rangeAnalysis)]
  rangeAnalysis = rangeAnalysis[rangeAnalysis > topRangeNumber]
  
  #append data to the log file
  recordLogFile = list(lowRangeNumber, highRangeNumber, length(basePrimeRange), lengthBasePrime, sqrtMaxRange, length(rangeAnalysis), Sys.time(), Sys.Date(), nrowsReadBasePrime)
  write.table(recordLogFile, file = logFile, append = TRUE, quote = FALSE, row.names = FALSE, sep = ";", col.names = FALSE)
  
  #test if exists new prime number in range analysis
  if(length(rangeAnalysis) < 1){
    print("Stop: don't have more prime numbers")
    flush.console()
    break()
  }
  
  #set new values to basePrime
  lengthBasePrime = lengthBasePrime + length(rangeAnalysis)
  
  #key
  topBasePrimeRange = topRangeNumber
  
  #append values of rangeAnalysis to file basePrime txt
  write.table(rangeAnalysis, file = file_basePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #write resumeBasePrime to actualize length
  write.table(c(lengthBasePrime, highRangeNumber, max(rangeAnalysis)), file = resumeBasePrime, quote = FALSE,  row.names = FALSE, col.names = FALSE)
  
  if(highRangeNumber > conditionExitLoop){
    continue <- FALSE
  }
  }
)
#stop the cluster
stopCluster(mycluster)