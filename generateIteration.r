#
#   Name:   isDivisible.R
#   Purpose: This program return TRUE or FALSE if input value is divisible
#            if it is divisible by any of the number of the input vector
#    Copyright (C) 2015  Ricardo García Ruiz, xrigaru@gmail.com
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# input: numeric value to divisibility calculate
# input: vector with primes

print(highRangeNumber)
print(lowRangeNumber)


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
rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, primeTestRange[1])==FALSE]
rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, primeTestRange[2])==FALSE]

for(i in seq(from=1, to=length(primeTestRange), by=1)) {
  rangeAnalysis = rangeAnalysis[mapply(isModuleN, rangeAnalysis, primeTestRange[i])==FALSE]
}

rangeAnalysis=rangeAnalysis[!is.na(rangeAnalysis)]

if(length(rangeAnalysis)==0){
  print("Stop: don't have more prime numbers")
}

#set new values to basePrime
basePrime = c(basePrime,rangeAnalysis)


#write basePrime to txt

write.csv(basePrime, file="basePrime.csv", quote = FALSE,  sep = ";", row.names = FALSE, col.names = FALSE)
