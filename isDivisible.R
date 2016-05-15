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

isDivisible <- function(valor, primeList = NULL){
  
  if(is.null(primeList)){
    stop("method should be a list of prime numbers")
  }
  
  
  
  # Set a vector 'mod' with the result to calculate the modules
  # of 'value' for each number in 'primeList'
  mod = valor %% primeList
  
  # It must calculate the length of the resulting vector 
  # to checking for zero values mudulo
  # If greater than zero, it returns 'TRUE', indicating that it is divisible
  return (length(mod[mod == 0]) > 0)
  
}