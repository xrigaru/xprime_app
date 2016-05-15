#
#   Name:   DivisibleBy2.R
#   Purpose: This program return TRUE or FALSE if input is even number
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

# input: numeric value

DivisibleBy2 <- function(inNumber = NULL)
{

  if(is.null(inNumber)){
    stop("method should be a number")
  }
  
  # split string
  num <- strsplit(toString(inNumber), split = character(0))
  
  # get coincidence between first rigth character and vector
  # if exists coincidence this function return boolean value: TRUE
  return(num[[1]][length(num[[1]])] %in% c(0,2,4,6,8))
  
}
