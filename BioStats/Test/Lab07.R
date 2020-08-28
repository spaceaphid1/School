#Jackson Anderson
#Computational Biology
#Lab 07: Writing Functions

# Problem 1

#writing a function to calculate traingle area given two user input values
triangleArea <- function( b, h ) {
  area <- .5 * b * h
  return( area )
}

triangleArea( 10, 9 ) #testing the triangleArea function

# Problem 2

# 2a: making a function that returns absolute value of the users input value
myAbs <- function( x ) {
  absVal <- if ( x < 0 ) {
    x * (-1)
  } else {
    x
  }
  return(absVal)
}

myAbs(5)
myAbs(-2.3)

# 2b: making an absolute value function that works on vectors; used indexing
x <- c(1.1, 2, 0, -4.3, 9, -12)
myAbsVec <- function(x) {
  for ( i in 1:length(x)) {
    if ( x[i] < 0 ) {
      x[i] <- x[i] * -1
    } else {
      x[i]
    }
  }
  return(x)
}

myAbsVec(x)


# Part 3
#Writing a function that returns a vector of the first n Fib. numbers at a designated starting element x in the Fib. sequence.

#NOTE: I'm pretty sure I did not do this correctly, as the function I made makes 3a and 3b seem somewhat redundant (?). I finished this function before I got sick. When I tried to redo it WHILE sick, my brain was not up to the challenge. I am more than happy to resubmit when I am feeling better, but as of now (Wed March 4), I figured it best to submit what I have currently, rather than nothing at all.
fibFunc <- function( x, n ) {
  if ( x <= 0 | n <= 0 ) { #checking user input for negative values and 0
    print( "Input must be greater than zero")
  } else {
  fibDat <- rep(0, (x+n) ) #creating empty vec of length x+n
  fibDat <- replace (fibDat, 2, 1) #replacing 2nd element in fibDat with 1
  for ( i in 3:length(fibDat)) #loop that calculates fib seq. to the x+n place
    fibDat[i] <- fibDat[i-1] + fibDat[i-2]
  return(fibDat[x:(x+n)]) #returning the users starting number, x, and n number of fib numbers after x
  }
}


#Part 4:

# 4a: writing a function that calculates the squared difference of two user input values
sqFunc <- function (a, b) {
  sqDiff <- (a - b) ^ 2
  return(sqDiff)
}

sqFunc(3,5) #testing the function
sqFunc(c(2,4,6), 4)#testing the function with a vector input

# 4b: writing a function that calculates the mean of the elements within a vector
meanFunc <- function( x ) {
  numElm <- length(x)
  sum <- sum(x)
  avg <- sum / numElm
  return(avg)
}

meanFunc(c(5, 15, 10))# testing the meanFunc
# 4b Cont.: testing meanFunc on data imported as a .csv
meanDat <- DispersalMasterData
str(meanDat$focalFlowerHeight_cm)

#4b Cont.: removing NA's
meanDat <- meanDat %>%
  filter(!is.na(focalFlowerHeight_cm))

#first method: using the meanFunc on data stored in a variable in the data.frame
meanFunc(meanDat$focalFlowerHeight_cm) 

#second method: storing data from variable "x" in a vector, then using meanFunc to calculate the mean of the data in the vector
meanDatVec <- meanDat$focalFlowerHeight_cm
meanFunc(meanDatVec)

# 4c: writing a sum of squares function: taking elements from vector (given by user); using the meanFunc and the sqFunc defined above to calculate the sum of the squares for the given input vector. 
sumSqFunc <- function (x) {
  emptyVec <- rep(0, length(x))
  mean1 <- meanFunc(x)
  for ( i in 1:length(x)) {
    emptyVec[i] <- sqFunc(x[i], mean1)
  }
  return(sum(emptyVec))
}

sumSqFunc(meanDat$focalFlowerHeight_cm)#checking the sumSqFunc 


# Spinning to knitr: PDF
knitr::knit2pandoc("/cloud/project/BioStats/Test/Lab07.R")
library(tinytex)
