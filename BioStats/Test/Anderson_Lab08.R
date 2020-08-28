# Jackson Anderson
# Lab 08
# Markdown and More Functions
# CompBio

library(knitr)

# Step 3:

# parts b-e
growthFunc <- function(r, K, t, n) { #establishing inputs and naming the function
  for ( i in seq( 2, t)) {
    n[i] <- n[i-1] + ( r *n[i-1] * ( K - n[i-1]) / K)
  }
  for (i in 1:t) {
    popDat[i] <- n[i] #creating new vector of length n and storing function values in it
  }
  timeVec <- c(1:t) #creating time vector of length t
  plot( timeVec, popDat, xlab = "Population Abundance", ylab = "Generations", main = "Population Abunance Over Time") #plotting the values on a graph
  generations <- c(timeVec)#creating a  vector for generations using timeVec
  abundance<- c(popDat)#creating a vector for pop. abundances using popDat
  newData <- data.frame(generations, abundance)#creating new data frame using generations and abundance
  write.csv( newData, file = "Growth Function Data.csv")#writing newData to a .csv and saving in current working directory
}

# Inputs in parentheses below, in this order: r, K, t, n
growthFunc()#user inputs function values here and runs

