#Jackson Anderson
#Lab 09, EBIO4420
#March 14, 2019


install.packages("lubridate")
library(lubridate)

dat <- read.csv("/Users/jacksonanderson/Desktop/EBIO4420/compBioSandbox/CompBio_on_git/Datasets/Cusack_et_al/Cusack_et_al_random_versus_trail_camera_trap_data_Ruaha_2013_14.csv", stringsAsFactors = F)
head(dat)
str(dat)


testVec <- dat$DateTime[1:5]
temp <-dmy_hm(testVec, truncated = 0) #converts to POSIXct 
str(temp)

#converting date/time data in "dat" to POSIXct format

dat[,6] <- dmy_hm(dat[,6])
str(dat) #This seems to be successful ( I looked through all the 2013 year entries, and they all had the same format), but I am still trying to figure out how to get rid for the redundant "seconds" for the time data, as non of the entries have "seconds" time entries





