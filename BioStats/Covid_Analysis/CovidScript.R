#' BioStats Covid Analysis Group Assignment

#required packages
library(tidyverse)
library(grid)
library(gridExtra)
library(nlme)
library(plotrix)
library(betareg)

#Creating the Data
#NOTE: Even though we were told that the CU data was baked into th County data, the numbers we have do not reflect this: when the positives from boulder were subtracted from the positives from the county, several days resulted in negative numbers. For the rest of this analysis, we will use the numbers as they are below, and acknowledge that the two are not completely independent of one another!

countyTests <- c(80,450,440,360,445,350,180,184,515,508,480,470,520,253,160,165,605,545,525)
cuTests <- c(NA,23,79,73,52,80,NA,NA,32,30,107,68,226,NA,NA,NA,171,178,139)
countyPostive <- c(2,5,5,5,20,13,3,3,13,12,8,14,5,4,5,9,17,13,10)
cuPositive <- c(NA, 3,4,1,2,3,NA,NA,2,1,21,17,49,NA,NA,NA,30,41,45)
dateRange <- c("8/23", "8/24","8/25","8/26", "8/27", "8/28", "8/29", "8/30", "8/31", "9/1", "9/2", "9/3", "9/4", "9/5", "9/5", "9/7", "9/8", "9/9", "9/10")


#Creating an empty data frame
dat <- data.frame(Source = rep(NA, 19*2),
                     Tests_Administered = rep(NA, 19*2),
                     Positive_Tests = rep(NA, 19*2),
                     Positivity_Rate = rep(NA, 19*2),
                     Date = rep(NA, 19*2))

#populating the data frame with data
dat[1:19, 1] <- rep("CU", 19)
dat[20:38, 1] <- rep("County", 19)
dat[1:19, 2] <- cuTests
dat[20:38, 2] <- countyTests
dat[1:19, 3] <- cuPositive
dat[20:38, 3] <- countyPostive
dat[1:19, 4] <- cuPositive/cuTests
dat[20:38, 4] <- countyPostive/countyTests
dat[1:38, 5] <- rep(dateRange, 2)

dat$Date <- as.Date(dat$Date,format = "%m/%d" )

  

str(dat)

#Preliminary Visualization

#Testing by Date
tests <- ggplot(dat,aes(x = Date,y = Tests_Administered)) + 
  geom_bar(aes(fill = Source),stat = "identity",position = "dodge") + theme_minimal()


#Positive cases by date
positives <- ggplot(dat,aes(x = Date,y = Positive_Tests)) + 
  geom_bar(aes(fill = Source),stat = "identity",position = "dodge") + theme_minimal()


#Positivity by Date
rate <- ggplot(dat,aes(x = Date,y = Positivity_Rate)) + 
  geom_bar(aes(fill = Source),stat = "identity",position = "dodge") + theme_minimal()

#Aggregated Plot
caseDataMultiplot <- grid.arrange(tests, positives, rate)



#Hypothesis Testing

#filtering the data by source
countyDat <- dat %>%
  filter(Source == "County")

cuDat <- dat %>%
  filter(Source == "CU")

#Testing

#Our alpha: null is defined as the positivity rate of the county
meanCountyRate <- mean(countyDat$Positivity_Rate)

hypothesisTest <- binom.test(sum(cuDat$Positive_Tests, na.rm = T), sum(cuDat$Tests_Administered, na.rm = T), meanCountyRate, alternative = "two.sided") 

hypothesisTest #does not fall within the predictions of the null (0.024 positivity) (p<.001); positivity rate at CU is 17.4% and ~ 6 times greater than that of the county

#Modeling: linear model using date as a temporal continuous co-variate. This will give us a better understanding of the relationship between positivity and time; i.e., positivity recorded on day x may be more related to the positivity recorded on day x-1 than x-2

modelDat <- dat %>%
  filter(!is.na(Positivity_Rate))

#fitting beta regression to account for response variable being a fraction
positivityModel <- betareg(Positivity_Rate ~ Source*Date, data = modelDat)

#diagnostics:
plot(positivityModel)

summary(positivityModel)

#' _Notes on the model above_
#' The results from this model indicate that, when accounting for the effect of time on positivity (based on the assumption that positivity changes with time), Cu's postivity rate is heading in the opposite direction (while the county's is decreasing with time, CU's is increasing). 

#' _Plotting_

#'date frame for important statistics (mean, se, categories)

statsDF <- data.frame(mean = c(mean(countyDat$Positivity_Rate), mean(cuDat$Positivity_Rate, na.rm = T)),
                      error = c(std.error(countyDat$Positivity_Rate), std.error(cuDat$Positivity_Rate, na.rm = T)),
                      treatment = c("County", "CU"))

#Observations Plot:

obsvPlot <- ggplot(statsDF, aes(treatment, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .3) +
  geom_jitter(data = modelDat,aes(Source, Positivity_Rate, color = Source), width = .05,alpha = .4)+
  labs(x = "Data Source", 
       y = "Positivity Rate",
       title = "Positivity Rate by Source") +
  annotate("text", 1, .08, label = "a") +
  theme_minimal()
obsvPlot

#Beta regression plot: taking into account date!
betaPlot <- ggplot(modelDat, aes(Date, Positivity_Rate)) +
  geom_point(aes(color = Source)) + 
  geom_smooth(aes(y = predict(positivityModel, modelDat)), 
              method = "loess", 
              color = "darkslategrey",
              linetype = "dashed") +
  labs(y = "Positivity Rate",
       title = "Beta Regression",
       subtitle = "Date as continuous covariate") +
  theme_minimal()

betaPlot

finalMultiplot <- grid.arrange(obsvPlot, betaPlot)
  


#'Closing Remarks: first thing that is evident is that the County sampled a lot more, so their numbers are going to be more accurate. Secondly, there is much more variation in the CU dataset (SD of ~.1 compared to the county's SD of ~.01); standard error for CU was greater than the County's, too (.03 vs .003, respectively). As well, modeling indicates that CU's sampling/testing methodology is related an effect size of + 11 percentage points (approximated) when compared to the positivity rate of the county; whether this is due to their sampling criteria, the behavior of the students, or some other confounding variable cannot be determined. The effect of sampling date on the SD of both the County and CU was negligible, however the number of tests administered accounted for roughly 1.2% of the variation seen in positivity rate. 





