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

countyTests <- c(80,450,440,360,445,350,180,184,515,508,480,470,520,253,160,165,605,545,525,700,260,205,630,NA,NA,NA)
cuTests <- c(NA,23,79,73,52,80,NA,NA,32,30,107,68,226,NA,NA,NA,171,178,139,318,NA,NA,240,218,297,571)
countyPostive <- c(2,5,5,5,20,13,3,3,13,12,8,14,5,4,5,9,17,13,10,20,15,5,20,NA,NA,NA)
cuPositive <- c(NA, 3,4,1,2,3,NA,NA,2,1,21,17,49,NA,NA,NA,30,41,45,89,NA,NA,77,56,100,130)
dateRange <- c("8/23", "8/24","8/25","8/26", "8/27", "8/28", "8/29", "8/30", "8/31", "9/1", "9/2", "9/3", "9/4", "9/5", "9/5", "9/7", "9/8", "9/9", "9/10", "9/11", "9/12","9/13","9/14","9/15","9/16","9/17")


#Creating an empty data frame
dat <- data.frame(Source = rep(NA, 26*2),
                     Tests_Administered = rep(NA, 26*2),
                     Positive_Tests = rep(NA, 26*2),
                     Positivity_Rate = rep(NA, 26*2),
                     Date = rep(NA, 26*2))

#populating the data frame with data
dat[1:26, 1] <- rep("CU", 26)
dat[27:52, 1] <- rep("County", 26)
dat[1:26, 2] <- cuTests
dat[27:52, 2] <- countyTests
dat[1:26, 3] <- cuPositive
dat[27:52, 3] <- countyPostive
dat[1:26, 4] <- cuPositive/cuTests
dat[27:52, 4] <- countyPostive/countyTests
dat[1:52, 5] <- rep(dateRange, 2)

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
meanCountyRate <- mean(countyDat$Positivity_Rate, na.rm = T)

hypothesisTest <- binom.test(sum(cuDat$Positive_Tests, na.rm = T), sum(cuDat$Tests_Administered, na.rm = T), meanCountyRate, alternative = "two.sided") 

hypothesisTest #does not fall within the predictions of the null (0.024 positivity) (p<.001); positivity rate at CU is 23.1% and ~ 6.67 times greater than that of the county

#' _Modeling:_ 
#'
#'linear model using date as a temporal continuous co-variate. This will give us a better understanding of the relationship between positivity and time; i.e., positivity recorded on day x may be more related to the positivity recorded on day x-1 than x-2

modelDat <- dat %>%
  filter(!is.na(Positivity_Rate))

#fitting beta regression to account for response variable being a fraction
positivityModel <- betareg(Positivity_Rate ~ Source*Date, data = modelDat)
testingModelCounty <- lm(Positivity_Rate ~ Tests_Administered*Date, data = countyDat) 
summary(testingModelCounty)#suggest that, for the county and for every unit increase in Date, there is no effect of tests administered on change in positivity rate 

testingModelCU <- lm(Positivity_Rate ~ Tests_Administered*Date, data = cuDat) 
summary(testingModelCU)

#diagnostics:
plot(positivityModel)

summary(positivityModel)

cuOddsChange <- exp(7.240544e-02)
cuOddsChange# the estimated odds that, as date's increase, postivity rate will grow at CU; for every day, the estimated positivity will increase by ~8% give the current trend and  all other treatments being held constant
countOddsChange <- exp(2.176491e-02)
countOddsChange# the estimated odds that, as date increases, positivity rate will grow within the county; for every day, the estimated positivity will increase by ~2% given the current trend and all other treatments being held constant.

#' _Notes on the model above_
#' The results from this model indicate that, when accounting for the effect of time on positivity (based on the assumption that positivity changes with time), Cu's postivity rate is heading in the opposite direction (while the county's is decreasing with time, CU's is increasing). 

#' _Plotting_

#'date frame for important statistics (mean, se, categories)

statsDF <- data.frame(mean = c(mean(countyDat$Positivity_Rate, na.rm = T), mean(cuDat$Positivity_Rate, na.rm = T)),
                      error = c(std.error(countyDat$Positivity_Rate), std.error(cuDat$Positivity_Rate, na.rm = T)),
                      treatment = c("County", "CU"))

#Observations Plot:

obsvPlot <- ggplot(statsDF, aes(treatment, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .3) +
  geom_jitter(data = modelDat,aes(Source, Positivity_Rate, color = Source), width = .05,alpha = .4)+
  labs(x = "Data Source", 
       y = "Positivity Rate",
       title = "Positivity Rate by Source",
       caption = "Data collected from 8/23 to 9/17") +
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
       subtitle = "Regression modeling CU's Positivity Rate Change with Increase in Time",
       caption = "Data collected from 8/23 to 9/17") +
  theme_minimal()

betaPlot


#'Testing Plot
#'

testingPlot <- ggplot(modelDat, aes(Tests_Administered, Positivity_Rate)) +
  geom_point(aes(color = Source)) + 
  geom_smooth(method = "lm", aes(color = Source)) +
  labs(y = "Positivity Rate",
       title = "Beta Regression",
       subtitle = "Regression modeling CU's Positivity Rate Change with Increase in Time",
       caption = "Data collected from 8/23 to 9/17") +
  theme_minimal()

testingPlot

finalMultiplot <- grid.arrange(obsvPlot, betaPlot)
  


#'Closing Remarks: first thing that is evident is that the County sampled a lot more, so their numbers are going to be more accurate. Secondly, there is much more variation in the CU dataset (SD of ~.1 compared to the county's SD of ~.01); standard error for CU was greater than the County's, too (.03 vs .003, respectively). As well, modeling indicates that CU's sampling/testing methodology is related an effect size of + 11 percentage points (approximated) when compared to the positivity rate of the county; whether this is due to their sampling criteria, the behavior of the students, or some other confounding variable cannot be determined. The effect of sampling date on the SD of both the County and CU was negligible, however the number of tests administered accounted for roughly 1.2% of the variation seen in positivity rate. 





