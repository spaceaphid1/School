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
dat$Source <- as.factor(dat$Source)
  

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

hypothesisTest #does not fall within the predictions of the null (0.024 positivity) (p<.001); positivity rate at CU is 23.1% and ~ 6.67 times greater than that of the county given this model

#' _Modeling:_ 
#'
#'linear model using date as a temporal continuous co-variate. This will give us a better understanding of the relationship between positivity and time; i.e., positivity recorded on day x may be more related to the positivity recorded on day x-1 than x-2

#' Filtering the Data
modelDat <- dat %>%
  filter(!is.na(Positivity_Rate))

cuModelDat <- modelDat %>%
  filter(Source == "CU")

countyModelDat <- modelDat %>% 
  filter(Source == "County")

#'Seeing how the number of positive tests is related to the number of tests administered by source

#'CU
cuTesting_reg <- lm(Positive_Tests ~ Tests_Administered, data = cuDat)
summary(cuTesting_reg) #significant effect of tests administered on number of positives; positively correlated

cuTesting_plot <- ggplot(cuModelDat, aes(Tests_Administered, Positive_Tests)) +
  geom_point(aes(color = Date)) +
  geom_smooth(aes(y = predict(cuTesting_reg, cuModelDat))) +
  theme_minimal() +
  labs(title = "CU: Tests Administered vs. Positive Tests") +
  annotate("text", 100, 125, label = "p< .001, 
           Adjusted R sq. = 0.91")

cuTesting_plot
ggsave("cuTesting_plot.png")

#'County
countyTesting_reg <-lm(Positive_Tests ~ Tests_Administered, data = countyDat)
summary(countyTesting_reg)#significant effect of tests administered on number of positives; positively correlated

countyTesting_plot <- ggplot(countyModelDat, aes(Tests_Administered, Positive_Tests)) +
  geom_point(aes(color = Date)) +
  geom_smooth(aes(y = predict(countyTesting_reg, countyModelDat))) +
  theme_minimal() +
  labs(title = ) + 
  labs(title = "County: Tests Administered vs. Positive Tests") +
  annotate("text", 200, 17, label = "p< .001,
           Adjusted R sq. = 0.44")

countyTesting_plot
ggsave("countyTesting_plot.png")

#'seeing how the rate of positivity is affected by tests administered by source

#'County

countyPositivity_beta <- betareg(Positivity_Rate ~ Tests_Administered*Date, data = countyDat)
summary(countyPositivity_beta) #suggests there is no sig effect of tests administered on positivity rate for county when controlling for date and vice-versa

#'Cu
cuPositivity_beta <- betareg(Positivity_Rate ~ Tests_Administered*Date, data = cuDat)
summary(cuPositivity_beta) #Suggests there is a stastically significant effect of Tests administered when date is controlled for (p<.05, effect size 1% increase). As well, an increase in date (day x to x+1) is related to a 14% increase in positivity rate. These statistics are telling two stories. 1) The postivity rate is significantly effected by the number of tests administered on a given day, and this effect is positive; this suggests that CU's sampling criteria is affecting their results (i.e., the more they sample, the more their positivity rate grows). As to what this means, I'm not entirely sure, but it could have something to do with the fact that an increase in positivity has a strong, postivite correlation to an increase in date. 

testingPosit_plot <- ggplot(modelDat, aes(Date, Positivity_Rate)) +
  geom_point(aes(col = Source, size = Tests_Administered)) +
  geom_smooth(aes(col = Source)) +
  labs(caption = "County Reg p>.05; Cu Reg p<.05 ") +
  theme_minimal() +
  labs(title = "Positivity Rate vs. Tests Administered~Date interaction Effect")

testingPosit_plot
ggsave("testingPosit_plot.png")

#'Seeing how the rate of positivity varies by source (using beta regression to account for proportional data) and date as a continuous covariate

positivityModel <- betareg(Positivity_Rate ~ Source*Date, data = modelDat)
summary(positivityModel) #suggests that, when date is controlled for, CU's positivity rate is ~8% greater than that of the County's (p<.01)

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
ggsave("obsvPlot.png")






