#' Covid Analysis

library(tidyverse)

Date_Tested <- c("8/23", "8/26", "8/29", "9/1", "9/4", "9/7", "9/10")
Pos_Propn <- c(5/540, 10/360, 7/180, 20/510, 10/525, 15/175, 15/535 )

countyDat <- data.frame(cbind(Date_Tested, Pos_Propn))

meanPositivity <- mean(Pos_Propn)

countyDat$Rounded_Positivity <- round(Pos_Propn, digits = 3)


ggplot(countyDat, aes(Date_Tested, Rounded_Positivity)) +
  geom_bar(stat = "identity")+
  ggtitle("Rounded Positivity Rate by Date") +
  xlab("Date Tested") +
  ylab("Positivity Rate (rounded)") +
  labs(subtitle = "Boulder County", caption = "includes CU Boulder Case Data")

