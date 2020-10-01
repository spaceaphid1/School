#'Jackson's BioStats Final Project
#'Packages

library(tidyverse)
library(readxl)
library(nlme)
library(lme4)

#'Data Inspection

richnessDat <- read_excel("~/Repos/School/BioStats/FinalProject/finalProjData.xlsx", 
                            sheet = "Species Richness")
head(richnessDat)
str(richnessDat)

#'Reformatting the data

richnessDat$FertilizerTreatment <- as.factor(richnessDat$FertilizerTreatment)
richnessDat$Community <- as.factor(richnessDat$Community)

#'Fert Treatment on total plant Species in given plot
ggplot(richnessDat, aes(FertilizerTreatment, TotalSpp#)) +
  geom_boxplot()

#'Fert Treatment on total non-clonal plant Species in given plot

ggplot(richnessDat, aes(`Fertilizer Treatment`, `NC Spp #`)) +
  geom_boxplot()

#'Fert Treatment on total clonal plant Species in given plot

ggplot(richnessDat, aes(`Fertilizer Treatment`, `Clon Spp #`)) +
  geom_boxplot()


