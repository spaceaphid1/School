#'Jackson's BioStats Final Project
#'Packages

library(tidyverse)
library(readxl)
library(nlme)
library(lme4)
library(lattice)

#'Creating quick diagnostics function

qqplotFunc <- function(x) {
  qqnorm(x, ~ resid(., type = "p"), abline = c(0, 1))
}
  

#'Data Inspection

richnessDat <- read_excel("~/Repos/School/BioStats/FinalProject/finalProjData.xlsx", 
                            sheet = "Species Richness")
head(richnessDat)
str(richnessDat)

#'Reformatting the data
richnessDat$FertilizerTreatment <- as.factor(richnessDat$FertilizerTreatment)
richnessDat$Community <- as.factor(richnessDat$Community)

#'Label Ordering for proper trend vis
labelOrders <- c("CO", "UN", "SP", "LP")

#' _Hypothesis 1_

hyp1Dat <- richnessDat %>%
  filter(Community == "non-clonal only")

#' Preliminary Vis
#'Fert Treatment on total plant Species in given plot
ggplot(hyp1Dat, aes(x=factor(FertilizerTreatment, levels = labelOrders), TotalSppNum)) +
  geom_boxplot()

lme_hyp1 <- lme(TotalSppNum~FertilizerTreatment,
                         data = hyp1Dat,
                         random = ~1|Block)
anova(lme_hyp1)#sig overall treatment effect

plot.lme(lme_hyp1)#Doesnt appear to be much heteroskedasticity in residual variance; no need for model that accounts for unequal resid. variance

qqnorm(lme_hyp1, ~ resid(., type = "p"), abline = c(0, 1))#looks good

summary(lme_hyp1) #it would appear that both the uniformly the uniform spread and small patch fertilizer treatment spread were significantly different from the control (effect size ~ -3.0 and - 3.3 spp difference, respectively, both p < 0.001). The large patch fertilizer spread did not differ signficantly from the control (p> 0.01)

summary_hyp1 <- summary(lme_hyp1)# confident in using this model for comparisons against the control

#'Pairwise Comparisons: Small Patch and Uniform Fertilizer Distributions

hyp1_unBase <- hyp1Dat %>% mutate(FertilizerTreatment = relevel(FertilizerTreatment, "UN"))

lme_hyp1_unBase <- lme(TotalSppNum~FertilizerTreatment,
                       data = hyp1_unBase,
                       random = ~1|Block)

summary(lme_hyp1_unBase)

#' Same model with no intercept for plotting
lme_hyp1_noint <- lme(TotalSppNum~ 0 +FertilizerTreatment,
                      data = hyp1Dat,
                      random = ~1|Block)

summary_hyp1_noint <- summary(lme_hyp1_noint)#No sig diff between response for UN and SP

#'DF for plotting important values
hyp1_summary_df <- data.frame(trt_mean_noint = summary_hyp1_noint$tTable[,"Value"],
                                   trt_se_noint = summary_hyp1_noint$tTable[,"Std.Error"],
                                   trt_cat = levels(hyp1Dat$FertilizerTreatment))

ggplot(hyp1_summary_df, aes(x = factor(trt_cat, levels = labelOrders), trt_mean_noint)) +
  geom_point() + 
  geom_errorbar(aes(ymin=trt_mean_noint-trt_se_noint, ymax=trt_mean_noint+trt_se_noint), width = 0.5) +
  geom_jitter(data = hyp1Dat, aes(FertilizerTreatment, TotalSppNum), width = .05, alpha = .4) +
  annotate("text", x = 1, y = 37, color = "red", label = "a") +
  annotate("text", x = 4, y = 37, color = "red", label = "a") +
  annotate("text", x = 2, y = 33, color = "blue", label = "b") +
  annotate("text", x = 3, y = 36, color = "blue", label = "b") +
  labs(x = "Fertilizer Treatment",
       y = "Total Species Count") +
  ggtitle("Non-Clonal Treatment", subtitle = "Species Richness by Fertilizer Treatment") +
  theme_minimal()

#' _Hypothesis 2_
#' 

hyp2Dat <- richnessDat %>%
  filter(Community == "mixed")

#'Preliminary Vis

ggplot(hyp2Dat, aes(FertilizerTreatment, TotalSppNum)) +
  geom_boxplot()

#'Modeling
lme_hyp2 <- lme(TotalSppNum~FertilizerTreatment,
                data = hyp2Dat,
                random = ~1|Block)

anova(lme_hyp2)#sig trt effect

plot.lme(lme_hyp2)#Seems good

qqnorm(lme_hyp2, ~ resid(., type = "p"), abline = c(0, 1))#also looks good

summary(lme_hyp2) #similar trend as seen in the non-clonal only treatment; all treatments are sig diff from the control, however. As well, the rates of declination seem like they might be steeper (based on effect size)

summary_hyp2 <- summary(lme_hyp2)

#'Pairwise Comparisons: 

#'UN as base
hyp2_unBase <- hyp2Dat %>% mutate(FertilizerTreatment = relevel(FertilizerTreatment, "UN"))

lme_hyp2_unBase <- lme(TotalSppNum~FertilizerTreatment,
                       data = hyp1_unBase,
                       random = ~1|Block)

summary(lme_hyp2_unBase)#UN not sig diff from SP (p = .76); all else sig (p<.001)

#'SP as base
hyp2_spBase <- hyp2Dat %>% mutate(FertilizerTreatment = relevel(FertilizerTreatment, "SP"))

lme_hyp2_spBase <- lme(TotalSppNum~FertilizerTreatment,
                       data = hyp2_spBase,
                       random = ~1|Block)

#'LP as base
hyp2_lpBase <- hyp2Dat %>% mutate(FertilizerTreatment = relevel(FertilizerTreatment, "LP"))

lme_hyp2_lpBase <- lme(TotalSppNum~FertilizerTreatment,
                       data = hyp2_lpBase,
                       random = ~1|Block)

summary(lme_hyp2_lpBase)#All Sig diff (p<.05)

#' Same model with no intercept for plotting
lme_hyp2_noint <- lme(TotalSppNum ~ 0 +FertilizerTreatment,
                      data = hyp2Dat,
                      random = ~1|Block)

summary_hyp2_noint <- summary(lme_hyp2_noint)#No sig diff between response for UN and SP

#'DF for plotting important values
hyp2_summary_df <- data.frame(trt_mean_noint = summary_hyp2_noint$tTable[,"Value"],
                              trt_se_noint = summary_hyp2_noint$tTable[,"Std.Error"],
                              trt_cat = levels(hyp1Dat$FertilizerTreatment))

ggplot(hyp2_summary_df, aes(x = factor(trt_cat, levels = labelOrders), trt_mean_noint)) +
  geom_point() + 
  geom_errorbar(aes(ymin=trt_mean_noint-trt_se_noint, ymax=trt_mean_noint+trt_se_noint), width = 0.5) +
  geom_jitter(data = hyp2Dat, aes(FertilizerTreatment, TotalSppNum), width = .05, alpha = .4) +
  # annotate("text", x = 1, y = 37, color = "red", label = "a") +
  annotate("text", x = 2, y = 33, color = "blue", label = "a") +
  annotate("text", x = 3, y = 33, color = "blue", label = "a") +
  # annotate("text", x = 4, y = 37, color = "red", label = "a") +
  labs(x = "Fertilizer Treatment",
       y = "Total Species Count") +
  ggtitle("Mixed Treatment", subtitle = "Species Richness by Fertilizer Treatment") +
  theme_minimal()

#' _Notes on the above_
#' The above models and graphs are checking for a significant treatment effect within each fertilizer community type; below, we will examine how total species counts differs within each fertilizer treatment type by community
#' 
#' _Hypothesis 1: Community_

#'Control
hyp1_CO <- richnessDat %>%
  filter(FertilizerTreatment == "CO")

#'Prelim Vis

ggplot(hyp1_CO, aes(Community, TotalSppNum)) +
  geom_boxplot()

#'Modeling

hyp1_CO_lme <- lme(TotalSppNum~Community,
                   data = hyp1_CO,
                   random = ~1|Block)

plot.lme(hyp1_CO_lme)#Looks good

qqplotFunc(hyp1_CO_lme)#looks Good

summary(hyp1_CO_lme)#controls are significantly different (p<.01); Clonal only has less species in it than the mixed

#'Uniform
hyp1_UN <- richnessDat %>%
  filter(FertilizerTreatment == "UN")

#'Prelim Vis

ggplot(hyp1_UN, aes(Community, TotalSppNum)) +
  geom_boxplot()

#'Modeling

hyp1_UN_lme <- lme(TotalSppNum~Community,
                   data = hyp1_UN,
                   random = ~1|Block)

plot.lme(hyp1_UN_lme)#Looks good

qqplotFunc(hyp1_UN_lme)#looks Good

summary(hyp1_UN_lme)#Uniform treatments not significantly different from one another (p = .07)

#'Small Patch
hyp1_SP <- richnessDat %>%
  filter(FertilizerTreatment == "SP")

#'Prelim Vis

ggplot(hyp1_SP, aes(Community, TotalSppNum)) +
  geom_boxplot()

#'Modeling

hyp1_SP_lme <- lme(TotalSppNum~Community,
                   data = hyp1_SP,
                   random = ~1|Block)

plot.lme(hyp1_UN_lme)#Looks good

qqplotFunc(hyp1_UN_lme)#looks Good

summary(hyp1_UN_lme)#Small Patch treatments not significantly different from one another (p = .95)

#'Large Patch
hyp1_LP <- richnessDat %>%
  filter(FertilizerTreatment == "LP")

#'Prelim Vis

ggplot(hyp1_LP, aes(Community, TotalSppNum)) +
  geom_boxplot()

#'Modeling

hyp1_LP_lme <- lme(TotalSppNum~Community,
                   data = hyp1_LP,
                   random = ~1|Block)

plot.lme(hyp1_LP_lme)#Looks good

qqplotFunc(hyp1_LP_lme)#looks Good

summary(hyp1_LP_lme)#Large Patch treatments not significantly different from one another (p = .36)

#Plotting: Need to create a besides histogram with each fertilizer treatment as x and spp count as y; besides will be community type. Then, need to aggregate all the mean values for each fertilizer treatment's effect on spp and plot a regression showing difference in declination by community type
