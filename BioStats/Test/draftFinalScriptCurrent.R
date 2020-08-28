#' ## Evolutionary Ecology Experimental Class Data Analysis
#' 
#' Courtney Van Den Elzen, Jackson Anderson, and Nancy Emery
#' 
#' April 2020

####  Packages ####
#+ message = FALSE, warning = FALSE
library(tidyverse)
library(nlme)
library(lattice) # contains function 'qqmath' for making qqplot for lmer model
library(lmtest)
library(glmmTMB)
library(sjPlot)
library(sjmisc)
library(knitr)
library("grid")
library("gridExtra")
library(wesanderson)

#### Full Data: Loading and Cleaning ####
dat <- read_csv("/Users/jacksonanderson/Desktop/DispersalPlasticity/Dispersal_Git/Las-dispersal-plasticity/DispersalMasterData.csv")

#' #### Data Cleaning
#' 
#' Mean disk seed count
dat$meanDiskCount <-(dat$diskSeedCount1 + dat$diskSeedCount2)/2
#' Ratio of ray seeds to disk seeds
dat$phyllToTotal <- round(dat$phyllaryCount / (dat$phyllaryCount + dat$meanDiskCount), 3)

#Setting Label Orders for all experiments
labelOrders <- c("Control", "Structure", "Low", "Medium", "High")


#### Data Cleaning: Shade Experiment ####
#'

#' #### Data
shadeDat <- dat %>% 
  filter( experiment == "shade") %>%
  filter( !is.na(phyllToTotal)) %>%
  mutate_at(vars(treatmentCat), factor)


#' Replicates per treatment
shade_reps <- summarize(group_by(shadeDat, treatmentCat), n())
shade_reps

#' Boxplot of the phyllary proportion by treatment
ggplot(shadeDat, aes(treatmentCat, phyllToTotal)) +
  geom_boxplot()

#' Boxplot of the floral height by treatment
ggplot(shadeDat, aes(treatmentCat, focalFlowerHeight_cm)) +
  geom_boxplot()

#' Boxplot of the plant mass by treatment
ggplot(shadeDat, aes(treatmentCat, focalPlantMass_g)) +
  geom_boxplot()

#### Data Cleaning: Density Experiment ####
#' 
#' #### Data 
densityDat <- dat %>% 
  filter( experiment == "density") %>%
  filter( !is.na(phyllToTotal)) %>%
  mutate_at(vars(treatmentCat), factor)

#' Replicates per treatment
density_reps <- summarize(group_by(densityDat, treatmentCat), n())
density_reps

#'Neighbor Plant Biomass Means

#' NOTE: Unclear if I should use the treatment category of the non focal plant count. Values differ when using treatment cat as filter argument (0.135g and 0.113 for low and high, respectively) vs non focal plant count (0.0852g and 0.121g for 1 and 2 non focal plant counts, respecitvely). 

#' One Non-Focal Plant mass mean
massOneNFPlant <- densityDat %>% 
  filter(densityNonFocalPlantCount == 1 ) %>%
  filter(!is.na(densityNonFocalPlantMass_g)) %>%
  summarise(mean(densityNonFocalPlantMass_g))
massOneNFPlant

#' Two Non-Focal Plants mass mean
massTwoNFPlant <- densityDat %>% 
  filter(densityNonFocalPlantCount == 2 ) %>%
  filter(!is.na(densityNonFocalPlantMass_g)) %>%
  summarise(mean(densityNonFocalPlantMass_g))
massTwoNFPlant

#' Boxplot of the phyllary proportion by treatment
ggplot(densityDat, aes(treatmentCat, phyllToTotal)) +
  geom_boxplot()

#' Boxplot of the floral height by treatment
ggplot(densityDat, aes(treatmentCat, focalFlowerHeight_cm)) +
  geom_boxplot()

#' Boxplot of the plant mass by treatment
ggplot(densityDat, aes(treatmentCat, focalPlantMass_g)) +
  geom_boxplot()

#### Data Cleaning: Resource Experiment ####

#' #### Data
#' 
#' The "high" treatment was removed due to low replication (3 data points)
resourceDat <- dat %>% 
  filter(experiment == "resources" & !is.na(treatmentCat)) %>% 
  mutate_at(vars(treatmentCat), factor) %>%
  filter( survivorship == 1)

#' Replicates per treatment
resource_reps <- summarize(group_by(resourceDat, treatmentCat), n())
resource_reps

#' The "high" treatment was removed due to low replication (3 data points)
resourceDat <- resourceDat %>% 
  filter( treatmentCat != "High" ) %>%
  droplevels()

#' Boxplot of the phyllary proportion by treatment
ggplot(resourceDat, aes(treatmentCat, phyllToTotal)) +
  geom_boxplot()

#' Boxplot of the floral height by treatment
ggplot(resourceDat, aes(treatmentCat, focalFlowerHeight_cm)) +
  geom_boxplot()

#' Boxplot of the plant mass by treatment
ggplot(resourceDat, aes(treatmentCat, focalPlantMass_g)) +
  geom_boxplot()



#### Plant Mass: Shade to Control####
#' 
#' Get rid of NA's
shadeDat_mass <- shadeDat %>% filter( !is.na(focalPlantMass_g))

#' __Linear mixed model__
lme_mass_shade <- lme(focalPlantMass_g~treatmentCat, 
                      random = ~1|bin, 
                      data = shadeDat_mass)
summary(lme_mass_shade)
<<<<<<< Updated upstream
=======
anova(lme_mass_shade)
anova(lme_mass_shade, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_mass_shade, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_mass_shade)

lme_mass_shade$tTable

#' Though the residual variances are somewhat different per fitted value, they are certainly more evenly distrubuted than what we have seen with other response variables. Even so, we fit an unequal variances model and then compared it to the equal variances model.
#' 
#' __Linear mixed model with unequal variances__
lme_mass_shade_uv <- lme(focalPlantMass_g~treatmentCat,
                         data = shadeDat_mass,
                         random = ~1|bin,
                         weights = varIdent(form = ~1|treatmentCat))

summary(lme_mass_shade_uv)
<<<<<<< Updated upstream
=======
anova(lme_mass_shade_uv)
anova(lme_mass_shade_uv, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_mass_shade_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_mass_shade_uv)

#' The uneuqual variances model to seem to homogenize residual variances, but is it worth it to use this model over the equal variances model at the cost of less degrees of freedom?
anova(lme_mass_shade, lme_mass_shade_uv) # No

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_mass_shade_noint <- lme(focalPlantMass_g~ 0 +treatmentCat,
                            data = shadeDat_mass,
                            random = ~1|bin)

#' _Final answer_
summary_lme_mass_shade <- summary(lme_mass_shade)
summary_lme_mass_shade_noint <- summary(lme_mass_shade_noint)


#' _Anova Shade Mass_

anova(summary_lme_mass_shade) #no trt effect

#' Plot the model outcome (work in progress)
shade_mass_output_df <- data.frame(cat_mean_noint = summary_lme_mass_shade_noint$tTable[,"Value"],
                                   cat_se_noint = summary_lme_mass_shade_noint$tTable[,"Std.Error"],
                                   trt_cat = levels(shadeDat$treatmentCat))

shade_mass_output_df

p_shade_mass_output <- ggplot(shade_mass_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_errorbar(aes(ymin=cat_mean_noint-cat_se_noint, ymax=cat_mean_noint+cat_se_noint), color = "brown4", width = 0.5) +
  geom_point() +
  geom_jitter(data = shadeDat, aes(x = treatmentCat, y = focalPlantMass_g), color = "brown4", width = 0.1, alpha = 0.4) +
  theme_light() +
  ylab("Focal Plant Mass (g)") +
  xlab("Shade Treatment (µmol of photons/m^2s)") +
  ggtitle("Shade Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n122.0", "Structure\n107.0", "Low\n87.2", "Medium\n71.6", "High\n69.0")) +
  ylim(0,.5)
p_shade_mass_output
  # annotate("text", x= 1, y = -.01, label = "Control ", size = 3.3) +
  # annotate("text", x= 2, y = -.01, label = "Structure ", size = 3.3) +
  # annotate("text", x= 3, y = -.01, label = "Low ", size = 3.3) +
  # annotate("text", x= 4, y = -.01, label = "Medium ", size = 3.3) +
  # annotate("text", x= 5, y = -.01, label = "High ", size = 3.3) +

  # labs(
  #   # caption = "Increasing shade level with treatment effect",
  #   title = "Shade Experiment",
  # ) +
  



shadeDat %>%
  filter(shadeDat$treatmentCat == "High")

#### Plant Mass: Shade Pairwise Comparisons #### 
#' _Shade Plant Mass All bases_ 
#' Structure to all
shadeDat_mass_structbase <- shadeDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "Structure"))

lme_mass_shade_structbase <- lme(focalPlantMass_g~treatmentCat, 
                                 random = ~1|bin, 
                                 data = shadeDat_mass_structbase)

summary(lme_mass_shade_structbase)
#' 
#' Low to all
shadeDat_mass_lowbase <- shadeDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_mass_shade_lowbase <- lme(focalPlantMass_g~treatmentCat, 
                              random = ~1|bin, 
                              data = shadeDat_mass_lowbase)

summary(lme_mass_shade_lowbase)
#' 
#' Medium to all
shadeDat_mass_medbase <- shadeDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "Medium"))

lme_mass_shade_medbase <- lme(focalPlantMass_g~treatmentCat, 
                              random = ~1|bin, 
                              data = shadeDat_mass_medbase)

summary(lme_mass_shade_medbase)

#' High To All

shadeDat_mass_highbase <- shadeDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "High"))

lme_mass_shade_highbase <- lme(focalPlantMass_g~treatmentCat, 
                              random = ~1|bin, 
                              data = shadeDat_mass_highbase)

summary(lme_mass_shade_highbase)

#### Plant Mass: Density to Control ####
#' 
#' Get rid of NA's
densityDat_mass <- densityDat %>% filter( !is.na(focalPlantMass_g))

#' __Linear mixed model__
lme_mass_density <- lme(focalPlantMass_g~treatmentCat, 
                        random = ~1|bin, 
                        data = densityDat_mass)
summary(lme_mass_density)
<<<<<<< Updated upstream
=======
anova(lme_mass_density)
anova(lme_mass_density, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_mass_density, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_mass_density)
lme_mass_density$tTable

#' Though the residual variances are somewhat different per fitted value, they are certainly more evenly distrubuted than what we have seen with other response variables. Even so, we fit an unequal variances model and then compared it to the equal variances model.
#' 
#' __Linear mixed model with unequal variances__
lme_mass_density_uv <- lme(focalPlantMass_g~treatmentCat,
                           data = densityDat_mass,
                           random = ~1|bin,
                           weights = varIdent(form = ~1|treatmentCat))

summary(lme_mass_density_uv)
<<<<<<< Updated upstream
=======
anova(lme_mass_density_uv)
anova(lme_mass_density_uv, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_mass_density_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_mass_density_uv)

#' The variance structure by fitted value seems to be somewhat more normal using the unequal variances model, though it looks like the gains are marginal, at best. Is it worth it to use the unequal variances model at the cost of degrees of freedom?
anova(lme_mass_density, lme_mass_density_uv) # No

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_mass_density_noint <- lme(focalPlantMass_g~ 0 +treatmentCat,
                              data = densityDat_mass,
                              random = ~1|bin)

summary(lme_mass_density_noint)

#' _Final answer_
summary_lme_mass_density <- summary(lme_mass_density)
summary_lme_mass_density_noint <- summary(lme_mass_density_noint)

#' _Anova Density Mass_

anova(summary_lme_mass_density) #no trt effect

#' Plot the model outcome (work in progress)
density_mass_output_df <- data.frame(cat_mean_noint = summary_lme_mass_density_noint$tTable[,"Value"],
                                     cat_se_noint = summary_lme_mass_density_noint$tTable[,"Std.Error"],
                                     trt_cat = levels(densityDat$treatmentCat))

density_mass_output_df

p_density_mass_output <- ggplot(density_mass_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_point() +
  geom_errorbar(aes(ymin=cat_mean_noint-cat_se_noint, ymax=cat_mean_noint+cat_se_noint), color = "cyan4",width = .5, show.legend = F) +
  geom_jitter(data = densityDat, aes(x = treatmentCat, y = focalPlantMass_g), color = "cyan4", width = 0.1, alpha = 0.4) +
  theme_light() + 
  ylab("Focal Plant Mass (g)") +
  xlab("Density Treatment (plants per cone)") +
  ggtitle("Density Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n0", "Low\n1", "High\n2")) +
  ylim(0,.6)

p_density_mass_output
#'


#### Plant Mass: Density Pairwise Comparisons ####
# Veg Biomass:
densityDat_mass <- densityDat %>% filter( !is.na(focalPlantMass_g))
# High To All:
dens_mass_highbase <- densityDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "High"))

lme_mass_dens_highbase <- lme(focalPlantMass_g~treatmentCat, 
                              random = ~1|bin, 
                              data = dens_mass_highbase)

summary(lme_mass_dens_highbase)

# Low To All:

dens_mass_lowbase <- densityDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_mass_dens_lowbase <- lme(focalPlantMass_g~treatmentCat, 
                             random = ~1|bin, 
                             data = dens_mass_lowbase)

summary(lme_mass_dens_lowbase)

#### Plant Mass: Resource to Control ####
#' 
#' Get rid of NAs in the data
resourceDat_mass <- resourceDat %>% filter( focalPlantMass_g > 0)

#' __Linear mixed model__
lme_mass_resource <- lme(focalPlantMass_g~treatmentCat, 
                         random = ~1|bin,
                         data = resourceDat_mass)
summary(lme_mass_resource)
<<<<<<< Updated upstream
=======
anova(lme_mass_resource)
anova(lme_mass_resource, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_mass_resource, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_mass_resource)

#' There aren't super clear differences in the variance of the residuals among the treatments,
#' but I decided to fit the unequal variances model anyway to see what it looked like.
#'  
#' __Linear mixed model with unequal variances__
lme_mass_resource_uv <- lme(focalPlantMass_g~treatmentCat,
                            data = resourceDat_mass,
                            random = ~1|bin,
                            weights = varIdent(form = ~1|treatmentCat))

summary(lme_mass_resource_uv)
<<<<<<< Updated upstream
=======
anova(lme_mass_resource_uv)
anova(lme_mass_resource_uv, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_mass_resource_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_mass_resource_uv)

#' Looks like the unequal variances model fixes the issues. But is it worth the extra spending of degrees of freedom?
anova(lme_mass_resource, lme_mass_resource_uv) # yes!

#' _Final answer_
summary_lme_mass_resource_uv <- summary(lme_mass_resource_uv)
summary_lme_mass_resource_uv

#' _Anova Resource Mass_

anova(summary_lme_mass_resource_uv)

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_mass_resource_uv_noint <- lme(focalPlantMass_g~ 0 +treatmentCat,
                                  data = resourceDat_mass,
                                  random = ~1|bin,
                                  weights = varIdent(form = ~1|treatmentCat))

summary(lme_mass_resource_uv_noint)

#' _Final answer_
summary_lme_mass_resource_uv <- summary(lme_mass_resource_uv)
summary_lme_mass_resource_uv_noint <- summary(lme_mass_resource_uv_noint)

summary_lme_mass_resource_uv_noint

#' Plot the model outcome (work in progress)
mass_resource_output_df <- data.frame(cat_mean_noint = summary_lme_mass_resource_uv_noint$tTable[,"Value"],
                                      cat_se_noint = summary_lme_mass_resource_uv_noint$tTable[,"Std.Error"],
                                      trt_cat = levels(resourceDat$treatmentCat))
levels(resourceDat$treatmentCat)
mass_resource_output_df

p_res_mass_output <- ggplot(mass_resource_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_point() +
  geom_errorbar(aes(ymin=cat_mean_noint-cat_se_noint, ymax=cat_mean_noint+cat_se_noint),color = "goldenrod3", width = .5, show.legend = F) +
  geom_jitter(data = resourceDat, aes(x = treatmentCat, y = focalPlantMass_g),color = "goldenrod3", width = 0.1, alpha = 0.4, show.legend = F) +
  theme_light() + 
  ylab("Focal Plant mass (g)") +
  xlab("Nutrient Addition Treatment (tbsp nutrients/gal. water)") +
  ggtitle("Eutrophication Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n0", "Low\n0.5x", "High\n1x")) +
  ylim(-0.5, 2)
  

p_res_mass_output

#' ggsave("/Users/Courtney/Documents/Las-dispersal-plasticity-master/ray_mass_resource_means_plot.pdf", p_res_mass_output)


#### Plant Mass: Resource Pairwise Comparisons ####
# Biomass
resDat_mass <- resourceDat_mass %>% filter( !is.na(focalPlantMass_g))
# Medium To All:
res_mass_medbase <- resDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "Medium"))

lme_mass_res_medbase <- lme(focalPlantMass_g~treatmentCat,
                            data = res_mass_medbase,
                            random = ~1|bin,
                            weights = varIdent(form = ~1|treatmentCat))

summary(lme_mass_res_medbase)

# Low To All:

res_mass_lowbase <- resDat_mass %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_mass_res_lowbase <- lme(focalPlantMass_g~treatmentCat,
                            data = res_mass_lowbase,
                            random = ~1|bin,
                            weights = varIdent(form = ~1|treatmentCat))

summary(lme_mass_res_lowbase)

#### Ray Seed Proportion: Shade to Control ####
#' 
#' 
#' __Linear mixed model__
lme_propn_shade <- lme(phyllToTotal~treatmentCat, 
                       random = ~1|bin, 
                       data = shadeDat)
summary(lme_propn_shade)
<<<<<<< Updated upstream
=======
anova(lme_propn_shade)
anova(lme_propn_shade, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_shade, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_shade)

lme_propn_shade$tTable

#' There are again clear differences in the variance of the residuals among the treatments, so we fit an unequal variances model
#' 
#' __Linear mixed model with unequal variances__
lme_propn_shade_uv <- lme(phyllToTotal~treatmentCat,
                          data = shadeDat,
                          random = ~1|bin,
                          weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_shade_uv)
<<<<<<< Updated upstream
=======
anova(lme_propn_shade_uv)
anova(lme_propn_shade_uv, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_shade_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_shade_uv)

#' Looks like the unequal variances model fixes the issues. But is it worth the extra spending of degrees of freedom?
anova(lme_propn_shade, lme_propn_shade_uv) # yes!

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_propn_shade_uv_noint <- lme(phyllToTotal~ 0 +treatmentCat,
                                data = shadeDat,
                                random = ~1|bin,
                                weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_shade_uv_noint)

#' _Final answer_
summary_lme_propn_shade_uv <- summary(lme_propn_shade_uv)
summary_lme_propn_shade_uv_noint <- summary(lme_propn_shade_uv_noint)

#' _Anova Shade Proportion_ 

anova(lme_propn_shade_uv)

#' Plot the model outcome (work in progress)
shade_propn_output_df <- data.frame(cat_mean_noint = summary_lme_propn_shade_uv_noint$tTable[,"Value"],
                                    cat_se_noint = summary_lme_propn_shade_uv_noint$tTable[,"Std.Error"],
                                    trt_cat = levels(shadeDat$treatmentCat))

shade_propn_output_df

p_shade_propn_output <- ggplot(shade_propn_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_point() +
  geom_errorbar(aes(ymin=cat_mean_noint-cat_se_noint, ymax=cat_mean_noint+cat_se_noint), color = "brown4", width = .5, show.legend = F) +
  geom_jitter(data = shadeDat, aes(x = treatmentCat, y = phyllToTotal), color = "brown4", width = 0.1, alpha = 0.4, show.legend = F) +
  theme_light() + 
  ylab("# Phyllaries / Phyllaries + Disk Seeds") +
  xlab("Shade Treatment (µmol of photons/m^2s)") +
  ggtitle("Shade Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n122.0", "Structure\n107.0", "Low\n87.2", "Medium\n71.6", "High\n69.0")) +
  ylim(-.005, .2)

p_shade_propn_output

#' ggsave("/Users/Courtney/Documents/Las-dispersal-plasticity-master/ray_shade_propn_means_plot.pdf", p_shade_propn_output)


#### Ray Seed Proportion: Shade Pairwise Comparisons ####

#' _Shade Ray Seed Proportion All Bases_
#' Structure to all
shadeDat_propn <- shadeDat %>% filter( !is.na(phyllToTotal))
shadeDat_propn_structbase <- shadeDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "Structure"))

lme_propn_shade_structbase <- lme(phyllToTotal~treatmentCat, 
                                  random = ~1|bin, 
                                  data = shadeDat_propn_structbase)

summary(lme_propn_shade_structbase)
#' 
#' Low to all
shadeDat_propn_lowbase <- shadeDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_propn_shade_lowbase <- lme(phyllToTotal~treatmentCat, 
                               random = ~1|bin, 
                               data = shadeDat_propn_lowbase)

summary(lme_propn_shade_lowbase)
#' 
#' Medium to All
shadeDat_propn_medbase <- shadeDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "Medium"))

lme_propn_shade_medbase <- lme(phyllToTotal~treatmentCat, 
                               random = ~1|bin, 
                               data = shadeDat_propn_medbase)

summary(lme_propn_shade_medbase)

#' High to All
shadeDat_propn_highbase <- shadeDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "High"))

lme_propn_shade_highbase <- lme(phyllToTotal~treatmentCat, 
                                random = ~1|bin, 
                                data = shadeDat_propn_highbase)

summary(lme_propn_shade_highbase)


#### Ray Seed Proportion: Density to Control ####
#' 
#' 
#' __Linear mixed model__
lme_propn_dens <- lme(phyllToTotal~treatmentCat, 
                      random = ~1|bin, 
                      data = densityDat)
summary(lme_propn_dens)

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_dens, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_dens)

#' There are again clear differences in the variance of the residuals among the treatments, so we fit an unequal variances model
#' 
#' __Linear mixed model with unequal variances__
lme_propn_dens_uv <- lme(phyllToTotal~treatmentCat,
                         data = densityDat,
                         random = ~1|bin,
                         weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_dens_uv)

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_dens_uv, ~ residuals(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_dens_uv)

#' The unequal variances model is not worth it and does not fix the residual issues. Try a beta GLMM instead!
anova(lme_propn_dens, lme_propn_dens_uv)

#' __Beta GLMM__
bglmm_propn_dens <- glmmTMB(phyllToTotal ~ treatmentCat + (1|bin), family = beta_family("logit"), data = densityDat)
summary(bglmm_propn_dens)

#' _Anova Density Proportion_

anova(lme_propn_dens) #no trt effect

res <- resid(bglmm_propn_dens)
pred <- predict(bglmm_propn_dens)

ggplot(densityDat, aes(treatmentCat, res)) +
  geom_boxplot()

p_density_propn_output <- ggplot(densityDat, aes(pred, res)) +
  geom_point() 
p_density_propn_output
#' similar p-values, no significant difference. Which model should we go with for this dataset - beta glmm, unequal 
#' variances lme, or equal variances lme? I have not made a plot for this yet to avoid opening a can of worms for 
#' plotting this type of model in the same way. 
#' 
#' Plotting

lme_propn_density_uv_noint <- lme(phyllToTotal~ 0 +treatmentCat,
                                data = densityDat,
                                random = ~1|bin,
                                weights = varIdent(form = ~1|treatmentCat))

summary_lme_propn_density_noint <- summary(lme_propn_density_uv_noint)

propn_density_output_df <- data.frame(cat_mean_noint = summary_lme_propn_density_noint$tTable[,"Value"],
                                        cat_se_noint = summary_lme_propn_density_noint$tTable[,"Std.Error"],
                                        trt_cat = levels(densityDat$treatmentCat))

p_dens_propn_output <- ggplot(propn_density_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_point() +
  geom_errorbar(aes(ymin = cat_mean_noint - cat_se_noint, ymax = cat_mean_noint + cat_se_noint), color = "cyan4", width = .5, show.legend = F) +
  geom_jitter(data = densityDat_propn, aes(x = treatmentCat, y = phyllToTotal), color = "cyan4", width = 0.1, alpha = 0.4, show.legend = F) +
  theme_light() + 
  ylab("# Phyllaries / Phyllaries + Disk Seeds") +
  xlab("Density Treatment (plants per cone)") +
  ggtitle("Density Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n0", "Low\n1", "High\n2")) 


p_dens_propn_output

#### Ray Seed Proportion: Density Pairwise Comparisons ####
# Seed Proportion
densityDat_propn <- densityDat %>% filter( !is.na(phyllToTotal))
# High To All:
dens_propn_highbase <- densityDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "High"))

lme_propn_dens_highbase <- lme(phyllToTotal~treatmentCat, 
                               random = ~1|bin, 
                               data = dens_propn_highbase)

summary(lme_propn_dens_highbase)

# Low To All:

dens_propn_lowbase <- densityDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_propn_dens_lowbase <- lme(phyllToTotal~treatmentCat, 
                              random = ~1|bin, 
                              data = dens_propn_lowbase)

summary(lme_propn_dens_lowbase)


#### Ray Seed Proportion: Resource to Control ####
#' 
#' Get rid of NAs in the data
resourceDat_propn <- resourceDat %>% filter(!is.na(phyllToTotal))

#' __Linear mixed model__
lme_propn_resource <- lme(phyllToTotal~treatmentCat, 
                          random = ~1|bin,
                          data = resourceDat_propn)
summary(lme_propn_resource)

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_resource, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_resource)

#' There are clear differences in the variance of the residuals among the treatments, so we fit an unequal variances model.
#' 
#' __Linear mixed model with unequal variances__
lme_propn_resource_uv <- lme(phyllToTotal~treatmentCat,
                             data = resourceDat_propn,
                             random = ~1|bin,
                             weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_resource_uv)
anova(lme_propn_resource_uv)

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_resource_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_resource_uv)

#' Looks like the unequal variances model fixes the issues. But is it worth the extra spending of degrees of freedom?
anova(lme_propn_resource, lme_propn_resource_uv) # yes!

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_propn_resource_uv_noint <- lme(phyllToTotal ~ 0 + treatmentCat,
                                   data = resourceDat_propn,
                                   random = ~1|bin,
                                   weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_resource_uv_noint)

#' _Final answer_
summary_lme_propn_resource_uv <- summary(lme_propn_resource_uv)
summary_lme_propn_resource_uv_noint <- summary(lme_propn_resource_uv_noint)

#' _Anova Resource Propotion_

anova(lme_propn_resource_uv)

#' Plot the model outcome (work in progress)
propn_resource_output_df <- data.frame(cat_mean_noint = summary_lme_propn_resource_uv_noint$tTable[,"Value"],
                                       cat_se_noint = summary_lme_propn_resource_uv_noint$tTable[,"Std.Error"],
                                       trt_cat = levels(resourceDat$treatmentCat))
propn_resource_output_df


p_res_propn_output <- ggplot(propn_resource_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_point() +
  geom_errorbar(aes(ymin = cat_mean_noint - cat_se_noint, ymax = cat_mean_noint + cat_se_noint), color = "goldenrod4", width = .5, show.legend = F) +
  geom_jitter(data = resourceDat_propn, aes(x = treatmentCat, y = phyllToTotal), color = "goldenrod4", width = 0.1, alpha = 0.4, show.legend = F) +
  theme_light() +
  ylab("# Phyllaries / Phyllaries + Disk Seeds") +
  xlab("Nutrient Addition Treatment (tbsp nutrients/gal. water)") +
  ggtitle("Eutrophication Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n0", "Low\n0.5x", "Medium\n1x")) +
  ylim(-.01, .5)

p_res_propn_output



#### Ray Seed Proportion: Resource Pairwise Comparisons ####
# Seed Proportion
resDat_propn <- resourceDat %>% filter( !is.na(phyllToTotal))
# Medium To All:
res_propn_medbase <- resDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "Medium"))

lme_propn_res_medbase <- lme(phyllToTotal~treatmentCat,
                             data = res_propn_medbase,
                             random = ~1|bin,
                             weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_res_medbase)

# Low To All:

res_propn_lowbase <- resDat_propn %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_propn_res_lowbase <- lme(phyllToTotal~treatmentCat,
                             data = res_propn_lowbase,
                             random = ~1|bin,
                             weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_dens_lowbase)


#### Inflorescence Height: Shade to Control ####

#' Get rid of NA's
shadeDat_height <- shadeDat %>% filter( !is.na(focalFlowerHeight_cm))

#' __Linear mixed model__
lme_height_shade <- lme(focalFlowerHeight_cm~treatmentCat, 
                        random = ~1|bin, 
                        data = shadeDat_height)
summary(lme_height_shade)
# <<<<<<< Updated upstream
# =======
# anova(lme_height_shade)
# anova(lme_height_shade, type = "marginal")
# >>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_height_shade, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_height_shade)

lme_height_shade$tTable

#' There are again differences in the variance of the residuals among the treatments, so we fit an unequal variances model
#' 
#' __Linear mixed model with unequal variances__
lme_height_shade_uv <- lme(focalFlowerHeight_cm~treatmentCat,
                           data = shadeDat_height,
                           random = ~1|bin,
                           weights = varIdent(form = ~1|treatmentCat))

summary(lme_height_shade_uv)
# <<<<<<< Updated upstream
# =======
# anova(lme_height_shade_uv)
# anova(lme_height_shade_uv, type = "marginal")
# >>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_height_shade_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_height_shade_uv)

#' Looks like the unequal variances model somewhat fixes the issues. But is it worth the extra spending of degrees of freedom?
anova(lme_height_shade, lme_height_shade_uv) # No

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_height_shade_noint <- lme(focalFlowerHeight_cm~ 0 +treatmentCat,
                              data = shadeDat_height,
                              random = ~1|bin)

summary(lme_height_shade_noint)

#' _Final answer_
summary_lme_height_shade <- summary(lme_height_shade)
summary_lme_height_shade_noint <- summary(lme_height_shade_noint)

#' _Anova Shade Height_

anova(lme_height_shade)

#' Plot the model outcome (work in progress)
shade_height_output_df <- data.frame(cat_mean_noint = summary_lme_height_shade_noint$tTable[,"Value"],
                                     cat_se_noint = summary_lme_height_shade_noint$tTable[,"Std.Error"],
                                     trt_cat = levels(shadeDat$treatmentCat))

shade_height_output_df

p_shade_height_output <- ggplot(shade_height_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) +
  geom_point() +
  geom_errorbar(aes(ymin=cat_mean_noint-cat_se_noint, ymax=cat_mean_noint+cat_se_noint),color = "brown4", width = .5, show.legend = F) +
  geom_jitter(data = shadeDat, aes(x = treatmentCat, y = focalFlowerHeight_cm),color = "brown4", width = 0.1, alpha = 0.4, show.legend = F) +
  theme_light() +
  ylab("Focal Flower Height (cm)") +
  xlab("Shade Treatment (µmol of photons/m^2s)") +
  ggtitle("Shade Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n122.0", "Structure\n107.0", "Low\n87.2", "Medium\n71.6", "High\n69.0")) +
  ylim(-.5, 20)

p_shade_height_output
#'

<<<<<<< Updated upstream
#### Inflorescence Height: Shade Pairwise Comparisons ####
=======
#### #' #### Density: Ray seed proportion ####
#' 
#' 
#' __Linear mixed model__
lme_propn_dens <- lme(phyllToTotal~treatmentCat, 
                      random = ~1|bin, 
                      data = densityDat)
summary(lme_propn_dens)
anova(lme_propn_dens)
anova(lme_propn_dens, type = "marginal")
>>>>>>> Stashed changes


#' _Shade: Focal Flower Height All Bases_
shadeDat_height <- shadeDat %>% filter( !is.na(focalFlowerHeight_cm))

#' Structure Base
shadeDat_h_structbase <- shadeDat_height %>% mutate(treatmentCat = relevel(treatmentCat, "Structure"))

<<<<<<< Updated upstream
lme_h_shade_structbase <- lme(focalFlowerHeight_cm~treatmentCat, 
                              random = ~1|bin, 
                              data = shadeDat_h_structbase)
=======
summary(lme_propn_dens_uv)
anova(lme_propn_dens_uv)
anova(lme_propn_dens_uv, type = "marginal")
>>>>>>> Stashed changes

summary(lme_h_shade_structbase)

#' Low Base:
shadeDat_h_lowbase <- shadeDat_height %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_h_shade_lowbase <- lme(focalFlowerHeight_cm~treatmentCat, 
                           random = ~1|bin, 
                           data = shadeDat_h_lowbase)

summary(lme_h_shade_lowbase)

#' Medium Base
shadeDat_h_medbase <- shadeDat_height %>% mutate(treatmentCat = relevel(treatmentCat, "Medium"))

lme_h_shade_medbase <- lme(focalFlowerHeight_cm~treatmentCat, 
                           random = ~1|bin, 
                           data = shadeDat_h_medbase)

summary(lme_h_shade_medbase)


#### Inflorescence height: Density To Control #### 
#' 
#' Get rid of NA's
densityDat_height <- densityDat %>% filter( !is.na(focalFlowerHeight_cm))

#' __Linear mixed model__
lme_height_density <- lme(focalFlowerHeight_cm~treatmentCat, 
                          random = ~1|bin, 
                          data = densityDat_height)
summary(lme_height_density)
# <<<<<<< Updated upstream
# =======
# anova(lme_height_density)
# anova(lme_height_density, type = "marginal")
# >>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_height_density, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_height_density)

lme_height_density$tTable

#' Though the residual variances are somewhat different per fitted value, they are certainly much more evenly distrubuted than what we have seen with other response variables. Even so, we fit an unequal variances model and then compared it to the equal variances model.
#' 
#' __Linear mixed model with unequal variances__
lme_height_density_uv <- lme(focalFlowerHeight_cm~treatmentCat,
                             data = densityDat_height,
                             random = ~1|bin,
                             weights = varIdent(form = ~1|treatmentCat))

summary(lme_height_density_uv)
<<<<<<< Updated upstream
=======
anova(lme_height_density_uv)
anova(lme_height_density_uv, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_height_density_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_height_density_uv)

#' The variance structure by fitted value seems to be somewhat more normal using the unequal variances model, though it looks like the gains are marginal, at best. Is it worth it to use the unequal variances model at the cost of degrees of freedom?
anova(lme_height_density, lme_height_density_uv) # No

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_height_density_noint <- lme(focalFlowerHeight_cm~ 0 +treatmentCat,
                                data = densityDat_height,
                                random = ~1|bin)

summary(lme_height_density_noint)

#' _Final answer_
summary_lme_height_density <- summary(lme_height_density)
summary_lme_height_density_noint <- summary(lme_height_density_noint)

#' _Anova Density Height_

anova(lme_height_density)

#' Plot the model outcome (work in progress)
density_height_output_df <- data.frame(cat_mean_noint = summary_lme_height_density_noint$tTable[,"Value"],
                                       cat_se_noint = summary_lme_height_density_noint$tTable[,"Std.Error"],
                                       trt_cat = levels(densityDat$treatmentCat))

density_height_output_df

p_density_height_output <- ggplot(density_height_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_point() +
  geom_errorbar(aes(ymin=cat_mean_noint-cat_se_noint, ymax=cat_mean_noint+cat_se_noint),color = "cyan4", width = .5, show.legend = F) +
  geom_jitter(data = densityDat, aes(x = treatmentCat, y = focalFlowerHeight_cm),color = "cyan4", width = 0.1, alpha = 0.4, show.legend = F) +
  theme_light() +
  ylab("Focal Flower Height (cm)") +
  xlab("Density Treatment") +
  xlab("Density Treatment (plants per cone)") +
  ggtitle("Density Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n0", "Low\n1", "High\n2")) 


p_density_height_output
#'
#' ggsave("")
#'

<<<<<<< Updated upstream
#### Inflorescence Height: Density Pairwise Comparisons ####
# Flower Height:
=======
#' #### Resource: Ray seed proportion
#' 
#' Get rid of NAs in the data
resourceDat_propn <- resourceDat %>% filter(!is.na(phyllToTotal))

#' __Linear mixed model__
lme_propn_resource <- lme(phyllToTotal~treatmentCat, 
                          random = ~1|bin,
                          data = resourceDat_propn)
summary(lme_propn_resource)
anova(lme_propn_resource)
anova(lme_propn_resource, type = "marginal")

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_resource, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_resource)

#' There are clear differences in the variance of the residuals among the treatments, so we fit an unequal variances model.
#' 
#' __Linear mixed model with unequal variances__
lme_propn_resource_uv <- lme(phyllToTotal~treatmentCat,
                       data = resourceDat_propn,
                       random = ~1|bin,
                       weights = varIdent(form = ~1|treatmentCat))

summary(lme_propn_resource_uv)
anova(lme_propn_resource_uv)
anova(lme_propn_resource_uv, type = "marginal")

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_propn_resource_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_propn_resource_uv)
>>>>>>> Stashed changes

densityDat_h <- densityDat %>% filter( !is.na(focalFlowerHeight_cm))
# High To All:
dens_h_highbase <- densityDat_h %>% mutate(treatmentCat = relevel(treatmentCat, "High"))

lme_h_dens_highbase <- lme(focalFlowerHeight_cm~treatmentCat, 
                           random = ~1|bin, 
                           data = dens_h_highbase)

summary(lme_h_dens_highbase)

# Low To All:

dens_h_lowbase <- densityDat_h %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_h_dens_lowbase <- lme(focalFlowerHeight_cm~treatmentCat, 
                          random = ~1|bin, 
                          data = dens_h_lowbase)

summary(lme_h_dens_lowbase)



#### Inflorescence Height: Resource to Control ####
#' 
#' Get rid of NAs in the data
resourceDat_height <- resourceDat %>% filter( focalFlowerHeight_cm > 0)

#' __Linear mixed model__
lme_height_resource <- lme(focalFlowerHeight_cm~treatmentCat, 
                           random = ~1|bin,
                           data = resourceDat_height)
summary(lme_height_resource)
<<<<<<< Updated upstream
=======
anova(lme_height_resource)
anova(lme_height_resource, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_height_resource, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_height_resource)

#' There are differences in the variance of the residuals among the treatments, so we fit an unequal variances model
#' 
#' __Linear mixed model with unequal variances__
lme_height_resource_uv <- lme(focalFlowerHeight_cm~treatmentCat,
                              data = resourceDat_height,
                              random = ~1|bin,
                              weights = varIdent(form = ~1|treatmentCat))

summary(lme_height_resource_uv)
<<<<<<< Updated upstream
=======
anova(lme_height_resource_uv)
anova(lme_height_resource_uv, type = "marginal")
>>>>>>> Stashed changes

#' _Diagnostic plots_
#' 
#' qqplot to test for normality
qqnorm(lme_height_resource_uv, ~ resid(., type = "p"), abline = c(0, 1))

#' Residuals vs fitted plot
plot.lme(lme_height_resource_uv)

#' Looks like the unequal variances model fixes the issues. But is it worth the extra spending of degrees of freedom?
anova(lme_height_resource, lme_height_resource_uv) # no!

#' Fit the same model without at intercept to get the standard errors of the means for each group (for plotting)
lme_height_resource_noint <- lme(focalFlowerHeight_cm ~ 0 + treatmentCat,
                                 data = resourceDat_height,
                                 random = ~1|bin)

summary(lme_height_resource_noint)

#' _Final answer_
summary_lme_height_resource <- summary(lme_height_resource)
summary_lme_height_resource_noint <- summary(lme_height_resource_noint)

#' _Anova Resource Flower Height_

anova(lme_height_resource)

#' Plot the model outcome (work in progress)
height_resource_output_df <- data.frame(cat_mean_noint = summary_lme_height_resource_noint$tTable[,"Value"],
                                        cat_se_noint = summary_lme_height_resource_noint$tTable[,"Std.Error"],
                                        trt_cat = levels(resourceDat$treatmentCat))

height_resource_output_df

p_res_height_output <- ggplot(height_resource_output_df, aes(x=factor(trt_cat, levels = labelOrders), y=cat_mean_noint)) + 
  geom_point() +
  geom_errorbar(aes(ymin = cat_mean_noint - cat_se_noint, ymax = cat_mean_noint + cat_se_noint), color = "goldenrod4", width = .5, show.legend = F) +
  geom_jitter(data = resourceDat_height, aes(x = treatmentCat, y = focalFlowerHeight_cm), color = "goldenrod4", width = 0.1, alpha = 0.4, show.legend = F) +
  theme_light() +
  ylab("Focal Flower Height (cm)") +
  xlab("Nutrient Addition Treatment (tbsp nutrients/gal. water)") +
  ggtitle("Eutrophication Experiment") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) +
  scale_x_discrete(labels = c("Control\n0", "Low\n0.5x", "Medium\n1x")) 

p_res_height_output

#' ggsave("/Users/Courtney/Documents/Las-dispersal-plasticity-master/ray_height_resource_means_plot.pdf", p_res_height_output)


#### Inflorescence Height: Resource Pairwise Comparisons ####
# Flower Height:

resDat_h <- resourceDat_height %>% filter( !is.na(focalFlowerHeight_cm))
# High To All:
res_h_medbase <- resDat_h %>% mutate(treatmentCat = relevel(treatmentCat, "Medium"))

lme_h_res_medbase <- lme(focalFlowerHeight_cm~treatmentCat, 
                         random = ~1|bin, 
                         data = res_h_medbase)

summary(lme_h_res_medbase)

# Low To All:

res_h_lowbase <- resDat_h %>% mutate(treatmentCat = relevel(treatmentCat, "Low"))

lme_h_res_lowbase <- lme(focalFlowerHeight_cm~treatmentCat, 
                         random = ~1|bin, 
                         data = res_h_lowbase)

summary(lme_h_res_lowbase)

#### Final Plots ####

#' Biomass Plot

biomassGrid <- grid.arrange(p_shade_mass_output, p_density_mass_output, p_res_mass_output, nrow = 1, top = "Biomass Response to Treatment Effect by Experiment")

propnHeightGrid <- grid.arrange(p_shade_propn_output,  p_dens_propn_output, p_res_propn_output, p_shade_height_output, p_density_height_output, p_res_height_output, nrow = 2, ncol = 3, top = "Dispersal Propensity and Inflorescence Height Response to Treatment Effect by Experiment")


#### Spinning to Knitr ####
#' Spin into an html file
#' 
knitr::spin("/Users/jacksonanderson/Desktop/DispersalPlasticity/Dispersal_Git/Las-dispersal-plasticity/FinalAnalysesAndMarkdown")
getwd()