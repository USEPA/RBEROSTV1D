###########################################################################################
# PURPOSE: Calculate BMP Efficiciencies for Stormwater BMPs ####
# BY: Cathy Chamberlin ####
# DATE: 11/16/2020 ####
###########################################################################################

# Lines of code tagged with #*# may need to be edited by the user.

##################################################
# 1. Setup ####
##################################################

# install.packages("tidyverse") #*# # This line of code may be necessary if packages are not installed
library(tidyverse)
theme_set(theme_classic())


##################################################
# 2. Read in Data ####
##################################################

urban.BMP.effic.dat.raw <- read.csv(
  "./Data/StormWaterBMPPerformanceCurveData.csv" #*#
)

##################################################
# 3. Format BMP Performance Data ####
##################################################


urban.BMP.effic.dat <- urban.BMP.effic.dat.raw %>%
  rename(BMP_Name = 1) %>%
  mutate(
    LoadReduction = as.numeric(sub("%", '', LoadReduction)) * 0.01, 
    Pollutant = as.factor(
      case_when(
        Pollutant %in% c(
          "Cumulative Nitrogen Load\nReduction", "Nitrogen"
        ) ~ "Nitrogen",
        Pollutant %in% c(
          "Cumulative Phosphorus Load\nReduction", "Phosphorus"
        ) ~ "Phosphorus",
        Pollutant %in% c(
          "Cumulative TSS Phosphorus Load\nReduction", "TSS"
        ) ~ "TSS",
        Pollutant %in% c(
          "Cumulative Zinc Phosphorus Load\nReduction", "Zinc"
        ) ~ "Zinc",
        Pollutant == "Runoff Volume" ~ "Runoff Volume"
      )
    ), 
    BMP_Name = as.factor(BMP_Name), 
    InfiltrationRate_inperhr = as.factor(InfiltrationRate_inperhr)
  )

#####
# #Plot Curves to compare with ms4_nomographs.pdf
# 
# ggplot(
#   data = urban.BMP.effic.dat %>% 
#     filter(
#       !(
#         BMP_Name %in% 
#           c("Porous_Pavement", "InfiltrationBasin", "InfiltrationTrench")
#         )
#       ), 
#   mapping = aes(
#     x = DepthofRunoffFromImpvArea_in, 
#     y = LoadReduction, 
#     group = Pollutant, 
#     color = Pollutant
#     )
#   ) +
#   geom_line() +
#   facet_wrap('BMP_Name')
# 
# ggplot(
#   data = urban.BMP.effic.dat %>% 
#     filter(BMP_Name == "Porous_Pavement"), 
#   mapping = aes(
#     x = DepthofRunoffFromImpvArea_in, 
#     y = LoadReduction, 
#     group = Pollutant, 
#     color = Pollutant
#     )
#   ) +
#   geom_line() +
#   ggtitle("Porous Pavement")
# 
# ggplot(
#   data = urban.BMP.effic.dat %>% 
#     filter( BMP_Name %in% c("InfiltrationBasin", "InfiltrationTrench")), 
#   mapping = aes(
#     x = DepthofRunoffFromImpvArea_in, 
#     y = LoadReduction, 
#     group = interaction(Pollutant, InfiltrationRate_inperhr), 
#     color = Pollutant,
#     shape = InfiltrationRate_inperhr
#     )
#   ) +
#   geom_line() +
#   geom_point() + 
#   facet_wrap('BMP_Name')
#####

##################################################
# 4. Make BMP Dataframe ####
##################################################

urban.BMPs.list <- c( #*# If users wish to add new BMPs, efficiency data will need to be identified and new curves developed
  "Biofiltration_w_Underdrain",               
  "Bioretention_Basin",
  "Enhanced_Biofiltration_w_ISR",
  "Extended_Dry_Detention_Basin",
  "Grass_Swale_w_detention",
  "Gravel_Wetland",                           
  "Infiltration_Basin",
  "Infiltration_Chamber",
  "Infiltration_Trench",
  "Porous_Pavement_w_subsurface_infiltration",
  "Porous_Pavement_w_underdrain",
  "Sand_Filter_w_underdrain",                 
  "Wet_Pond" 
)

# The following expands the efficiencies by InfiltrationRate
Urban.BMP.Efficiency.Curves_blank <- expand_grid( # First for BMPs that don't differ by infiltration rate
  BMP = urban.BMPs.list[
    -which(
      urban.BMPs.list %in% c(
        "Infiltration_Basin",
        "Infiltration_Trench",
        "Infiltration_Chamber",
        "Porous_Pavement_w_subsurface_infiltration"
      )
    )
  ],
  Pollutant = c("N", "P"),
  InfiltrationRate_inperhr = NA_real_,
  Coef.1 = NA_real_,
  Coef.2 = NA_real_,
  Coef.3 = NA_real_,
  Coef.1_se = NA_real_,
  Coef.2_se = NA_real_,
  Coef.3_se = NA_real_,
  Best.Fit.Curve = NA_character_
) %>%
  add_row(
    expand_grid( # Second for BMPs that do differ by infiltration rate
      BMP = c(
        "Infiltration_Basin",
        "Infiltration_Trench",
        "Infiltration_Chamber",
        "Porous_Pavement_w_subsurface_infiltration"
      ),
      Pollutant = c("N", "P"),
      InfiltrationRate_inperhr = c(NA_real_, 0.17, 0.27, 0.52, 1.02, 2.41, 8.27), # keep the NA in case infiltration rates are not provided
      Coef.1 = NA_real_,
      Coef.2 = NA_real_,
      Coef.3 = NA_real_,
      Best.Fit.Curve = NA_character_
    )
  )

# # The following can be used to ignore infiltration rate differences 
# Urban.BMP.Efficiency.Curves_blank <- expand_grid( #*#
#   BMP = urban.BMPs.list, 
#   Pollutant = c("N", "P"),
#   InfiltrationRate_inperhr = NA_real_,
#   Coef.1 = NA_real_, 
#   Coef.2 = NA_real_, 
#   Coef.3 = NA_real_, 
#   Best.Fit.Curve = NA_character_
#   ) 

##################################################
# 5. Model efficiency curves for each BMP ####
##################################################

# This segment is verbose, displaying multiple models for some BMPs. The best fit models are used in the following code, but the alternatives are kept if future users would prefer to use them.

## Biofiltration_w_Underdrain & BMP == Bioretention_Basin

biofiltration.N.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Biofiltration", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(biofiltration.N.model) # log model has significant coefficients & a good adjusted R squared

biofiltration.P.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Biofiltration", Pollutant == "Phosphorus"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(biofiltration.P.model) # log model has significant coefficients & a good adjusted R squared

## Enhanced_Biofiltration_w_ISR

enhancedbiofiltrationwISR.N.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Enhanced_Biofiltration_w_ISR", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(enhancedbiofiltrationwISR.N.model) # log model has significant coefficients & a good adjusted R squared

enhancedbiofiltrationwISR.P.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Enhanced_Biofiltration_w_ISR", Pollutant == "Phosphorus"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(enhancedbiofiltrationwISR.P.model) # log model has significant coefficients & a good adjusted R squared

## "Extended_Dry_Detention_Basin" & "Grass_Swale_w_detention"

grassswalewdetention.N.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Grass_Swale", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(grassswalewdetention.N.model) # log model has significant coefficients, but an adjusted R-square less than 0.9

grassswalewdetention.N.model.quadratic <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Grass_Swale", Pollutant == "Nitrogen") %>%
    mutate(
      DepthofRunoffFromImpvArea_in_sqrd = DepthofRunoffFromImpvArea_in ^ 2
    ),
  lm(
    LoadReduction ~ 
      DepthofRunoffFromImpvArea_in_sqrd + DepthofRunoffFromImpvArea_in
  )
)
summary(grassswalewdetention.N.model.quadratic) # quadratic model has significant coefficients in all but the intercept, and has a good adjusted R squared

grassswalewdetention.N.model.linear <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Grass_Swale", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ DepthofRunoffFromImpvArea_in)
)
summary(grassswalewdetention.N.model.linear) # linear model has a significant slope, and has a good adjusted R squared

extractAIC(grassswalewdetention.N.model)
extractAIC(grassswalewdetention.N.model.quadratic) # The quadratic model has the lowest AIC
extractAIC(grassswalewdetention.N.model.linear)

grassswalewdetention.P.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Grass_Swale", Pollutant == "Phosphorus"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(grassswalewdetention.P.model) # log model has significant coefficients, but an adjusted R-square less than 0.9

grassswalewdetention.P.model.quadratic <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Grass_Swale", Pollutant == "Phosphorus") %>%
    mutate(
      DepthofRunoffFromImpvArea_in_sqrd = DepthofRunoffFromImpvArea_in ^ 2
    ),
  lm(
    LoadReduction ~ 
      DepthofRunoffFromImpvArea_in_sqrd + DepthofRunoffFromImpvArea_in
  )
)
summary(grassswalewdetention.P.model.quadratic) # quadratic model has significant coefficients in all but the intercept, and has a good adjusted R squared

grassswalewdetention.P.model.linear <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Grass_Swale", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ DepthofRunoffFromImpvArea_in)
)
summary(grassswalewdetention.P.model.linear) # linear model has a significant slope, and has a good adjusted R squared

extractAIC(grassswalewdetention.P.model)
extractAIC(grassswalewdetention.P.model.quadratic) # The quadratic model has the lowest AIC
extractAIC(grassswalewdetention.P.model.linear)

## Gravel_Wetland, Surface_Constructed_Wetland, Subsurface_Gravel_Wetland

gravelwetland.N.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Gravel_Wetlands", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(gravelwetland.N.model) # log model has significant coefficients & a good adjusted R squared

gravelwetland.P.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Gravel_Wetlands", Pollutant == "Phosphorus"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(gravelwetland.P.model) # log model has significant coefficients & a good adjusted R squared

## Infiltration Basin

infiltrationbasin.n.models <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationBasin", Pollutant == "Nitrogen") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[1],
    coef2 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[2],
    coef1_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[1,2],
    coef2_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[2,2],
    adj_R_sqr = summary(
      lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
    )$adj.r.squared
  ) %>%# none of these models match ICF's, and the adjusted R squares are pretty low. Graphically, they don' match up either.
  mutate(
    InfiltrationRate_inperhr = as.numeric(
      as.character(InfiltrationRate_inperhr)
    )
  )
infiltrationbasin.n.models

infiltrationbasin.n.models.saturating <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationBasin", Pollutant == "Nitrogen") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1],
    coef2 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2],
    coef3 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3],
    coef1_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1,2],
    coef2_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2,2],
    coef3_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3,2]
  ) %>%# These models fit the data much better and conceptually won't allow efficiencies over 100%
  mutate(
    InfiltrationRate_inperhr = as.numeric(
      as.character(InfiltrationRate_inperhr)
    )
  )
infiltrationbasin.n.models.saturating

## This model below is the temporary holding place that does not differentiate by soil infiltration rate
infiltrationbasin.n.model.saturating <- with(
  urban.BMP.effic.dat %>%
    filter(BMP_Name == "InfiltrationBasin", Pollutant == "Nitrogen"),
  summary(
    nls(
      LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
      start = list(A = 0, C = 1, k = -1))
  )
)
infiltrationbasin.n.model.saturating

infiltrationbasin.p.models <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationBasin", Pollutant == "Phosphorus") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[1],
    coef2 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[2],
    coef1_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[1,2],
    coef2_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[2,2],
    adj_R_sqr = summary(
      lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
    )$adj.r.squared
  )  %>%
  mutate(
    InfiltrationRate_inperhr = as.numeric(
      as.character(InfiltrationRate_inperhr)
    )
  )
infiltrationbasin.p.models# none of these models match ICF's, and the adjusted R squares are pretty low for higher infiltration values. 

infiltrationbasin.p.models.saturating <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationBasin", Pollutant == "Phosphorus") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1],
    coef2 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2],
    coef3 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3],
    coef1_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1,2],
    coef2_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2,2],
    coef3_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3,2]
  ) %>%
  mutate(
    InfiltrationRate_inperhr = as.numeric(
      as.character(InfiltrationRate_inperhr)
    )
  )
infiltrationbasin.p.models.saturating# These models fit the data much better and conceptually won't allow efficiencies over 100%

## This model below is the temporary holding place that does not differentiate by soil infiltration rate
infiltrationbasin.p.model.saturating <- with(
  urban.BMP.effic.dat %>%
    filter(BMP_Name == "InfiltrationBasin", Pollutant == "Phosphorus"),
  summary(
    nls(
      LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
      start = list(A = 0, C = 1, k = -1))
  )
)
infiltrationbasin.p.model.saturating

## the following code combines the infiltration-rate specific models and the general model
infiltrationbasin.n.models.all <- infiltrationbasin.n.models.saturating %>% 
  add_row(
    InfiltrationRate_inperhr = NA_real_, 
    coef1 = coef(infiltrationbasin.n.model.saturating)[1], 
    coef2 = coef(infiltrationbasin.n.model.saturating)[2], 
    coef3 = coef(infiltrationbasin.n.model.saturating)[3],
    coef1_se = infiltrationbasin.n.model.saturating$coefficients[1,2], 
    coef2_se = infiltrationbasin.n.model.saturating$coefficients[2,2], 
    coef3_se = infiltrationbasin.n.model.saturating$coefficients[3,2]
  )

infiltrationbasin.p.models.all <- infiltrationbasin.p.models.saturating %>% 
  add_row(
    InfiltrationRate_inperhr = NA_real_, 
    coef1 = coef(infiltrationbasin.p.model.saturating)[1], 
    coef2 = coef(infiltrationbasin.p.model.saturating)[2], 
    coef3 = coef(infiltrationbasin.p.model.saturating)[3],
    coef1_se = infiltrationbasin.p.model.saturating$coefficients[1,2], 
    coef2_se = infiltrationbasin.p.model.saturating$coefficients[2,2], 
    coef3_se = infiltrationbasin.p.model.saturating$coefficients[3,2]
  )

#####
# ### These plots exhibit the difference between the two infiltrationbasin models. Dashed black lines are y ~ A*log(x)+B and solid black lines are y ~ A + C*(1-e^(k*x)) both for the 0.52 in/hr infiltration rate
# my_fun_exp_n <- function(x) {
#   infiltrationbasin.n.models$coef1[3] +
#     infiltrationbasin.n.models$coef2[3] * log(x)
# }
# my_fun_sat_n <- function(x) {
#   infiltrationbasin.n.models.saturating$coef1[3] +
#     infiltrationbasin.n.models.saturating$coef2 [3] * (
#       1 - exp(infiltrationbasin.n.models.saturating$coef3[3] * x)
#     )
#   }
# 
# 
# ggplot(
#   data = urban.BMP.effic.dat %>%
#     filter(
#       BMP_Name == "InfiltrationBasin", Pollutant == 'Nitrogen'
#       ),
#   mapping = aes(
#     x = DepthofRunoffFromImpvArea_in,
#     y = LoadReduction,
#     group = InfiltrationRate_inperhr,
#     color = InfiltrationRate_inperhr
#     )
#   ) +
#   geom_line() +
#   ggtitle("Nitrogen") +
#   stat_function(fun = my_fun_exp_n, color = "black", lty = 2, lwd = 2) +
#   stat_function(fun = my_fun_sat_n, color = "black", lwd = 2)
# 
# my_fun_exp_p <- function(x) {
#   infiltrationbasin.p.models$coef1[3] + 
#     infiltrationbasin.p.models$coef2[3] * log(x)
# }
# my_fun_sat_p <- function(x) {
#   infiltrationbasin.p.models.saturating$coef1[3] + 
#     infiltrationbasin.p.models.saturating$coef2 [3] * (
#       1 - exp(infiltrationbasin.p.models.saturating$coef3[3] * x)
#     )
#   }
# 
# 
# ggplot(
#   data = urban.BMP.effic.dat %>%
#     filter(
#       BMP_Name == "InfiltrationBasin", Pollutant == 'Phosphorus'
#       ),
#   mapping = aes(
#     x = DepthofRunoffFromImpvArea_in, 
#     y = LoadReduction, 
#     group = InfiltrationRate_inperhr, 
#     color = InfiltrationRate_inperhr
#     )
#   ) + 
#   geom_line() +
#   ggtitle("Phosphorus") +
#   stat_function(fun = my_fun_exp_p, color = "black", lty = 2, lwd = 2) +
#   stat_function(fun = my_fun_sat_p, color = "black", lwd = 2)
#####

## Infiltration Trench, Infiltration_Chamber, Porous_Pavement_w_subsurface_infiltration

infiltrationtrench.n.models <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationTrench", Pollutant == "Nitrogen") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[1],
    coef2 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[2],
    coef1_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[1,2],
    coef2_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[2,2],
    adj_R_sqr = summary(
      lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
    )$adj.r.squared
  ) 
infiltrationtrench.n.models# none of these models match ICF's, and the adjusted R squares are pretty low. 

infiltrationtrench.n.models.saturating <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationTrench", Pollutant == "Nitrogen") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1],
    coef2 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2],
    coef3 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3],
    coef1_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1,2],
    coef2_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2,2],
    coef3_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3,2]
  ) 
infiltrationtrench.n.models.saturating# These models fit the data much better and conceptually won't allow efficiencies over 100%
with(infiltrationbasin.n.models.saturating, coef1 + coef2) # This represents the limit of the functions

## This model below is the temporary holding place that does not differentiate by soil infiltration rate
infiltrationtrench.n.model.saturating <- with(
  urban.BMP.effic.dat %>%
    filter(BMP_Name == "InfiltrationTrench", Pollutant == "Nitrogen"),
  summary(
    nls(
      LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
      start = list(A = 0, C = 1, k = -1))
  )
)
infiltrationtrench.n.model.saturating

infiltrationtrench.p.models <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationTrench", Pollutant == "Phosphorus") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[1],
    coef2 = lm(
      LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
    )$coefficients[2],
    coef1_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[1,2],
    coef2_se = summary(
      lm(
        LoadReduction ~ log(DepthofRunoffFromImpvArea_in)
      )
    )$coefficients[2,2],
    adj_R_sqr = summary(
      lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
    )$adj.r.squared
  ) 
infiltrationtrench.p.models# none of these models match ICF's, and the adjusted R squares low for higher infiltration rates. 

infiltrationtrench.p.models.saturating <- urban.BMP.effic.dat %>%
  filter(BMP_Name == "InfiltrationTrench", Pollutant == "Phosphorus") %>%
  group_by(InfiltrationRate_inperhr) %>%
  summarize(
    coef1 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1],
    coef2 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2],
    coef3 = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3],
    coef1_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[1,2],
    coef2_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[2,2],
    coef3_se = summary(
      nls(
        LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
        start = list(A = 0, C = 1, k = -1))
    )$coefficients[3,2]
  ) 
infiltrationtrench.p.models.saturating# These models fit the data much better and conceptually won't allow efficiencies over 100%
with(infiltrationbasin.p.models.saturating, coef1 + coef2) # This represents the limit of the functions

## This model below is the temporary holding place that does not differentiate by soil infiltration rate
infiltrationtrench.p.model.saturating <- with(
  urban.BMP.effic.dat %>%
    filter(BMP_Name == "InfiltrationTrench", Pollutant == "Phosphorus"),
  summary(
    nls(
      LoadReduction ~ A + C * (1 - exp(k * DepthofRunoffFromImpvArea_in)), 
      start = list(A = 0, C = 1, k = -1))
  )
)
infiltrationtrench.p.model.saturating

## the following code combines the infiltration-rate specific models and the general model
infiltrationtrench.n.models.all <- infiltrationtrench.n.models.saturating %>% 
  add_row(
    InfiltrationRate_inperhr = NA, 
    coef1 = coef(infiltrationtrench.n.model.saturating)[1], 
    coef2 = coef(infiltrationtrench.n.model.saturating)[2], 
    coef3 = coef(infiltrationtrench.n.model.saturating)[3],
    coef1_se = infiltrationtrench.n.model.saturating$coefficients[1,2], 
    coef2_se = infiltrationtrench.n.model.saturating$coefficients[2,2], 
    coef3_se = infiltrationtrench.n.model.saturating$coefficients[3,2]
  )

infiltrationtrench.p.models.all <- infiltrationtrench.p.models.saturating %>% 
  add_row(
    InfiltrationRate_inperhr = NA, 
    coef1 = coef(infiltrationtrench.p.model.saturating)[1], 
    coef2 = coef(infiltrationtrench.p.model.saturating)[2], 
    coef3 = coef(infiltrationtrench.p.model.saturating)[3],
    coef1_se = infiltrationtrench.p.model.saturating$coefficients[1,2], 
    coef2_se = infiltrationtrench.p.model.saturating$coefficients[2,2], 
    coef3_se = infiltrationtrench.p.model.saturating$coefficients[3,2]
  )

#####
# ### These plots exhibit the difference between the two infiltration trench models. Dashed black lines are y ~ A*log(x)+B and solid black lines are y ~ A + C*(1-e^(k*x)) both for the 0.52 in/hr infiltration rate
# my_fun_exp_n <- function(x) {
#   infiltrationtrench.n.models$coef1[3] +
#     infiltrationtrench.n.models$coef2[3] * log(x)
# }
# my_fun_sat_n <- function(x) {
#   infiltrationtrench.n.models.saturating$coef1[3] +
#     infiltrationtrench.n.models.saturating$coef2 [3] * (
#       1 - exp(infiltrationtrench.n.models.saturating$coef3[3] * x)
#     )
#   }
# 
# 
# ggplot(
#   data = urban.BMP.effic.dat %>%
#     filter(
#       BMP_Name == "InfiltrationTrench", Pollutant == 'Nitrogen'
#       ),
#   mapping = aes(
#     x = DepthofRunoffFromImpvArea_in,
#     y = LoadReduction,
#     group = InfiltrationRate_inperhr,
#     color = InfiltrationRate_inperhr
#     )
#   ) +
#   geom_line() +
#   ggtitle("Nitrogen") +
#   stat_function(fun = my_fun_exp_n, color = "black", lty = 2, lwd = 2) +
#   stat_function(fun = my_fun_sat_n, color = "black", lwd = 2)
# 
# my_fun_exp_p <- function(x) {
#   infiltrationtrench.p.models$coef1[3] +
#     infiltrationtrench.p.models$coef2[3] * log(x)
# }
# my_fun_sat_p <- function(x) {
#   infiltrationtrench.p.models.saturating$coef1[3] +
#     infiltrationtrench.p.models.saturating$coef2 [3] * (
#       1 - exp(infiltrationtrench.p.models.saturating$coef3[3] * x)
#     )
#   }
# 
# 
# ggplot(
#   data = urban.BMP.effic.dat %>%
#     filter(
#       BMP_Name == "InfiltrationTrench", Pollutant == 'Phosphorus'
#       ),
#   mapping = aes(
#     x = DepthofRunoffFromImpvArea_in,
#     y = LoadReduction,
#     group = InfiltrationRate_inperhr,
#     color = InfiltrationRate_inperhr
#     )
#   ) +
#   geom_line() +
#   ggtitle("Phosphorus") +
#   stat_function(fun = my_fun_exp_p, color = "black", lty = 2, lwd = 2) +
#   stat_function(fun = my_fun_sat_p, color = "black", lwd = 2)
#####

## Porous_Pavement_w_underdrain


porouspavementwunderdrain.N.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Porous_Pavement", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(porouspavementwunderdrain.N.model) # log model does not have significant coefficients, and the adjusted R-square less than 0.9

porouspavementwunderdrain.N.model.quadratic <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Porous_Pavement", Pollutant == "Nitrogen") %>%
    mutate(
      DepthofRunoffFromImpvArea_in_sqrd = DepthofRunoffFromImpvArea_in ^ 2
    ),
  lm(
    LoadReduction ~ 
      DepthofRunoffFromImpvArea_in_sqrd + DepthofRunoffFromImpvArea_in
  )
)
summary(porouspavementwunderdrain.N.model.quadratic) # quadratic model has significant coefficients in all but the intercept, but an adjusted R-square less than 0.9

porouspavementwunderdrain.N.model.linear <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Porous_Pavement", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ DepthofRunoffFromImpvArea_in)
)
summary(porouspavementwunderdrain.N.model.linear) # linear model does not have a significant coefficient for design depth, the adjusted R-square less than 0.9, but is higher than for the other two

porouspavementwunderdrain.N.model.constant <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Porous_Pavement", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ rep(1, length(DepthofRunoffFromImpvArea_in)))
)
summary(porouspavementwunderdrain.N.model.constant) # constant model is simple and significant.


extractAIC(porouspavementwunderdrain.N.model)
extractAIC(porouspavementwunderdrain.N.model.linear) # The linear model has the lowest AIC value of -41.1
extractAIC(porouspavementwunderdrain.N.model.quadratic)
extractAIC(porouspavementwunderdrain.N.model.constant)


porouspavementwunderdrain.P.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Porous_Pavement", Pollutant == "Phosphorus"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(porouspavementwunderdrain.P.model) # log model has significant coefficients and a good R^2

## Sand_Filter_w_underdrain and Wet_Pond
wetpond.N.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Wet_Pond", Pollutant == "Nitrogen"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(wetpond.N.model) # log model has significant coefficients & a good adjusted R squared

wetpond.P.model <- with(
  urban.BMP.effic.dat %>% 
    filter(BMP_Name == "Wet_Pond", Pollutant == "Phosphorus"),
  lm(LoadReduction ~ log(DepthofRunoffFromImpvArea_in))
)
summary(wetpond.P.model) # log model has significant coefficients & a good adjusted R squared

##################################################
# 6. Write Models into Urban.BMP.Efficiences.Curve ####
##################################################

# We insert the best fit models from the above section. If users wish to use different models, alter lines tagged with #*#

Urban.BMP.Efficiency.Curves <- Urban.BMP.Efficiency.Curves_blank %>%
  mutate(
    Coef.1 = case_when(
      BMP %in% c('Enhanced_Biofiltration_w_ISR') ~ 
        case_when(
          Pollutant == "N" ~ enhancedbiofiltrationwISR.N.model$coefficients[1],  #*#
          Pollutant == "P" ~ enhancedbiofiltrationwISR.P.model$coefficients[1] #*#
        ),
      BMP %in% c('Biofiltration_w_Underdrain', 'Bioretention_Basin') ~ 
        case_when(
          Pollutant == "N" ~ biofiltration.N.model$coefficients[1], #*#
          Pollutant == "P" ~ biofiltration.P.model$coefficients[1] #*#
        ),
      BMP %in% c('Extended_Dry_Detention_Basin', 'Grass_Swale_w_detention') ~ 
        case_when(
          Pollutant == "N" ~ 
            grassswalewdetention.N.model.quadratic$coefficients[1], #*#
          Pollutant == "P" ~ 
            grassswalewdetention.P.model.quadratic$coefficients[1] #*#
        ),
      BMP %in% c(
        'Gravel_Wetland', 
        'Surface_Constructed_Wetland', 
        'Subsurface_Gravel_Wetland'
      ) ~ case_when(
        Pollutant == "N" ~ gravelwetland.N.model$coefficients[1], #*#
        Pollutant == "P" ~ gravelwetland.P.model$coefficients[1] #*#
      ),
      BMP %in% c('Porous_Pavement_w_underdrain') ~ case_when(
        Pollutant == "N" ~ 
          porouspavementwunderdrain.N.model.linear$coefficients[1], #*#
        Pollutant == "P" ~ 
          porouspavementwunderdrain.P.model$coefficients[1] #*#
      ),
      BMP %in% c('Sand_Filter_w_underdrain', 'Wet_Pond') ~ case_when(
        Pollutant == "N" ~ wetpond.N.model$coefficients[1],  #*#
        Pollutant == "P" ~ wetpond.P.model$coefficients[1] #*#
      ),
      BMP %in% c('Infiltration_Basin') ~ case_when(
        Pollutant == "N" ~ 
          infiltrationbasin.n.models.all$coef1[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationbasin.p.models.all$coef1[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      ),
      BMP %in% c(
        'Infiltration_Trench', 
        'Porous_Pavement_w_subsurface_infiltration', 
        'Infiltration_Chamber'
      ) ~ case_when(
        Pollutant == "N" ~ 
          infiltrationtrench.n.models.all$coef1[
            match( #*#
              InfiltrationRate_inperhr, 
              infiltrationtrench.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationtrench.p.models.all$coef1[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      )
    ),
    Coef.2 = case_when(
      BMP %in% c('Enhanced_Biofiltration_w_ISR') ~ 
        case_when(
          Pollutant == "N" ~ enhancedbiofiltrationwISR.N.model$coefficients[2], #*#
          Pollutant == "P" ~ enhancedbiofiltrationwISR.P.model$coefficients[2] #*#
        ),
      BMP %in% c('Biofiltration_w_Underdrain', 'Bioretention_Basin') ~ 
        case_when(
          Pollutant == "N" ~ biofiltration.N.model$coefficients[2], #*#
          Pollutant == "P" ~ biofiltration.P.model$coefficients[2] #*#
        ),
      BMP %in% c('Extended_Dry_Detention_Basin', 'Grass_Swale_w_detention') ~ 
        case_when(
          Pollutant == "N" ~ 
            grassswalewdetention.N.model.quadratic$coefficients[2], #*#
          Pollutant == "P" ~ 
            grassswalewdetention.P.model.quadratic$coefficients[2] #*#
        ),
      BMP %in% c(
        'Gravel_Wetland', 
        'Surface_Constructed_Wetland', 
        'Subsurface_Gravel_Wetland'
      ) ~ case_when(
        Pollutant == "N" ~ gravelwetland.N.model$coefficients[2], #*#
        Pollutant == "P" ~ gravelwetland.P.model$coefficients[2] #*#
      ),
      BMP %in% c('Porous_Pavement_w_underdrain') ~ case_when(
        Pollutant == "N" ~ 
          porouspavementwunderdrain.N.model.linear$coefficients[2], #*#
        Pollutant == "P" ~ 
          porouspavementwunderdrain.P.model$coefficients[2] #*#
      ),
      BMP %in% c('Sand_Filter_w_underdrain', 'Wet_Pond') ~ case_when(
        Pollutant == "N" ~ wetpond.N.model$coefficients[2], #*#
        Pollutant == "P" ~ wetpond.P.model$coefficients[2] #*#
      ),
      BMP %in% c('Infiltration_Basin') ~ case_when(
        Pollutant == "N" ~ 
          infiltrationbasin.n.models.all$coef2[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationbasin.p.models.all$coef2[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      ),
      BMP %in% c(
        'Infiltration_Trench', 
        'Porous_Pavement_w_subsurface_infiltration', 
        'Infiltration_Chamber'
      ) ~ case_when(
        Pollutant == "N" ~ 
          infiltrationtrench.n.models.all$coef2[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationtrench.p.models.all$coef2[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      )
    ),
    Coef.3 = case_when(
      BMP %in% c('Extended_Dry_Detention_Basin', 'Grass_Swale_w_detention') ~ 
        case_when(
          Pollutant == "N" ~ 
            grassswalewdetention.N.model.quadratic$coefficients[3], #*#
          Pollutant == "P" ~ 
            grassswalewdetention.P.model.quadratic$coefficients[3] #*#
        ),
      BMP %in% c('Infiltration_Basin') ~ case_when(
        Pollutant == "N" ~ 
          infiltrationbasin.n.models.all$coef3[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationbasin.p.models.all$coef3[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      ),
      BMP %in% c(
        'Infiltration_Trench', 
        'Porous_Pavement_w_subsurface_infiltration', 
        'Infiltration_Chamber'
      ) ~ case_when(
        Pollutant == "N" ~ 
          infiltrationtrench.n.models.all$coef3[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationtrench.p.models.all$coef3[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      )
    ),
    Coef.1_se = case_when(
      BMP %in% c('Enhanced_Biofiltration_w_ISR') ~ 
        case_when(
          Pollutant == "N" ~ summary(enhancedbiofiltrationwISR.N.model)$coefficients[1,2],  #*#
          Pollutant == "P" ~ summary(enhancedbiofiltrationwISR.P.model)$coefficients[1,2] #*#
        ),
      BMP %in% c('Biofiltration_w_Underdrain', 'Bioretention_Basin') ~ 
        case_when(
          Pollutant == "N" ~ summary(biofiltration.N.model)$coefficients[1,2], #*#
          Pollutant == "P" ~ summary(biofiltration.P.model)$coefficients[1,2] #*#
        ),
      BMP %in% c('Extended_Dry_Detention_Basin', 'Grass_Swale_w_detention') ~ 
        case_when(
          Pollutant == "N" ~ 
            summary(grassswalewdetention.N.model.quadratic)$coefficients[1,2], #*#
          Pollutant == "P" ~ 
            summary(grassswalewdetention.P.model.quadratic)$coefficients[1,2] #*#
        ),
      BMP %in% c(
        'Gravel_Wetland', 
        'Surface_Constructed_Wetland', 
        'Subsurface_Gravel_Wetland'
      ) ~ case_when(
        Pollutant == "N" ~ summary(gravelwetland.N.model)$coefficients[1,2], #*#
        Pollutant == "P" ~ summary(gravelwetland.P.model)$coefficients[1,2] #*#
      ),
      BMP %in% c('Porous_Pavement_w_underdrain') ~ case_when(
        Pollutant == "N" ~ 
          summary(porouspavementwunderdrain.N.model.linear)$coefficients[1,2], #*#
        Pollutant == "P" ~ 
          summary(porouspavementwunderdrain.P.model)$coefficients[1,2] #*#
      ),
      BMP %in% c('Sand_Filter_w_underdrain', 'Wet_Pond') ~ case_when(
        Pollutant == "N" ~ summary(wetpond.N.model)$coefficients[1,2],  #*#
        Pollutant == "P" ~ summary(wetpond.P.model)$coefficients[1,2] #*#
      ),
      BMP %in% c('Infiltration_Basin') ~ case_when(
        Pollutant == "N" ~ 
          infiltrationbasin.n.models.all$coef1_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationbasin.p.models.all$coef1_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      ),
      BMP %in% c(
        'Infiltration_Trench', 
        'Porous_Pavement_w_subsurface_infiltration', 
        'Infiltration_Chamber'
      ) ~ case_when(
        Pollutant == "N" ~ 
          infiltrationtrench.n.models.all$coef1_se[
            match( #*#
              InfiltrationRate_inperhr, 
              infiltrationtrench.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationtrench.p.models.all$coef1_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      )
    ),
    Coef.2_se = case_when(
      BMP %in% c('Enhanced_Biofiltration_w_ISR') ~ 
        case_when(
          Pollutant == "N" ~ summary(enhancedbiofiltrationwISR.N.model)$coefficients[2,2], #*#
          Pollutant == "P" ~ summary(enhancedbiofiltrationwISR.P.model)$coefficients[2,2] #*#
        ),
      BMP %in% c('Biofiltration_w_Underdrain', 'Bioretention_Basin') ~ 
        case_when(
          Pollutant == "N" ~ summary(biofiltration.N.model)$coefficients[2,2], #*#
          Pollutant == "P" ~ summary(biofiltration.P.model)$coefficients[2,2] #*#
        ),
      BMP %in% c('Extended_Dry_Detention_Basin', 'Grass_Swale_w_detention') ~ 
        case_when(
          Pollutant == "N" ~ 
            summary(grassswalewdetention.N.model.quadratic)$coefficients[2,2], #*#
          Pollutant == "P" ~ 
            summary(grassswalewdetention.P.model.quadratic)$coefficients[2,2] #*#
        ),
      BMP %in% c(
        'Gravel_Wetland', 
        'Surface_Constructed_Wetland', 
        'Subsurface_Gravel_Wetland'
      ) ~ case_when(
        Pollutant == "N" ~ summary(gravelwetland.N.model)$coefficients[2,2], #*#
        Pollutant == "P" ~ summary(gravelwetland.P.model)$coefficients[2,2] #*#
      ),
      BMP %in% c('Porous_Pavement_w_underdrain') ~ case_when(
        Pollutant == "N" ~ 
          summary(porouspavementwunderdrain.N.model.linear)$coefficients[2,2], #*#
        Pollutant == "P" ~ 
          summary(porouspavementwunderdrain.P.model)$coefficients[2,2] #*#
      ),
      BMP %in% c('Sand_Filter_w_underdrain', 'Wet_Pond') ~ case_when(
        Pollutant == "N" ~ summary(wetpond.N.model)$coefficients[2,2], #*#
        Pollutant == "P" ~ summary(wetpond.P.model)$coefficients[2,2] #*#
      ),
      BMP %in% c('Infiltration_Basin') ~ case_when(
        Pollutant == "N" ~ 
          infiltrationbasin.n.models.all$coef2_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationbasin.p.models.all$coef2_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      ),
      BMP %in% c(
        'Infiltration_Trench', 
        'Porous_Pavement_w_subsurface_infiltration', 
        'Infiltration_Chamber'
      ) ~ case_when(
        Pollutant == "N" ~ 
          infiltrationtrench.n.models.all$coef2_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationtrench.p.models.all$coef2_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      )
    ),
    Coef.3_se = case_when(
      BMP %in% c('Extended_Dry_Detention_Basin', 'Grass_Swale_w_detention') ~ 
        case_when(
          Pollutant == "N" ~ 
            summary(grassswalewdetention.N.model.quadratic)$coefficients[3,2], #*#
          Pollutant == "P" ~ 
            summary(grassswalewdetention.P.model.quadratic)$coefficients[3,2] #*#
        ),
      BMP %in% c('Infiltration_Basin') ~ case_when(
        Pollutant == "N" ~ 
          infiltrationbasin.n.models.all$coef3_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationbasin.p.models.all$coef3_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationbasin.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      ),
      BMP %in% c(
        'Infiltration_Trench', 
        'Porous_Pavement_w_subsurface_infiltration', 
        'Infiltration_Chamber'
      ) ~ case_when(
        Pollutant == "N" ~ 
          infiltrationtrench.n.models.all$coef3_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.n.models.all$InfiltrationRate_inperhr #*#
            )
          ], 
        Pollutant == "P" ~ 
          infiltrationtrench.p.models.all$coef3_se[ #*#
            match(
              InfiltrationRate_inperhr, 
              infiltrationtrench.p.models.all$InfiltrationRate_inperhr #*#
            )
          ]
      )
    ),
    Best.Fit.Curve = case_when( #*# # This case_when() sequence assigns BMPs the models we chose. Users may wish to change the assignment of model structures to various BMPs.
      BMP %in% c( 
        'Enhanced_Biofiltration_w_ISR', 
        'Biofiltration_w_Underdrain', 
        'Bioretention_Basin',
        'Gravel_Wetland', 
        'Surface_Constructed_Wetland', 
        'Subsurface_Gravel_Wetland', 
        'Sand_Filter_w_underdrain', 
        'Wet_Pond'
      ) ~ 
        'y ~ Coef.1 + Coef.2 * log( x )',
      BMP %in% c('Extended_Dry_Detention_Basin', 'Grass_Swale_w_detention') ~
        'y ~ Coef.1 + Coef.2 * ( x ^2) + Coef.3 * x ',
      BMP %in% c('Porous_Pavement_w_underdrain') ~ case_when(
        Pollutant == "N" ~ 'y ~ Coef.1 + Coef.2 * x ', 
        Pollutant == "P" ~ 'y ~ Coef.1 + Coef.2 * log( x )'
      ),
      BMP %in% c(
        'Infiltration_Basin', 
        'Infiltration_Trench', 
        'Porous_Pavement_w_subsurface_infiltration', 
        'Infiltration_Chamber'
      ) ~ 'y ~ Coef.1 + Coef.2 * (1 - exp(Coef.3 * x ))'
    )
  )

Urban.BMP.Efficiency.Curves

##################################################
# 7. Write BMP Performance Curves to csv ####
##################################################


write_csv(
  Urban.BMP.Efficiency.Curves, 
  file = "./RBEROST-Northeast/Preprocessing/Inputs/UrbanBMPPerformanceCurves.csv"
)


