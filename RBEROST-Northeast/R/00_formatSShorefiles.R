# formatSShorefiles.R
# N. Detenbeck
# 12/1/2025

library(readr)
library(dplyr)
library(bit64)

# ne_dynamic_sparrow_se_tn
ne_dynamic_sparrow_se_tn_CTC <- read_csv("RBEROST-Northeast/Preprocessing/Inputs/ne_dynamic_sparrow_se_tn_CTC.csv") %>%
  mutate(comid_time = as.integer64(comid_time))
View(ne_dynamic_sparrow_se_tn_CTC)
head(ne_dynamic_sparrow_se_tn_CTC)
# Get column names
variable_names <- names(ne_dynamic_sparrow_se_tn_CTC)
print(variable_names)
ne_dynamic_sparrow_se_tn <- select(ne_dynamic_sparrow_se_tn_CTC,-1)
head(ne_dynamic_sparrow_se_tn)
write.csv(ne_dynamic_sparrow_se_tn, "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Preprocessing/Inputs/ne_dynamic_sparrow_se_tn.csv", row.names = FALSE)

# Predict_NoBFlowN_WSeptic
Predict_NoBFlowN_WSeptic_CTC <- read_csv("RBEROST-Northeast/Preprocessing/Inputs/Predict_NoBFlowN_WSeptic_CTC.csv") %>%
  mutate(comid_time = as.integer64(comid_time))
View(Predict_NoBFlowN_WSeptic_CTC)
head(Predict_NoBFlowN_WSeptic_CTC)
# Get column names
variable_names <- names(Predict_NoBFlowN_WSeptic_CTC)
print(variable_names)
Predict_NoBFlowN_WSeptic <- select(Predict_NoBFlowN_WSeptic_CTC,-1)
head(Predict_NoBFlowN_WSeptic)
write.csv(Predict_NoBFlowN_WSeptic, "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Preprocessing/Inputs/Predict_NoBFlowN_WSeptic.csv", row.names = FALSE)

# Predict_P_CTC
Predict_P_CTC <- read_csv("RBEROST-Northeast/Preprocessing/Inputs/Predict_P_CTC.csv") %>%
  mutate(comid_time = as.integer64(comid_time))
View(Predict_P_CTC)
head(Predict_P_CTC)
# Get column names
variable_names <- names(Predict_P_CTC)
print(variable_names)
Predict_P <- select(Predict_P_CTC,-1)
head(Predict_P)
write.csv(Predict_P, "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Preprocessing/Inputs/Predict_P.csv", row.names = FALSE)

# ne_sparrow_model_input_CTC
ne_sparrow_model_input_CTC <- read_csv("RBEROST-Northeast/Preprocessing/Inputs/ne_sparrow_model_input_CTC.csv")
View(ne_sparrow_model_input_CTC)
head(ne_sparrow_model_input_CTC)
# Get column names
variable_names <- names(ne_sparrow_model_input_CTC)
print(variable_names)
ne_sparrow_model_input <- select(ne_sparrow_model_input_CTC,-1)
head(ne_sparrow_model_input)
write.csv(ne_sparrow_model_input, "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Preprocessing/Inputs/ne_sparrow_model_input.csv", row.names = FALSE)
