#######################################################################################################
# PURPOSE: Format Data to store on EDM
# BY: Cathy Chamberlin (ORISE-EPA)
# DATE: 2021-02-25
#######################################################################################################
InPath <- "./RBEROST-Northeast/Preprocessing/Inputs/"
OutPath <- "./RBEROST-Northeast/Preprocessing/Outputs/"
horizon <- 15
AgBMPcomparison <- "No Practice"
interest_rate <- 0.03
n.scenarios <- 3 
scenariostepchange <- 0.01

source(
  "./RBEROST-Northeast/R/01_Optimization_Preprocessing+Uncertainty-Northeast.R", 
  local = TRUE
  )
source("./RBEROST-Northeast/R/00_ProcessNdepdata-Northeast.R")

library(janitor)

## Format Ag Efficiency Data in Long form to keep on EDM ####
ag_effic_bycomid_NoPrac <- full_join(
  full_join(
  ag_effic_bycomid_tn,
  ag_effic_bycomid_tp,
  by = "comid",
  suffix = c(".TN", ".TP")
),
full_join(
  ag_effic_bycomid_tn_se,
  ag_effic_bycomid_tp_se,
  by = "comid",
  suffix = c(".TN", ".TP")
),
by = "comid",
suffix = c(".estimate", ".se")
) %>%
  pivot_longer(
    cols = -comid,
    names_to = c("BMP", "Nutrient", "Value_Type"),
    names_pattern = "(.*)[.](..)[.](.*)",
    values_to = "Value"
  ) %>%
  mutate(
    Parameter = "EfficiencyvsNoPractice_fracremoval",
    BMP_Category = "ag",
    Value_Type = case_when(
      Value_Type == "estimate" ~ "", Value_Type == "se" ~ "se"
      )
    ) %>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  left_join(
    ., COMID_State, by = "comid"
  ) %>%
  select(
    comid,
    hydseq,
    cfromnode,
    ctonode,
    State,
    BMP_Category,
    BMP,
    Nutrient,
    Parameter,
    Value_Type,
    Value
  )

AgBMPcomparison <- "Baseline"
source(
  "./RBEROST-Northeast/R/01_Optimization_Preprocessing+Uncertainty-Northeast.R", 
  local = TRUE
  )

ag_effic_bycomid_Baseline <- full_join(
  full_join(
  ag_effic_bycomid_tn,
  ag_effic_bycomid_tp,
  by = "comid",
  suffix = c(".TN", ".TP")
),
full_join(
  ag_effic_bycomid_tn_se,
  ag_effic_bycomid_tp_se,
  by = "comid",
  suffix = c(".TN", ".TP")
),
by = "comid",
suffix = c(".estimate", ".se")
) %>%
  pivot_longer(
    cols = -comid,
    names_to = c("BMP", "Nutrient", "Value_Type"),
    names_pattern = "(.*)[.](..)[.](.*)",
    values_to = "Value"
  ) %>%
  mutate(
    Parameter = "EfficiencyvsBaseline_fracremoval",
    BMP_Category = "ag",
    Value_Type = case_when(
      Value_Type == "estimate" ~ "", Value_Type == "se" ~ "se"
      )
    ) %>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  left_join(
    ., COMID_State, by = "comid"
  ) %>%
  select(
    comid,
    hydseq,
    cfromnode,
    ctonode,
    State,
    BMP_Category,
    BMP,
    Nutrient,
    Parameter,
    Value_Type,
    Value
  )

ag_effic_bycomid <- rbind(ag_effic_bycomid_NoPrac, ag_effic_bycomid_Baseline)

## Format Existing Buffer extent Data in long form to keep on EDM ####
existing_buffers <- riparian.existingbuffer %>%
  select(-V1, - totalbanklength_ft) %>%
  pivot_longer(
    cols = c(-comid, -totalbanklength_ft_se),
    names_to = c("BMP", "Approx_DesignSpec_Value"),
    names_pattern = "_?(.*_*)_([0-9]+)ft_ft",
    values_to = "Value"
  ) %>%
  mutate(
    Parameter = "ExistingLength_ft",
    BMP_Category = "ripbuf",
    Approx_DesignSpec_Units = "Width_ft",
    Year = "2016", 
    LENGTHKM_se = totalbanklength_ft_se * 0.0003048, #km/ft
    Value_Type = case_when(grepl("_se", BMP) ~ "se", TRUE ~ ""),
    BMP = gsub("_se", "", BMP)
  ) %>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  left_join(
    ., COMID_State, by = "comid"
  ) %>%
  select(
    comid,
    LENGTHKM_se,
    hydseq,
    cfromnode,
    ctonode,
    State,
    BMP_Category,
    BMP,
    Approx_DesignSpec_Value,
    Approx_DesignSpec_Units,
    Year,
    Parameter,
    Value_Type,
    Value
  )

## Format Ndeposition data by comid to keep on EDM ####
NdepChange <- as.data.frame(ndep_postarcgis) %>%
  select(comid = FEATUREID, ndep_2012 = ndep_2012_, ndep_2019 = ndep_2019_) %>%
  pivot_longer(
    cols = -comid,
    names_to = "Year",
    names_pattern = "([0-9]+)",
    values_to = "Value"
  )%>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  left_join(
    ., COMID_State, by = "comid"
  ) %>%
  mutate(Parameter = "AtmosphericDep_kgha-1", Nutrient = "TN") %>%
  select(
    comid, hydseq, cfromnode, ctonode, State, Nutrient, Year, Parameter, Value
  )


## Format urban efficiency curves by comid to keep on EDM ####
urban_effic <- merge(
  urban.BMPs.list, 
  urban.effic.curves %>% rename(Nutrient = Pollutant), 
  by = "BMP"
) %>%
  mutate(
    Parameter = "Efficiency_fracremoval_asfuncofDesignSpec",
    Nutrient = paste0("T", Nutrient)
  ) %>%
  rowwise() %>%
  mutate(
    Estimate = gsub(
      "Coef.1", Coef.1, gsub(
        'Coef.2', Coef.2, gsub('Coef.3', Coef.3, Best.Fit.Curve)
      )
    ),
    se = case_when(
      Best.Fit.Curve == "y ~ Coef.1 + Coef.2 * log( x )" ~ paste0(
        "y ~ sqrt(", Coef.1_se, " ^ 2 + ((",  Coef.2,  " * log( x )) *",
        case_when(
          Coef.2 != 0 ~ paste0(
            "sqrt((", Coef.2_se, " / ", Coef.2, ") ^ 2 + (0) ^ 2)"
          ),
          Coef.2 == 0 ~ "0"
        ),
        ")^ 2)"
      ),
      Best.Fit.Curve == "y ~ Coef.1 + Coef.2 * ( x ^2) + Coef.3 * x" ~ paste0(
        "y ~ sqrt(", Coef.1_se, "^ 2 + ((", Coef.2, "* ( x ^2)) * ",
        case_when(
          Coef.2 != 0 ~ paste0(
            "sqrt((", Coef.2_se, "/", Coef.2, ") ^ 2 + (0) ^ 2)"
          ),
          Coef.2 == 0 ~ "0"
        ),
        ")^ 2 + ((", Coef.3, "* x) * ",
        case_when(
          Coef.3 != 0 ~ paste0(
            "sqrt((", Coef.3_se, "/ ", Coef.3, ") ^ 2 + (0) ^ 2)"
          ),
          Coef.3 == 0 ~ "0"
        ),
        ")^ 2)"
      ),
      Best.Fit.Curve == "y ~ Coef.1 + Coef.2 * x" ~ paste0(
        "y ~ sqrt(", Coef.1_se, "^ 2 + ((", Coef.2, "* x) * ",
        case_when(
          Coef.2 != 0 ~ paste0(
            "sqrt((", Coef.2_se, " / ", Coef.2, ") ^ 2 + 0 ^ 2)"
          ), 
          Coef.2 == 0 ~ "0"
        ),
        ") ^ 2)"
      ),
      Best.Fit.Curve == "y ~ Coef.1 + Coef.2 * (1 - exp(Coef.3 * x ))" ~ paste0(
        "y ~ sqrt(",  Coef.1_se,  " ^ 2 + (", 
        case_when(
          Coef.2 != 0 & Coef.3 != 0 ~ paste0(
            Coef.2, 
            " * (1 - exp(", 
            Coef.3, 
            " * x )) * sqrt((", 
            Coef.2_se, 
            " / ", 
            Coef.2, 
            ") ^ 2 + (sqrt(0 ^ 2 + (exp(",
            Coef.3,
            " * x) * x * ",
            Coef.3_se, 
            ") ^ 2) / (1 - exp(",
            Coef.3,
            "* x ))) ^ 2)"
          ), 
          Coef.2 == 0 ~ "0", 
          Coef.3 == 0 ~ "0"
        ), 
        ") ^ 2)"
      )
    )
  ) %>%
  pivot_longer(
    cols = c(Estimate, se), names_to = "Value_Type", values_to = "Value"
  ) %>%
  mutate(
    Value_Type = case_when(
      Value_Type == "Estimate" ~ "", Value_Type == "se" ~ "se"
    )
  ) %>%
  select(
    BMP_Category, BMP, Nutrient, Parameter, Value_Type, Value, InfiltrationRate_inperhr
  ) %>%
  pivot_wider(
    id_cols = c(InfiltrationRate_inperhr, Nutrient, BMP_Category, Parameter, Value_Type),
    names_from = BMP,
    values_from = Value
  ) %>%
  add_row(
    InfiltrationRate_inperhr = 0, 
    Nutrient = c("TN", "TN", "TP", "TP"), 
    BMP_Category = "urban",
    Parameter = "Efficiency_fracremoval_asfuncofDesignSpec",
    Value_Type = c("", "se", "", "se")
  ) %>%
  mutate(
    across(
      any_of(
        c(
          "Infiltration_Basin",
          "Infiltration_Chamber",
          "Infiltration_Trench",
          "Porous_Pavement_w_subsurface_infiltration"
        )
      ),
      ~ case_when(InfiltrationRate_inperhr == 0 ~ " y ~ 0", TRUE ~ .) # Infiltration BMPs should not be used in subcatchments with very low infiltration rates (predominately HSG D). Not only is this unreasonable theoretically, we also do not have an efficiency curve to match these low infiltration rates. The -999 will force the model not to implement these BMPs in areas that have low infiltration rates. There is an additional check, that the max implementation of these BMPs in these comids will be set to 0.
    )
  ) %>%
  group_by(Nutrient, Value_Type) %>%
  fill(all_of(Urban_BMPs))

urban_effic.n <- urban_effic[which(urban_effic$Value_Type == "" & urban_effic$Nutrient == "TN"),]
urban_effic.p <- urban_effic[which(urban_effic$Value_Type == "" & urban_effic$Nutrient == "TP"),]


urban_effic_bycomid <- urban_effic %>%
  right_join(
    ., comid.infiltrationrates.matched %>% select(-V1),
    by = "InfiltrationRate_inperhr"
  ) %>%
  rowwise() %>%
  mutate(
    across(
      .cols = any_of(
        c(
          "Infiltration_Basin",
          "Infiltration_Chamber",
          "Infiltration_Trench",
          "Porous_Pavement_w_subsurface_infiltration"
        )
      ),
      .fns = ~ (
        case_when(
          Value_Type == "" ~ ., 
          Value_Type == "se" ~ case_when(
            Nutrient == "TN" ~ paste0(
              "c('", 
              paste(
                c(
                  (urban_effic.n[[cur_column()]][
                    unlist(
                      lapply(
                        X = InfiltrationRate_inperhr_dist,
                        FUN = match,
                        table = urban_effic.n$InfiltrationRate_inperhr
                      )
                    )
                  ]
                  )
                ),
                collapse = "', '"
              ),
              "')"
            ),
            Nutrient == "TP" ~ paste0(
              "c('", 
              paste(
                c(
                  (urban_effic.p[[cur_column()]][
                    unlist(
                      lapply(
                        X = InfiltrationRate_inperhr_dist,
                        FUN = match,
                        table = urban_effic.p$InfiltrationRate_inperhr
                      )
                    )
                  ]
                  )
                ),
                collapse = "', '"
              ),
              "')"
            )
          )
        )
      )
    )
  )  %>%
  ungroup() %>%
  pivot_longer(
    cols = all_of(bmp_urban_vec_direct),
    names_to = "BMP",
    values_to = "Value"
  ) %>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  left_join(
    ., COMID_State, by = "comid"
  ) %>%
  select(
    comid,
    hydseq,
    cfromnode,
    ctonode,
    State,
    BMP_Category,
    BMP,
    Nutrient,
    Parameter,
    Value_Type,
    Value
  )

## Format riparian efficiency curves for EDM ####

riparian_buffer_efficiencies_bycomid <- riparian.efficiencies %>%
  select(-V1) %>%
  pivot_longer(
    cols = -comid,
    names_to = c("Nutrient", "BMP", "Approx_DesignSpec_Value"),
    names_pattern = "([A-Z])_(.*)_([0-9]+)",
    values_to = "Value"
  ) %>%
  mutate(
    Value_Type = case_when(grepl("c[(]", Value) ~ "se", TRUE ~ ""),
    BMP_Category = "ripbuf",
    BMP = paste0(BMP, "ed_Buffer"),
    Nutrient = paste0("T", Nutrient),
    Approx_DesignSpec_Units = "Width_ft",
    Parameter = "Efficiency_fracremoval_asfuncofDesignSpec",
    Approx_DesignSpec_Units = "Width_ft",
  ) %>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  left_join(
    ., COMID_State, by = "comid"
  ) %>%
  select(
    comid,
    hydseq,
    cfromnode,
    ctonode,
    State,
    BMP_Category,
    BMP,
    Approx_DesignSpec_Value,
    Approx_DesignSpec_Units,
    Nutrient,
    Parameter,
    Value_Type,
    Value
  )

## Format riparian loading data ####
riparian_loading_bycomid <- riparian.loadings %>%
  select(-V1) %>%
  pivot_longer(
    cols = -comid, 
    names_to = c("Nutrient", "Value_Type"), 
    names_sep = "_riparian_kgyr",
    values_to = "Value"
  ) %>%
  mutate(
    Nutrient = paste0("T", substr(Nutrient, 1, 1)),
    Value_Type = case_when(
      Value_Type == "_se" ~ "se", 
      Value_Type == "" ~ ""
      ),
    Parameter = "RiparianLoading_kgyr-1",
    Year = "2016"
  ) %>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  left_join(
    ., COMID_State, by = "comid"
  ) %>%
  select(
    comid,
    hydseq,
    cfromnode,
    ctonode,
    State,
    Nutrient,
    Year,
    Parameter,
    Value_Type,
    Value
  )

## Format WWTP data ####
 
wwtp_bycomid <- wwtp_rem_uncertain %>%
  mutate(BMP_Category = "point", Nutrient = "TN") %>%
  pivot_longer(
    cols = c(
      load_kgyr_2014, load_kgyr_2018, load_kgyr_2014_se, load_kgyr_2018_se
      ),
    names_to = c("Parameter", "Year", "Value_Type"),
    names_pattern = "(.*_*)_([0-9]+)(.*)",
    values_to = "Value"
  ) %>%
  mutate(
    Parameter = paste0(Parameter, "-1"), Value_Type = gsub("_", "", Value_Type)
    ) %>%
  full_join(
    .,
    point_effic %>%
      pivot_longer(
        cols = contains("Efficiency"),
        names_to = c("Nutrient", "Parameter"),
        names_pattern = "(.*)_(.*)",
        values_to = "Value"
      ) %>%
      mutate(
        Plant_Name = bmp,
        Nutrient = paste0("T", Nutrient),
        Parameter = paste0(Parameter, "_fracremoval"),
        Value = case_when(Value == -999 ~ NA_real_, Value != -999 ~ Value)
      ),
    by = c(
      "comid",
      "State",
      "NPDES_ID",
      "BMP_Category",
      "Plant_Name",
      "Parameter",
      "Value",
      "Nutrient"
    )
  ) %>%
  mutate(BMP_Category = "point") %>%
  left_join(
    ., sparrow_in %>% select(comid, cfromnode, ctonode, hydseq), by = "comid"
  ) %>%
  select(
    comid,
    hydseq,
    cfromnode,
    ctonode,
    State,
    BMP_Category,
    Plant_Name,
    NPDES_ID,
    Nutrient,
    Year,
    Parameter,
    Value_Type,
    Value
  )

## Format Cost Data & Un-annualize capital costs ####

bmp_costs_se <- ag_costs_yearly %>%
  pivot_longer(
    cols = -c(BMP_Category, BMP, capital_units, operations_units), 
    names_to = c("costtype", "State", "Year"), 
    names_sep = "_", 
    values_to = "Value"
  ) %>%
  group_by(
    BMP_Category, BMP, State, costtype, capital_units, operations_units
  ) %>%
  summarize(mean = mean(Value), se = (sd(Value) / sqrt(4)), .groups = "keep") %>%
  pivot_wider(
    id_cols = c(BMP_Category, BMP, capital_units, operations_units), 
    names_from = c(costtype, State), 
    values_from = se
  ) %>%
  ungroup()

## adjust ag & riparian units ####

temp_bmp_costs <- merge(
  user_specs_BMPs[
    user_specs_BMPs$BMP_Selection=="X",
    c(
      "BMP_Category",
      "BMP",
      "capital_VT",
      "capital_NH",
      "operations_VT",
      "operations_NH", 
      "capital_units",
      "operations_units",
      "UserSpec_RD_in"
    )
  ],
  bmp_costs_se,
  by = c("BMP_Category", "BMP", "capital_units", "operations_units"),
  suffixes = c("_Estimate", "_se"),
  all = TRUE
)


# Convert area-specific costs to costs per acre for ag BMPs and to costs per sq foot for Riparian BMPs

temp_bmp_costs_rev <- temp_bmp_costs %>%
  mutate(
    capital_VT_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ capital_VT_Estimate * ft2_to_ac,
        capital_units == "km2" ~ capital_VT_Estimate * km2_to_ac,
        capital_units == "yd2" ~ capital_VT_Estimate * yd2_to_ac,
        capital_units == "ac" ~ capital_VT_Estimate
      ),
      BMP_Category == "urban" ~ capital_VT_Estimate,
      BMP_Category == "point" ~ capital_VT_Estimate,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ capital_VT_Estimate,
        capital_units == "km2" ~ capital_VT_Estimate * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ capital_VT_Estimate * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ capital_VT_Estimate / ft2_to_ac
      )
    ), 
    capital_VT_se_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ capital_VT_se * ft2_to_ac,
        capital_units == "km2" ~ capital_VT_se * km2_to_ac,
        capital_units == "yd2" ~ capital_VT_se * yd2_to_ac,
        capital_units == "ac" ~ capital_VT_se
      ),
      BMP_Category == "urban" ~ capital_VT_se,
      BMP_Category == "point" ~ capital_VT_se,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ capital_VT_se,
        capital_units == "km2" ~ capital_VT_se * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ capital_VT_se * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ capital_VT_se / ft2_to_ac
      )
    ), 
    capital_NH_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ capital_NH_Estimate * ft2_to_ac,
        capital_units == "km2" ~ capital_NH_Estimate * km2_to_ac,
        capital_units == "yd2" ~ capital_NH_Estimate * yd2_to_ac,
        capital_units == "ac" ~ capital_NH_Estimate
      ),
      BMP_Category == "urban" ~ capital_NH_Estimate,
      BMP_Category == "point" ~ capital_NH_Estimate,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ capital_NH_Estimate,
        capital_units == "km2" ~ capital_NH_Estimate * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ capital_NH_Estimate * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ capital_NH_Estimate / ft2_to_ac
      )
    ), 
    capital_NH_se_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ capital_NH_se * ft2_to_ac,
        capital_units == "km2" ~ capital_NH_se * km2_to_ac,
        capital_units == "yd2" ~ capital_NH_se * yd2_to_ac,
        capital_units == "ac" ~ capital_NH_se
      ),
      BMP_Category == "urban" ~ capital_NH_se,
      BMP_Category == "point" ~ capital_NH_se,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ capital_NH_se,
        capital_units == "km2" ~ capital_NH_se * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ capital_NH_se * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ capital_NH_se / ft2_to_ac
      )
    ), 
    operations_VT_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ operations_VT_Estimate * ft2_to_ac,
        capital_units == "km2" ~ operations_VT_Estimate * km2_to_ac,
        capital_units == "yd2" ~ operations_VT_Estimate * yd2_to_ac,
        capital_units == "ac" ~ operations_VT_Estimate
      ),
      BMP_Category == "urban" ~ operations_VT_Estimate,
      BMP_Category == "point" ~ operations_VT_Estimate,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ operations_VT_Estimate,
        capital_units == "km2" ~ operations_VT_Estimate * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ operations_VT_Estimate * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ operations_VT_Estimate / ft2_to_ac
      )
    ), 
    operations_VT_se_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ operations_VT_se * ft2_to_ac,
        capital_units == "km2" ~ operations_VT_se * km2_to_ac,
        capital_units == "yd2" ~ operations_VT_se * yd2_to_ac,
        capital_units == "ac" ~ operations_VT_se
      ),
      BMP_Category == "urban" ~ operations_VT_se,
      BMP_Category == "point" ~ operations_VT_se,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ operations_VT_se,
        capital_units == "km2" ~ operations_VT_se * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ operations_VT_se * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ operations_VT_se / ft2_to_ac
      )
    ), 
    operations_NH_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ operations_NH_Estimate * ft2_to_ac,
        capital_units == "km2" ~ operations_NH_Estimate * km2_to_ac,
        capital_units == "yd2" ~ operations_NH_Estimate * yd2_to_ac,
        capital_units == "ac" ~ operations_NH_Estimate
      ),
      BMP_Category == "urban" ~ operations_NH_Estimate,
      BMP_Category == "point" ~ operations_NH_Estimate,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ operations_NH_Estimate,
        capital_units == "km2" ~ operations_NH_Estimate * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ operations_NH_Estimate * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ operations_NH_Estimate / ft2_to_ac
      )
    ),
    operations_NH_se_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ operations_NH_se * ft2_to_ac,
        capital_units == "km2" ~ operations_NH_se * km2_to_ac,
        capital_units == "yd2" ~ operations_NH_se * yd2_to_ac,
        capital_units == "ac" ~ operations_NH_se
      ),
      BMP_Category == "urban" ~ operations_NH_se,
      BMP_Category == "point" ~ operations_NH_se,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ operations_NH_se,
        capital_units == "km2" ~ operations_NH_se * km2_to_ac / ft2_to_ac,
        capital_units == "yd2" ~ operations_NH_se * yd2_to_ac / ft2_to_ac,
        capital_units == "ac" ~ operations_NH_se / ft2_to_ac
      )
    )
  )

bmp_costs <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      "capital_VT_rev",
      "capital_NH_rev",
      "operations_VT_rev",
      "operations_NH_rev"
    )
  )

bmp_costs_se <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      "capital_VT_se_rev",
      "capital_NH_se_rev",
      "operations_VT_se_rev",
      "operations_NH_se_rev"
    )
  )
names(bmp_costs) <- c(
  "category","bmp","capital_VT","capital_NH","operations_VT","operations_NH"
)
names(bmp_costs_se) <- c(
  "category","bmp","capital_VT","capital_NH","operations_VT","operations_NH"
)


temp_bmp_costs_point <- merge(
  bmp_costs[bmp_costs$category =="point",], 
  point_comid,
  by = "bmp",
  all.x = TRUE
)
temp_bmp_costs_point$capital <- with(
  temp_bmp_costs_point, ifelse(State == "NH", capital_NH, capital_VT)
) #*#
temp_bmp_costs_point$operations <- with(
  temp_bmp_costs_point, ifelse(State == "NH", operations_NH, operations_VT)
) #*#
bmp_costs_point <- temp_bmp_costs_point %>% 
  select(c("category", "comid", "capital", "operations", "State"))

ag_ripbuf_costs_1 <- left_join(
  bmp_costs_se, 
  bmp_costs, 
  by = c("category", "bmp"),
  suffix = c("_se", "")
  ) %>%
  filter(category %in% c("ag", "ripbuf")) %>%
  select(Category = category, BMP = bmp, contains("Capital"), contains("Operations")) %>%
  pivot_longer(
    cols = c(contains("Capital"), contains("Operations")),
    names_to = c("Parameter", "State", "Value_Type"), 
    names_pattern = "(.*)_([A-Z]+)(.*)",
    values_to = "Value"
    ) %>%
  mutate(
    Value_Type = gsub("_", "", Value_Type),
    Parameter = paste0(
      Parameter, 
      "Costs_2019dollarsper", 
      case_when(
        Category == "ag" ~ "ac", 
        Category == "ripbuf" ~ "ft2"
        ),
      case_when(
        Parameter == "capital" ~ "",
        Parameter == "operations" ~ "peryear"
      )
      )
    ) %>% 
  select(Category, BMP, Parameter, State, Value_Type, Value)

urban_costs_1 <- bmp_costs %>%
  filter(category == "urban") %>%
  pivot_longer(
   cols = c(contains("Capital"), contains("Operations")),
    names_to = c("Parameter", "State"), 
    names_pattern = "(.*)_(.*)", 
    values_to = "Value"
    ) %>%
  expand_grid(., Value_Type = c("", "se")) %>%
  mutate(
    Value = case_when(Value_Type == "se" ~ 0, Value_Type == "" ~ Value)
    ) %>% # Variability in urban costs is accounted for in the urban adjustment coefficient, not in the base costs
  mutate(
    Parameter = paste0(
      Parameter, 
      case_when(
        Parameter == "capital" ~ "Costs_2019dollarsperft3", 
        Parameter == "operations" ~ "Costs_2019dollarsperft3peryear"
        )
      )
    ) %>% 
  select(Category = category, BMP = bmp, State, Parameter, Value_Type, Value) 

state.cost.table <- rbind(ag_ripbuf_costs_1, urban_costs_1) %>%
  rename(BMP_Category = Category) %>%
  pivot_wider(
    id_cols = "State",
    names_from = c(
      "BMP_Category", 
      "BMP", 
      "Parameter", 
      "Value_Type"
    ), 
    values_from = "Value"
  )

# Point costs

# Format point source BMP costs data
temp_point_costs_dat <- bmp_costs_point[
  bmp_costs_point$comid %in% streamcat_subset_all$comid,
] %>% 
  select(c("comid", "State", "capital", "operations"))
temp_point_costs_other <- as.data.frame(
  streamcat_subset_all[
    !(streamcat_subset_all$comid %in% temp_point_costs_dat$comid),
    ]
)
names(temp_point_costs_other) <- "comid"
temp_point_costs_other_rev <- merge(
  temp_point_costs_other, COMID_State, by = c("comid"), all.x = TRUE
)
temp_point_costs_other_rev$capital <- 0
temp_point_costs_other_rev$operations <- 0
names(temp_point_costs_other_rev) <- c(
  "comid", "State", "capital", "operations"
)
temp_point_costs_dat_rev <- rbind(
  temp_point_costs_dat, temp_point_costs_other_rev
)
temp_point_costs_dat_rev <- temp_point_costs_dat_rev[
  order(temp_point_costs_dat_rev$comid),
]
temp_point_costs_dat_rev$comid_form <- paste0(
  "'", temp_point_costs_dat_rev$comid, "'"
)
point_costs_dat <- temp_point_costs_dat_rev %>% 
  select(c("comid_form", "capital", "operations"))
  
point_costs_1 <- point_costs_dat %>%
  mutate(
    capital_se = capital * 0.15, 
    operations_se = operations * 0.15,
    comid_form = as.numeric(gsub("'", "", comid_form))
    ) %>% # Use a 15% coefficient of variation, in line with methods from JJ Environmental (2015)
  rename(
    point_capitalCost_2019dollars = capital,
    point_capitalCost_2019dollars_se = capital_se,
    point_operationsCost_2019dollarsperyear = operations,
    point_operationsCost_2019dollarsperyear_se = operations_se,
    comid = comid_form
    ) 

urban_cost_coefs_1 <- full_join(
  urban_cost_coeffs_dat, urban_cost_coeffs_dat_se, by = "comid_form"
  ) %>%
  mutate(comid_form = as.numeric(gsub("'", "", comid_form))) %>%
  rename(
    urban_costadjustment_frac = urban_cost_coef,
    urban_costadjustment_frac_se = urban_cost_coef_se,
    comid = comid_form
  )

cost.table <- merge(point_costs_1, urban_cost_coefs_1, by = "comid")
## View Tables ####

# View(ag_effic_bycomid)
# View(existing_buffers)
# View(NdepChange)
# View(urban_effic_bycomid)
# View(riparian_buffer_efficiencies_bycomid)
# View(riparian_loading_bycomid)
# View(wwtp_bycomid)

## Write One Large CSV file ####
numeric.workeddata <- merge(
  x = merge(
    x = merge(
      x = ag_effic_bycomid,
      y = existing_buffers,
      by = c(
        "comid",
        "hydseq", 
        "cfromnode", 
        "ctonode",
        "State",
        "BMP_Category", 
        "BMP", 
        "Parameter", 
        "Value",
        "Value_Type"
      ),
      all = TRUE
    ),
    y = NdepChange,
    by = c(
      "comid", 
      "hydseq",
      "cfromnode", 
      "ctonode", 
      "State",
      "Nutrient",
      "Year", 
      "Parameter", 
      "Value"
    ),
    all = TRUE
  ),
  y = riparian_loading_bycomid,
  by = c(
    "comid", 
    "hydseq", 
    "cfromnode", 
    "ctonode",
    "State",
    "Nutrient", 
    "Year",
    "Parameter",
    "Value_Type",
    "Value"
  ),
  all = TRUE
) %>%
  group_by(comid) %>%
  mutate(LENGTHKM_se = replace_na(mean(LENGTHKM_se, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = -c(
      "BMP_Category", 
      "BMP", 
      "Nutrient",
      "Parameter", 
      "Approx_DesignSpec_Value",
      "Approx_DesignSpec_Units",
      "Year",
      "Value",
      "Value_Type"
    ),
    names_from = c(
      "BMP_Category", 
      "BMP", 
      "Nutrient",
      "Parameter", 
      "Approx_DesignSpec_Value",
      "Approx_DesignSpec_Units",
      "Year",
      "Value_Type"
    ), 
    values_from = "Value"
  ) %>%
  full_join(
    ., 
    wwtp_bycomid, 
    by = c("comid", "hydseq", "cfromnode", "ctonode", "State")
  ) %>%
  pivot_wider(
    id_cols = -c(
      "BMP_Category", 
      "Nutrient",
      "Year",
      "Parameter", 
      "Value_Type",
      "Value"
    ),
    names_from = c(
      "BMP_Category", 
      "Nutrient",
      "Parameter", 
      "Year",
      "Value_Type"
    ), 
    values_from = "Value"
  ) %>%
  left_join(., cost.table, by = "comid") %>%
  left_join(., state.cost.table, by = "State")

text.workeddata <- merge(
  x = urban_effic_bycomid, 
  y = riparian_buffer_efficiencies_bycomid, 
  by = c(
    "comid", 
    "hydseq", 
    "cfromnode", 
    "ctonode",
    "State",
    "BMP_Category", 
    "BMP", 
    "Nutrient", 
    "Parameter", 
    "Value",
    "Value_Type"
  ),
  all = TRUE
) %>%
  pivot_wider(
    id_cols = -c(
      "BMP_Category", 
      "BMP", 
      "Nutrient",
      "Parameter", 
      "Approx_DesignSpec_Value",
      "Approx_DesignSpec_Units",
      "Value"
    ),
    names_from = c(
      "BMP_Category", 
      "BMP", 
      "Nutrient",
      "Parameter", 
      "Approx_DesignSpec_Value",
      "Approx_DesignSpec_Units",
      "Value_Type"
    ), 
    values_from = "Value"
  )

worked.table <- merge(
  text.workeddata, 
  numeric.workeddata, 
  by = c("comid", "hydseq", "cfromnode", "ctonode", "State"), 
  all = TRUE
)
names(worked.table) <- gsub(
  gsub(
    gsub(names(worked.table), pattern = "NA_", replacement = ""),
    pattern = "_NA",
    replacement = ""
  ),
  pattern = "_$",
  replacement = ""
)

sparrow.tables <- merge(
  x = merge(
    x = sparrow_in %>% 
      select(
        comid, 
        FL_GNIS_Na, 
        LevelPathI,
        TermFlag, 
        IncAreaKm2,
        TotAreaKM2, 
        CumAreaKm2,
        LENGTHKM,
        cfromnode, 
        ctonode, 
        reachtype,
        hydseq,
        basin_outlet, 
        estuary,
        FLOWcfs,
        urban_km2, 
        HUC_12 = HUC_12_Rev
      ) %>%
      mutate(
        FLOWcfs = signif(FLOWcfs, 3), CumAreaKm2 = signif(CumAreaKm2, 6)
      ), 
    y = sparrow_cons_out_tp %>% 
      rename(
        RES_DECAY_TP = RES_DECAY,
        DEL_FRAC_TP = DEL_FRAC, 
        SE_RES_DECAY_TP = SE_RES_DECAY, 
        SE_DEL_FRAC_TP = SE_DEL_FRAC
      ) %>%
      mutate(
        FLOWcfs = signif(FLOWcfs, 3), CumAreaKm2 = signif(CumAreaKm2, 6)
      ), 
    by = c("comid", "CumAreaKm2", "IncAreaKm2", "FLOWcfs"),
    all = TRUE
  ),
  y = sparrow_cons_out_tn %>% 
    rename(
      RES_DECAY_TN = RES_DECAY,
      DEL_FRAC_TN = DEL_FRAC, 
      SE_RES_DECAY_TN = SE_RES_DECAY, 
      SE_DEL_FRAC_TN = SE_DEL_FRAC
    ) %>%
    mutate(
      FLOWcfs = signif(FLOWcfs, 3), CumAreaKm2 = signif(CumAreaKm2, 6)
    ), 
  by = c("comid", "CumAreaKm2", "IncAreaKm2", "FLOWcfs"),
  all = TRUE
)

streamcat.tables <- merge(
  x = imperv %>% 
    select(comid, PctImp2011Cat, State = StateAbbrev) %>% 
    unique(.),
  y = cropland %>% 
    select(comid, PctCrop2011Cat, State = StateAbbrev) %>% 
    unique(.),
  by = c("comid", "State"),
  all = TRUE
)

cv_numpixels_nlcd <- 0.17 # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.) The PctImp2011Cat came from NLCD, possibly the impervious dataset, not the larger categorical one. I'm assuming the accuracy is the same regardless. https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/Documentation/Metadata/ImperviousSurfaces2016.html

full.table <- merge(
  x = merge(
    x = sparrow.tables,
    y = streamcat.tables,
    by = c("comid"),
    all = TRUE
  ),
  y = worked.table,
  by = c("comid", "State", "hydseq", "cfromnode", "ctonode"),
  all = TRUE
) %>%
  remove_empty(.) %>%
  mutate(
    PctImp2011Cat_se = PctImp2011Cat * cv_numpixels_nlcd,
    PctCrop2011Cat_se = PctCrop2011Cat * cv_numpixels_nlcd,
    `TN_AtmosphericDep_kgha-1_2012_se` = `TN_AtmosphericDep_kgha-1_2012` * 0.43, # Roughly corresponds to a WDUM of 2 in https://www.sciencedirect.com/science/article/pii/S0048969719329109?via%3Dihub
    `TN_AtmosphericDep_kgha-1_2019_se` = `TN_AtmosphericDep_kgha-1_2019` * 0.43, # Roughly corresponds to a WDUM of 2 in https://www.sciencedirect.com/science/article/pii/S0048969719329109?via%3Dihub
    point_TN_Efficiency_fracremoval_se = point_TN_Efficiency_fracremoval * 0.2 # Assume a 20% coefficient of variation based on methods in JJ Environmental (2015)
    ) %>%
  mutate(
    across(where(is.character), .fns = ~replace_na(., replace = "-")),
    across(where(is.numeric), .fns = ~replace_na(., replace = -999))
  )



## Write Tables to csv ####

destination.folder <- "../RBEROSTdataforEDM/"

write_csv(
  full.table,
  file = paste0(destination.folder, "RBEROSTdata.csv")
  )

##########################################################################################################################################
### Note about shapefile (Cathy Chamberlin 7/27/2021): There was an error with this version of the dbf.                                ###
### It was missing 11 comids. The dbf file has been remade using the SPARROW reaches shapefile and the SPARROW input and output files. ###
### The new version was created with FindCOMIDroutingissuewithEDM.Rmd                                                                  ###
##########################################################################################################################################

# ## Write dbf ####
# 
# flowline <- read_sf("./Data/ctwtshd.gpkg", "NHDFlowline_Network")
# catchments <- read_sf("./Data/ctwtshd.gpkg", "CatchmentSP")
# 
# shapefile.table <- flowline %>% 
#   select(
#     comid, gnis_name, lengthkm, reachcode, ftype, streamorde, fromnode, tonode, hydroseq
#   ) %>%
#   left_join(., sparrow_cons_out_tn %>% select(comid, tn, in.), by = "comid") %>%
#   left_join(., sparrow_cons_out_tp %>% select(comid, tp, ip), by = "comid")
# 
# # st_write(shapefile.table, dsn = paste0(destination.folder, "RBEROSTshapefile.shp"), factor2char = TRUE, max_nchar = 254) 

# write_csv(
#   ag_effic_bycomid, 
#   file = paste0(destination.folder, "ag_effic_bycomid.csv")
#   )
# write_csv(
#   existing_buffers, 
#   file = paste0(destination.folder, "existing_riparianbufferlength.csv")
#   )
# write_csv(
#   NdepChange, 
#   file = paste0(destination.folder, "Ndeposition_bycomid.csv")
#   )
# write_csv(
#   urban_effic_bycomid, 
#   file = paste0(destination.folder, "urban_effic_bycomid.csv")
#   )
# write_csv(
#   riparian_buffer_efficiencies_bycomid, 
#   file = paste0(destination.folder, "riparian_buffer_efficiencies_bycomid.csv")
#   )
# write_csv(
#   riparian_loading_bycomid, 
#   file = paste0(destination.folder, "riparian_loading_bycomid.csv")
#   )
# write_csv(
#   wwtp_bycomid, 
#   file = paste0(destination.folder, "wwtp_bycomid.csv")
#   )