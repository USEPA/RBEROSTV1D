###########################################################################################
# PURPOSE: Generate Uncertainty Estimates in the AMPL Model
# BY: Cathy Chamberlin
# DATE:  4/1/21
# UPDATED BY: Sam Ennett, Kelly-Anne Moffa (ICF)
# DATE: 11/2025
# UPDATED BY: Craig Connolly, Naomi Detenbeck (EPA)
# DATE: 5/2026
###########################################################################################
# This step generates the uncertainty estimates used in the AMPL model. It is designed to be run after the Preprocessor, and will ammend the AMPL files.

# Science base reference: https://www.sciencebase.gov/catalog/item/5d4192aee4b01d82ce8da477


# Run Preprocessor without uncertainty ####
suppressMessages(
  suppressWarnings(
    invisible(
      capture.output(
        source("./RBEROST-Northeast/R/01_Optimization_Preprocessing_Northeast_Seasonal_V6_[PROD].R", 
               local = TRUE)
      )
    )
  )
)

print(
  paste(
    "RBEROST is now creating AMPL files with uncertainty analysis at",
    Sys.time()
  )
)

scenarioincrement <- 1 - scenariostepchange

## SPARROW N uncertainty ####

n_columns <- c("in_total", "in_poin", "in_fert_ag", "in_manu", "in_atmo", "in_urb",
               "in_fert_dev", "in_septic", "PLOAD_INC_SCS", "PLOAD_INC_BFN", "PLOAD_INC_STO") # KM: removed PLOAD_INC_ST to remove any confusion as to whether or not it should be included
n_se_columns <- c("sin_total", "sin_poin", "sin_fert_ag", "sin_fert_dev",
                  "sin_manu", "sin_urb", "sin_other", "sin_septic",
                  "sin_storage")

temp_inc_tn <- sparrow_cons_out_tn %>% 
  mutate(comid = as.character(comid)) %>%
  select(c("comid","year","season", all_of(n_columns), all_of(n_se_columns)))


temp_inc_tn$in_ag <- with(temp_inc_tn, in_fert_ag + in_manu) #*#

temp_inc_tn$sin_ag <- temp_inc_tn %>%
  rowwise() %>% 
  mutate(
    sin_ag = my_propogateerror(
      vals = list(
        c(in_fert_ag, sin_fert_ag), c(in_manu, sin_manu)
      ), 
      method = "addsub"
    )
  ) %>%
  ungroup() %>%
  .$sin_ag

temp_inc_tn_rev <- temp_inc_tn

temp_inc_tn_rev[is.na(temp_inc_tn_rev)] <- 0

## in_other and in_storage uncertainty ####
temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, in_atmo + PLOAD_INC_SCS + PLOAD_INC_BFN) #*# # KM: Already calculated "other" SE in standard preprocessing code


temp_inc_tn_rev$in_storage <- with(temp_inc_tn_rev, PLOAD_INC_STO) #*# # KM: Already have storage SE from standard preprocessing code


## N data ####
inc_tn <- temp_inc_tn_rev %>% 
  select(
    c(
      "comid",
      "season",
      "year",
      "in_total",
      "in_poin",
      "in_urb",
      "in_ag",
      "in_other",
      "in_septic",
      "in_storage",
      "sin_total",
      "sin_poin", 
      "sin_urb",
      "sin_ag", 
      "sin_other",
      "sin_septic",
      "sin_storage"
    )
  ) %>%
  mutate(across(.cols = -comid, .fns = ~replace_na(., replace = 0)))

inc_tn_rev <- inc_tn

inc_tn_rev[is.na(inc_tn_rev)] <- 0

## SPARROW P uncertainty ####
p_columns <- c("ip", "ip_poin", "ip_fert", "ip_manu", "ip_rock", "ip_forest", "ip_atmo", "ip_urb","PLOAD_INC_SCS", "PLOAD_INC_STO") # KM: Removed PLOAD_INC_ST so there's no question as to whether or not it should be used
p_se_columns <- c("sip_total", "sip_poin", "sip_urb", "sip_other",
                  "sip_ag", "sip_storage")

temp_inc_tp <- sparrow_cons_out_tp %>% 
  mutate(comid = as.character(comid)) %>%
  select(c("comid","year","season", all_of(p_columns), all_of(p_se_columns))) #*#

temp_inc_tp$ip_ag <- with(temp_inc_tp, ip_fert+ip_manu) #*# # KM: already calculate SE for ag in standard preprocessing code

temp_inc_tp$ip_other <- with(temp_inc_tp, ip_rock+PLOAD_INC_SCS+ip_atmo+ip_forest) #*#

temp_inc_tp$ip_storage <- with(temp_inc_tp, PLOAD_INC_STO) #*#

inc_tp <- temp_inc_tp %>% 
  select(
    c(
      "comid", 
      "year",
      "season",
      "ip",
      "ip_poin",
      "ip_urb", 
      "ip_ag",
      "ip_other",
      "ip_storage",
      "sip_total",
      "sip_poin",
      "sip_urb",
      "sip_ag", 
      "sip_other",
      "sip_storage"
    )
  ) %>%
  mutate(across(.cols = -comid, .fns = ~replace_na(., replace = 0)))

## propagate error in transfer coefficient
### Separate by season
sparrow_cons_out_tn_s <- inc_tn_rev %>% 
  arrange(year, comid, season) %>%
  select(comid, year, season, in_total, in_storage, sin_total, sin_storage)

### Compute transfer coefficients
sm_N <- data.frame(
  sparrow_cons_out_tn_s$comid,
  sparrow_cons_out_tn_s$year,
  sparrow_cons_out_tn_s$season,
  # Calculate transfer coefficients between seasons
  ifelse(
    sparrow_cons_out_tn_s$in_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tn_s$in_total, default = 0) == 0,
      sparrow_cons_out_tn_s$in_storage / lead(sparrow_cons_out_tn_s$in_total, default = 1),
      sparrow_cons_out_tn_s$in_storage / sparrow_cons_out_tn_s$in_total
    )
  ),
  # Calculate transfer coefficients standard error between seasons
  ifelse(
    sparrow_cons_out_tn_s$sin_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tn_s$sin_total, default = 0) == 0,
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tn_s$in_storage, sparrow_cons_out_tn_s$sin_storage), 
          c(lead(sparrow_cons_out_tn_s$in_total, default = 1), lead(sparrow_cons_out_tn_s$sin_total, default = 1))
        ), 
        method = "div"
      ),
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tn_s$in_storage, sparrow_cons_out_tn_s$sin_storage), 
          c(sparrow_cons_out_tn_s$in_total, sparrow_cons_out_tn_s$sin_total)
        ), 
        method = "div"
      )
    )
  )
) %>%
  rename(comid = 1,
         year = 2,
         season = 3,
         coeff = 4,
         coeff_se = 5) %>%
  mutate(across(.cols = -c(comid, year, season), .fns = ~replace_na(., replace = 0)))

### Overwrite year 20 transfer coefficients, no Y21S1 to compare to
sm_N$coeff <- ifelse(sm_N$year == 20 & sm_N$season == 4, 0, sm_N$coeff)
sm_N$coeff_se <- ifelse(sm_N$year == 20 & sm_N$season == 4, 0, sm_N$coeff_se)


#
#
## Prep TP Data by Season ---------
sparrow_cons_out_tp_s <- inc_tp %>% 
  arrange(year, comid, season) %>%
  select(comid, year, season, ip, ip_storage, sip_total, sip_storage)

### Compute transfer coefficients
sm_P <- data.frame(
  sparrow_cons_out_tp_s$comid,
  sparrow_cons_out_tp_s$year,
  sparrow_cons_out_tp_s$season,
  # Calculate transfer coefficients between seasons
  ifelse(
    sparrow_cons_out_tp_s$ip == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tp_s$ip, default = 0) == 0,
      sparrow_cons_out_tp_s$ip_storage / lead(sparrow_cons_out_tp_s$ip, default = 1),
      sparrow_cons_out_tp_s$ip_storage / sparrow_cons_out_tp_s$ip
    )
  ),
  # Calculate transfer coefficients standard error between seasons
  ifelse(
    sparrow_cons_out_tp_s$sip_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tp_s$sip_total, default = 0) == 0,
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tp_s$ip_storage, sparrow_cons_out_tp_s$sip_storage), 
          c(lead(sparrow_cons_out_tp_s$ip, default = 1), lead(sparrow_cons_out_tp_s$sip_total, default = 1))
        ), 
        method = "div"
      ),
      my_propogateerror(
        vals = list(
          c(sparrow_cons_out_tp_s$ip_storage, sparrow_cons_out_tp_s$sip_storage), 
          c(sparrow_cons_out_tp_s$ip, sparrow_cons_out_tp_s$sip_total)
        ), 
        method = "div"
      )
    )
  )
) %>%
  rename(comid = 1,
         year = 2,
         season = 3,
         coeff = 4,
         coeff_se = 5) %>%
  mutate(across(.cols = -c(comid, year, season), .fns = ~replace_na(., replace = 0)))

### Overwrite year 20 transfer coefficients, no Y21S1 to compare to
sm_P$coeff <- ifelse(sm_P$year == 20 & sm_P$season == 4, 0, sm_P$coeff)
sm_P$coeff_se <- ifelse(sm_P$year == 20 & sm_P$season == 4, 0, sm_P$coeff_se)
# ND: Set P transfer coefficient to zero because P storage term in dynamic SPARROW
# model is insignificant so all subsequent storage terms should be zero
sm_P$coeff <- 0
sm_P$coeff_se <- 0

# # Transfer coefficient threshold
if (use_threshold) {
  sm_N[,4][sm_N[,4] > threshold] <- 0
  sm_P[,4][sm_P[,4] > threshold] <- 0
}

temp_sm_N_dat_se <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_sm_N_dat_tmp <- sm_N[sm_N$comid %in% watershed_comid$catchment_comid,] %>%
    arrange(comid) %>%
    select(-coeff) %>%
    rename(coeff = coeff_se)
  
}

temp_sm_P_dat_se <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_sm_P_dat_tmp <- sm_P[sm_P$comid %in% watershed_comid$catchment_comid,] %>%
    arrange(comid) %>%
    select(-coeff) %>%
    rename(coeff = coeff_se)
}

# (SC for StreamCat)
temp_sm_N_dat_SC_se <- foreach(i = 1:length(temp_sm_N_dat_se)) %do% {
  temp_sm_N_dat_se[[i]][
    temp_sm_N_dat_se[[i]]$comid %in% streamcat_subset_tn[[i]]$comid,
  ]
}

temp_sm_N_dat_other_se <- foreach(i = 1:length(temp_sm_N_dat_se)) %do% {
  temp_sm_N_dat_se[[i]][
    !(temp_sm_N_dat_se[[i]]$comid %in% streamcat_subset_tn[[i]]$comid),
  ]
}

# (SC for StreamCat)
temp_sm_P_dat_SC_se <- foreach(i = 1:length(temp_sm_P_dat_se)) %do% {
  temp_sm_P_dat_se[[i]][
    temp_sm_P_dat_se[[i]]$comid %in% streamcat_subset_tp[[i]]$comid,
  ]
}

temp_sm_P_dat_other_se <- foreach(i = 1:length(temp_sm_P_dat_se)) %do% {
  temp_sm_P_dat_se[[i]][
    !(temp_sm_P_dat_se[[i]]$comid %in% streamcat_subset_tp[[i]]$comid),
  ]
}


## Format transfer coefficients -----
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  sm_N_dat_se <- foreach(i = 1:length(temp_sm_N_dat_SC_se)) %do% {
    if (!is.null(temp_sm_N_dat_SC_se[[i]]) && nrow(temp_sm_N_dat_SC_se[[i]]) > 0) {
      temp_sm_N_dat_opt <- temp_sm_N_dat_SC_se[[i]]
      temp_sm_N_dat_opt$comid_form <- paste0(
        "'", temp_sm_N_dat_opt$comid, "'"
      )
      temp_sm_N_dat_opt$year_form <- paste0(
        "'", temp_sm_N_dat_opt$year, "'"
      )
      temp_sm_N_dat_opt %>%
        select(c('comid_form', season, year, 'coeff'))
    } else {
      NULL
    }
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  sm_P_dat_se <- foreach(i = 1:length(temp_sm_P_dat_SC_se)) %do% {
    if (!is.null(temp_sm_P_dat_SC_se[[i]]) && nrow(temp_sm_P_dat_SC_se[[i]]) > 0) {
      temp_sm_P_dat_opt <- temp_sm_P_dat_SC[[i]]
      temp_sm_P_dat_opt$comid_form <- paste0(
        "'", temp_sm_P_dat_opt$comid, "'"
      )
      temp_sm_P_dat_opt %>%
        select(c('comid_form', season, year, 'coeff'))
    } else {
      NULL  # Skip empty data frames
    }
  }
}



## propagate error in delivery fraction ####

### Adjust delivery fraction to pore point
max_delfrac_tn <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    sparrow_cons_out_tn %>% 
      filter(comid %in% watershed_comid$catchment_comid) %>%
      summarize(max_del_frac = max(DEL_FRAC, na.rm = TRUE), .groups = "drop") %>%
      pull(max_del_frac)
    
  }
)

max_se_delfrac_tn <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    sparrow_cons_out_tn %>% 
      filter(comid %in% watershed_comid$catchment_comid) %>%
      mutate(SE_DEL_FRAC = 0) %>% #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
      summarize(se_del_frac = mean(SE_DEL_FRAC, na.rm = TRUE), .groups = "drop") %>%
      pull(se_del_frac)
    
  }
)

temp_delfrac_rev_tn <- foreach(i = 1:length(target_selection)) %do% { 
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "year", "season", "DEL_FRAC")) %>%
    mutate(SE_DEL_FRAC = 0) #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
  
}

invisible(
  foreach(i = 1:length(target_selection)) %do% {
    
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], DEL_FRAC / max_delfrac_tn[i]
    )
    temp_delfrac_rev_tn[[i]]$se_delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]],
      (DEL_FRAC / max_se_delfrac_tn[i]) *
        sqrt(
          (SE_DEL_FRAC / DEL_FRAC) ^ 2 +
            (max_se_delfrac_tn[i] / max_delfrac_tn[i]) ^ 2
        )
    )
    
  }
)

delfrac_rev_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  if (nrow(temp_delfrac_rev_tn[[i]]) > 0) {

  temp_delfrac_rev_tn[[i]] %>%
    select(c("comid", "year", "season", "delfrac_rev", "se_delfrac_rev")) %>%
    mutate(se_delfrac_rev = case_when(is.na(se_delfrac_rev) ~ 0, T ~ se_delfrac_rev))
  } else {
    NULL
  }
}

max_delfrac_tp <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TP_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {
      
      sparrow_cons_out_tp %>% 
        filter(comid %in% watershed_comid$catchment_comid) %>%
        summarize(max_del_frac = max(DEL_FRAC, na.rm = TRUE), .groups = "drop") %>%
        pull(max_del_frac)
      
    } else { 1 }
    
  }
)

max_se_delfrac_tp <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TP_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {
      
      sparrow_cons_out_tp %>% 
        filter(comid %in% watershed_comid$catchment_comid) %>%
        mutate(SE_DEL_FRAC = 0) %>% #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
        summarize(se_del_frac = mean(SE_DEL_FRAC, na.rm = TRUE), .groups = "drop") %>%
        pull(se_del_frac)
      
    } else { 0 }
    
  }
)


temp_delfrac_rev_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tp %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(c("comid", "year", "season", "DEL_FRAC")) %>%
    mutate(SE_DEL_FRAC = 0) #*# KM: Not in SPARROW data with standard errors sent by EPA, assuming SE of 0
  
}

invisible(
  foreach(i = 1:length(target_selection)) %do% {
    
    if(nrow(temp_delfrac_rev_tp[[i]]) > 0) {
    
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[i]
    )
    temp_delfrac_rev_tp[[i]]$se_delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]],
      (DEL_FRAC / max_delfrac_tp[i]) *
        sqrt(
          (SE_DEL_FRAC / DEL_FRAC) ^ 2 +
            (max_se_delfrac_tp[i] / max_delfrac_tp[i]) ^ 2
        )
      )
    }
    
  }
)

delfrac_rev_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  if(nrow(temp_delfrac_rev_tp[[i]]) > 0) {
  
  temp_delfrac_rev_tp[[i]] %>% select(c("comid", "year", "season", "delfrac_rev", "se_delfrac_rev")) %>%
      mutate(season = as.numeric(season)) %>%
      mutate(se_delfrac_rev = case_when(is.na(se_delfrac_rev) ~ 0, T ~ se_delfrac_rev))
    
  }
  
  else {
    NULL
  }
  
}

## uncertainty in delivered baseloads ####



### Multiply all loads by revised del_frac
temp_inc_tn_dat <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  if (nrow(watershed_comid) > 0) {
  temp_inc_tn_dat_tmp <- merge(
    inc_tn_rev[inc_tn_rev$comid %in% watershed_comid$catchment_comid,],
    delfrac_rev_tn[[i]],
    by = c("comid", "season", "year"),
    all.x = TRUE
  )
  
  temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_ag","in_other","in_septic","in_storage")] <- 
    temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_ag","in_other","in_septic","in_storage")] *
    temp_inc_tn_dat_tmp[["delfrac_rev"]]
  
  temp_inc_tn_dat_tmp[c("sin_poin","sin_urb","sin_ag","sin_other","sin_septic","sin_storage")] <-
    temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_ag","in_other","in_septic","in_storage")] *
    temp_inc_tn_dat_tmp[["delfrac_rev"]] *
    sqrt(
      (temp_inc_tn_dat_tmp[c("sin_poin","sin_urb","sin_ag","sin_other","sin_septic","sin_storage")] / temp_inc_tn_dat_tmp[c("in_poin","in_urb","in_ag","in_other","in_septic","in_storage")]) ^ 2 +
        (
          temp_inc_tn_dat_tmp[["se_delfrac_rev"]] / 
            temp_inc_tn_dat_tmp[["delfrac_rev"]]
        ) ^ 2
    )
  
  temp_inc_tn_dat_tmp <- temp_inc_tn_dat_tmp[
    order(temp_inc_tn_dat_tmp$comid), 
  ]
  
  temp_inc_tn_dat_tmp
  }
  
}

temp_inc_tp_dat <- foreach(i = 1:length(target_selection)) %do% { 

  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])

  if (nrow(watershed_comid) > 0) {
    temp_inc_tp_dat_tmp <- merge(
      inc_tp[inc_tp$comid %in% watershed_comid$catchment_comid, ],
      delfrac_rev_tp[[i]],
      by = c("comid", "season", "year"),
      all.x = TRUE
    )

    temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_ag", "ip_other","ip_storage")] <-
      temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_ag", "ip_other","ip_storage")] *
        temp_inc_tp_dat_tmp[["delfrac_rev"]]

    temp_inc_tp_dat_tmp[c("sip_poin", "sip_urb", "sip_ag", "sip_other","sip_storage")] <-
      temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_ag", "ip_other","ip_storage")] *
      temp_inc_tp_dat_tmp[["delfrac_rev"]] *
      sqrt(
        (temp_inc_tp_dat_tmp[c("sip_poin", "sip_urb", "sip_ag", "sip_other","sip_storage")] / temp_inc_tp_dat_tmp[c("ip_poin", "ip_urb", "ip_ag", "ip_other","ip_storage")])^2 +
          (
            temp_inc_tp_dat_tmp[["se_delfrac_rev"]] /
              temp_inc_tp_dat_tmp[["delfrac_rev"]]
          )^2
      )

    temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
      order(temp_inc_tp_dat_tmp$comid),
    ]
    
    temp_inc_tp_dat_tmp
  }
}


## uncertainty in runoffcoeff ####

### Calculate runoff coefficient for urban area

# Specify column for impervious dataset
temp_runoffcoeff <- StreamCat_api %>% select(c("comid","PctImp2019Cat")) %>% #*#
  mutate(cv_numpixels_nlcd = 0.17) %>% # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.) The PctImp2011Cat came from NLCD, possibly the impervious dataset, not the larger categorical one. I'm assuming the accuracy is the same regardless. https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/Documentation/Metadata/ImperviousSurfaces2016.html
  mutate(PctImp2019Cat_se = PctImp2019Cat * cv_numpixels_nlcd)

temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2019Cat
) #*#

#This equation comes from Schueler 1987, Table A6 & Figure 1-2. 
Schueler.table <- data.frame(
  PercentImpervious = c(
    41, 38, 24, 33, 33, 19, 29, 29, 76, 20, 22, 38, 29, 50, 57, 21, 18, 37, 37, 22, 17, 27, 21, 34, 58, 81, 23, 5, 6, 69, 99, 91, 69, 21, 99, 90, 4, 1, 11, 7, 55, 34, 90, 22
  ),
  Mean = c(
    0.35, 0.18, 0.16, 0.46, 0.25, 0.19, 0.47, 0.24, 0.56, 0.24, 0.17, 0.22, 0.2, 0.37, 0.43, 0.17, 0.18, 0.24, 0.37, 0.99, 0.19, 0.11, 0.26, 0.28, 0.73, 0.82, 0.28, 0.11, 0.02, 0.65, 0.98, 0.99, 0.9, 0.21, 0.92, 0.74, 0.08, 0.12, 0.05, 0.08, 0.57, 0.47, 0.75, 0.42
  )
)

temp_runoffcoeff$runoffcoeff_se <- (predict(
  with(Schueler.table, lm(Mean ~ PercentImpervious)), 
  newdata = data.frame(PercentImpervious = temp_runoffcoeff$PctImp2019Cat), 
  se.fit = TRUE
)$se.fit) / sqrt(nrow(Schueler.table))

runoffcoeff <- temp_runoffcoeff %>% select(c("comid", "runoffcoeff", "runoffcoeff_se"))
runoffcoeff <- distinct(runoffcoeff) # Removes duplicates

## format baseline loading se ####

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  inc_tn_dat_se <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
    if (!is.null(temp_inc_tn_dat[[i]]) && nrow(temp_inc_tn_dat[[i]]) > 0) {
      temp_inc_tn_dat_opt <- temp_inc_tn_dat[[i]]
      temp_inc_tn_dat_opt$comid_form <- paste0(
        "'", temp_inc_tn_dat_opt$comid, "'"
      )
      temp_inc_tn_dat_opt %>%
        arrange(year, comid, season) %>%
        select(c("comid_form", "season", "year", "sin_poin", "sin_urb", "sin_ag", "sin_septic", "sin_other", "sin_storage")) %>%
        mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
    }
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  inc_tp_dat_se <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
    if (!is.null(temp_inc_tp_dat[[i]]) && nrow(temp_inc_tp_dat[[i]]) > 0) {
      temp_inc_tp_dat_opt <- as.data.frame(temp_inc_tp_dat[[i]])
      temp_inc_tp_dat_opt$comid_form <- paste0("'", temp_inc_tp_dat_opt$comid, "'")
      temp_inc_tp_dat_opt %>% 
        arrange(year, comid, season) %>%
        select(c("comid_form", "season", "year", "sip_poin", "sip_urb", "sip_ag", "sip_other", "sip_storage")) %>%
        mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
    }
  }
}

## format runoff coeff ####
temp_runoffcoeff_dat <- runoffcoeff %>%
  filter(comid %in% streamcat_subset_all$comid)

temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
runoffcoeff_dat_se <- temp_runoffcoeff_dat %>% 
  select(c("comid_form", "runoffcoeff_se")) 

## uncertainty in riparian loadings ####

riparian_loadings_tn <- foreach(i = seq_along(target_selection)) %do% {
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  if(nrow(watershed_comid) > 0 & !is.null(watershed_comid)) {
  riparian.loadings %>%
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, season, year, N_riparian_kgyr, N_riparian_kgyr_se) %>%
    mutate(year = as.character(str_pad(year, width = 2, side = "left", pad = "0"))) %>%
    right_join(., temp_inc_tn_dat[[i]] %>%
                filter(comid %in% watershed_comid$catchment_comid) %>%
                select(comid, season, year, in_total, sin_total), by = c("comid", "season", "year")) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > in_total ~ in_total,
        is.na(N_riparian_kgyr) ~ 0,
        TRUE ~ N_riparian_kgyr
      ),
      N_riparian_kgyr_se = case_when(
        N_riparian_kgyr > in_total ~ sin_total,
        is.na(N_riparian_kgyr_se) ~ 0,
        TRUE ~ N_riparian_kgyr_se
      )
    ) %>% 
    select(-c(in_total, sin_total))
  }
}

riparian_loadings_tp <- foreach(i = seq_along(target_selection)) %do% {

  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])

    if(nrow(watershed_comid) > 0 & !is.null(watershed_comid)) {
      riparian.loadings %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        select(comid, season, year, P_riparian_kgyr, P_riparian_kgyr_se) %>%
        mutate(year = as.character(str_pad(year, width = 2, side = "left", pad = "0"))) %>%
        right_join(., temp_inc_tp_dat[[i]] %>%
                     mutate(season = as.numeric(season)) %>%
                      filter(comid %in% watershed_comid$catchment_comid) %>%
                      select(comid, season, year, ip, sip_total), by = c("comid", "season", "year")) %>%
        mutate(
          P_riparian_kgyr = case_when(
            P_riparian_kgyr > ip ~ ip,
            is.na(P_riparian_kgyr) ~ 0,
            TRUE ~ P_riparian_kgyr
          ),
          P_riparian_kgyr_se = case_when(
            P_riparian_kgyr > ip ~ sip_total,
            is.na(P_riparian_kgyr_se) ~ 0,
            TRUE ~ P_riparian_kgyr_se
          )
        ) %>% 
        select(-ip, -sip_total)
    }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  
  temp_riparian_tn_dat <- foreach(i = seq_along(target_selection)) %do% {

    if(!is.null(delfrac_rev_tn[[i]]) && nrow(delfrac_rev_tn[[i]]) > 0 && 
       !is.null(riparian_loadings_tn[[i]]) && nrow(riparian_loadings_tn[[i]]) > 0) {
      temp_riparian_tn_dat_tmp <- merge(
        riparian_loadings_tn[[i]],
        delfrac_rev_tn[[i]],
        by = c("comid", "season", "year"),
        all = TRUE
      ) %>%
        rowwise() %>%
        mutate(
          in_riparian = N_riparian_kgyr * delfrac_rev,
          sin_riparian = case_when(
            N_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
              vals = list(c(N_riparian_kgyr, N_riparian_kgyr_se), c(delfrac_rev, se_delfrac_rev)), 
              method = "mult"
            ),
            N_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
          )
        ) %>%
        arrange(comid, season) %>%
        select(comid, season, year, in_riparian, sin_riparian) %>%
        ungroup()
    }
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  temp_riparian_tp_dat <- foreach(i = seq_along(target_selection)) %do% {

    if(!is.null(delfrac_rev_tp[[i]]) && nrow(delfrac_rev_tp[[i]]) > 0 && 
       !is.null(riparian_loadings_tp[[i]]) && nrow(riparian_loadings_tp[[i]]) > 0) {
      temp_riparian_tp_dat_tmp <- merge(
        riparian_loadings_tp[[i]],
        delfrac_rev_tp[[i]],
        by = c("comid", "season", "year"),
        all = TRUE
      ) %>%
        rowwise() %>%
        mutate(
          ip_riparian = P_riparian_kgyr * delfrac_rev,
          sip_riparian = case_when(
            P_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
              vals = list(c(P_riparian_kgyr, P_riparian_kgyr_se), c(delfrac_rev, se_delfrac_rev)), 
              method = "mult"
            ),
            P_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
          )
        ) %>%
        arrange(comid, season) %>%
        select(comid, season, year, ip_riparian, sip_riparian) %>%
        ungroup()
    }
  }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
    
    if (!is.null(temp_riparian_tn_dat[[i]]) && nrow(temp_riparian_tn_dat[[i]]) > 0) {
      temp_riparian_tn_dat[[i]] %>%
        filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        select(comid_form, season, year, in_riparian, sin_riparian)
    }
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {

    if (!is.null(temp_riparian_tp_dat[[i]]) && nrow(temp_riparian_tp_dat[[i]]) > 0 ) {
      temp_riparian_tp_dat[[i]] %>%
        filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        select(comid_form, season, year, ip_riparian, sip_riparian)
    } 
  }
}


if(length(RiparianBuffer_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_efficiencies_N <- foreach(
      i = 1: length(streamcat_subset_tn)
    ) %do% {
      riparian_buffer_efficiencies_N_tmp <- foreach(
        j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
      ) %do% {
        short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
          "Grass"
        } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
        
        riparian.efficiencies.tmp <- riparian.efficiencies %>%
          select(
            -contains("P_"),
            -!contains(short.form.bmp.name),
            -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
            -!contains("uncertainty"),
            comid
          ) %>%
          rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
          mutate(
            x = RiparianBuffer_Widths[j],
            expression = strsplit(
              str_extract(
                mapply(
                  gsub,
                  pattern = "x",
                  replacement = x,
                  x = mapply(
                    gsub,
                    pattern = "'",
                    replacement = "",
                    x = Curve.Form
                  )
                ),
                pattern = "(?<=c[(]).*(?=[)])"
              ),
              ","
            )
          ) %>%
          rowwise() %>%
          mutate(
            iter = 1,
            effic = list(
              lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
            ),
            effic_mean = suppressWarnings(mean(unlist(effic))),
            effic_sd = sd(unlist(effic))
          ) %>%
          select(comid, effic_mean, effic_sd) %>%
          rename_with(
            .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
            .cols = contains("effic")
          ) %>%
          ungroup()
      }
      
      riparian_buffer_efficiencies_N_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
          
        )
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_efficiencies_P <- foreach(
      i = 1: length(streamcat_subset_tp)
    ) %do% {
      riparian_buffer_efficiencies_P_tmp <- foreach(
        j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
      ) %do% {
        short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
          "Grass"
        } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
        
        riparian.efficiencies.tmp <- riparian.efficiencies %>%
          filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
          select(
            -contains("N_"),
            -!contains(short.form.bmp.name),
            -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
            -!contains("uncertainty"),
            comid
          ) %>%
          rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
          mutate(
            x = RiparianBuffer_Widths[j],
            expression = strsplit(
              str_extract(
                mapply(
                  gsub,
                  pattern = "x", 
                  replacement = x,
                  x = mapply(
                    gsub,
                    pattern = "'", 
                    replacement = "", 
                    x = Curve.Form
                  )
                ), 
                pattern = "(?<=c[(]).*(?=[)])"
              ), 
              ","
            )
          ) %>%
          rowwise() %>%
          mutate(
            iter = 1,
            effic = list(
              lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
            ),
            effic_mean = suppressWarnings(mean(unlist(effic))),
            effic_sd = sd(unlist(effic))
          ) %>%
          select(comid, effic_mean, effic_sd) %>%
          rename_with(
            .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
            .cols = contains("effic")
          ) %>%
          ungroup()
      }
      
      riparian_buffer_efficiencies_P_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
          
        )
    }
  }
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_N)) %do% {
      riparian_buffer_efficiencies_N[[i]] <- riparian_buffer_efficiencies_N[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_P)) %do% {
      riparian_buffer_efficiencies_P[[i]] <- riparian_buffer_efficiencies_P[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
  ## uncertainty in loading per bankft ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_loadingperbankft_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      tmp1 <- left_join(
        riparian_loadings_tn[[i]], riparian.existingbuffer, by = "comid"
      ) %>%
        select(
          comid, 
          season,
          year,
          N_riparian_kgyr, 
          N_riparian_kgyr_se, 
          totalbanklength_ft, 
          totalbanklength_ft_se
        ) %>%
        filter(!is.na(totalbanklength_ft)) %>%
        rowwise() %>%
        mutate(
          loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft,
          loading_per_bankft_kg_ftyr_se = if(
            N_riparian_kgyr <= 0
          ) {0} else {
            my_propogateerror(
              vals = list(
                c(N_riparian_kgyr, N_riparian_kgyr_se), 
                c(totalbanklength_ft, totalbanklength_ft_se)
              ), 
              method = "div"
            )
          }
        ) %>%
        ungroup()
      tmp1
      
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_loadingperbankft_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      if (length(riparian_loadings_tp[[i]]) > 0) { 
        tmp1 <- left_join(
          riparian_loadings_tp[[i]], riparian.existingbuffer, by = "comid"
        ) %>%
          select(
            comid, 
            season,
            year,
            P_riparian_kgyr, 
            P_riparian_kgyr_se, 
            totalbanklength_ft, 
            totalbanklength_ft_se
          ) %>%
          filter(!is.na(totalbanklength_ft)) %>%
          rowwise() %>%
          mutate(
            loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft,
            loading_per_bankft_kg_ftyr_se = if(
              P_riparian_kgyr == 0
            ) {0} else {
              my_propogateerror(
                vals = list(
                  c(P_riparian_kgyr, P_riparian_kgyr_se), 
                  c(totalbanklength_ft, totalbanklength_ft_se)
                ), 
                method = "div"
              )
            }
          ) %>%
          ungroup()
        
        tmp1
      }
      
    }
  }
  
  ## riparianbufferremoval un-revised uncertainty ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_N_tmp <- foreach(i = 1:length(riparian_loadingperbankft_N)) %do% {
      
      tmp2 <- riparian_loadingperbankft_N[[i]]%>%
        left_join(., riparian_buffer_efficiencies_N[[i]], by = "comid") %>%
        pivot_longer(
          cols = contains(RiparianBuffer_BMPs), 
          names_to = c("RiparianBMP", NA, "ValueType"), 
          names_sep = "_",
          values_to = "Value"
        ) %>%
        pivot_wider(
          id_cols = c(
            "comid",
            "season",
            "year",
            "RiparianBMP", 
            'loading_per_bankft_kg_ftyr', 
            "loading_per_bankft_kg_ftyr_se"
          ), 
          names_from = "ValueType", 
          values_from = "Value"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
        rowwise() %>%
        mutate(
          removal = case_when(
            effic == -999 ~ -999, effic >= 0 ~ (effic  * loading_per_bankft_kg_ftyr)
          ),
          removal_se = case_when(
            removal == -999 ~ 0, 
            removal >= 0 ~ if(
              loading_per_bankft_kg_ftyr == 0 | effic == 0
            ) {0} else {
              my_propogateerror(
                vals = list(
                  c(effic, se), 
                  c(loading_per_bankft_kg_ftyr, loading_per_bankft_kg_ftyr_se)
                ),
                method = "mult"
              )
            }
          )
        ) %>%
        ungroup()
      
      tmp2 
      
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_P_tmp <- foreach(i = 1:length(riparian_loadingperbankft_P)) %do% {
      
      if (length(riparian_loadingperbankft_P[[i]]) > 0) { 
      
        tmp2 <- riparian_loadingperbankft_P[[i]]%>%
          left_join(., riparian_buffer_efficiencies_P[[i]], by = "comid") %>%
          pivot_longer(
            cols = contains(RiparianBuffer_BMPs), 
            names_to = c("RiparianBMP", NA, "ValueType"), 
            names_sep = "_",
            values_to = "Value"
          ) %>%
          pivot_wider(
            id_cols = c(
              "comid",
              "season",
              "year",
              "RiparianBMP", 
              'loading_per_bankft_kg_ftyr', 
              "loading_per_bankft_kg_ftyr_se"
            ), 
            names_from = "ValueType", 
            values_from = "Value"
          ) %>%
          mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
          rowwise() %>%
          mutate(
            removal = case_when(
              effic == -999 ~ -999, effic >= 0 ~ (effic  * loading_per_bankft_kg_ftyr)
            ),
            removal_se = case_when(
              removal == -999 ~ 0, 
              removal >= 0 ~ if(
                loading_per_bankft_kg_ftyr == 0 | effic == 0
              ) {0} else {
                my_propogateerror(
                  vals = list(
                    c(effic, se), 
                    c(loading_per_bankft_kg_ftyr, loading_per_bankft_kg_ftyr_se)
                  ),
                  method = "mult"
                )
              }
            )
          ) %>%
          ungroup()
        
        tmp2 
      }
      
    }
  }
  
  ## riparian buffer removal ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_N <- foreach(
      i = 1:length(riparian_buffer_removal_N_tmp)
    ) %do% {
      riparian_buffer_removal_N_tmp[[i]] %>%
        pivot_wider(
          id_cols = c("comid", "season", "year"), 
          names_from = "RiparianBMP", 
          values_from = c("removal", "removal_se")
        )%>%
        rename_with(
          .fn = ~gsub(., pattern = "removal_se", replacement = "se"), 
          .cols = contains("removal_se")
        )
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_buffer_removal_P <- foreach(
      i = 1:length(riparian_buffer_removal_P_tmp)
    ) %do% {
      if (length(riparian_buffer_removal_P_tmp[[i]]) > 0) {
        riparian_buffer_removal_P_tmp[[i]] %>%
          pivot_wider(
            id_cols = c("comid", "season", "year"), 
            names_from = "RiparianBMP", 
            values_from = c("removal", "removal_se")
          )%>%
          rename_with(
            .fn = ~gsub(., pattern = "removal_se", replacement = "se"), 
            .cols = contains("removal_se")
          )
      } 
      
    }
  }
  
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  riparian_tn_removal_se <- foreach(i = 1:length(riparian_buffer_removal_N)) %do% {
    
    # Get all comids that should be in the dataset
    all_comids_needed <- streamcat_subset_tn[[i]]$comid
    
    # Start with existing riparian data or create empty dataframe
    if (length(riparian_buffer_removal_N[[i]]) > 0 && nrow(riparian_buffer_removal_N[[i]]) > 0) {
      # Process existing riparian data
      tmp1 <- riparian_buffer_removal_N[[i]] %>%
        filter(comid %in% all_comids_needed) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        select(comid_form, season, year, contains("se_")) %>%
        rename_with(.fn = ~gsub("se_", "", .), .cols = -c(comid_form, season, year)) %>%
        rename_with(.fn = ~paste0(., "_Buffer"), .cols = -c(comid_form, season, year)) %>%
        mutate(across(.cols = any_of(RiparianBuffer_BMPs), .fns = ~case_when(. == -999 ~ 0, . >= 0 ~ .)))
      
      # Find missing comids
      existing_comids <- unique(riparian_buffer_removal_N[[i]]$comid)
      missing_comids <- setdiff(all_comids_needed, existing_comids)
    } else {
      # No existing data - all comids are missing
      tmp1 <- NULL
      missing_comids <- all_comids_needed
    }
    
    # Add zero-filled rows for missing comids
    if (length(missing_comids) > 0 && !is.null(temp_inc_tn_dat[[i]])) {
      # Create template with all season/year combinations for missing comids
      missing_rows <- temp_inc_tn_dat[[i]] %>%
        filter(comid %in% missing_comids) %>%
        select(comid, season, year) %>%
        distinct() %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
      # Add zero columns for each riparian BMP
      for (bmp in RiparianBuffer_BMPs) {
        missing_rows[[bmp]] <- 0
      }
      
      # Select columns to match tmp1 structure
      missing_rows <- missing_rows %>%
        select(comid_form, season, year, any_of(RiparianBuffer_BMPs))
      
      # Combine existing and missing data
      if (!is.null(tmp1)) {
        tmp1 <- bind_rows(tmp1, missing_rows) %>%
          arrange(comid_form, season, year)
      } else {
        tmp1 <- missing_rows %>%
          arrange(comid_form, season, year)
      }
    }
    
    tmp1
  }
    
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    riparian_tp_removal_se <- foreach(i = 1:length(riparian_buffer_removal_P)) %do% {
      
      # Get all comids that should be in the dataset
      all_comids_needed <- streamcat_subset_tp[[i]]$comid
      
      # Start with existing riparian data or create empty dataframe
      if (length(riparian_buffer_removal_P[[i]]) > 0 && nrow(riparian_buffer_removal_P[[i]]) > 0) {
        # Process existing riparian data
        tmp1 <- riparian_buffer_removal_P[[i]] %>%
          filter(comid %in% all_comids_needed) %>%
          mutate(comid_form = paste0("'", comid, "'")) %>%
          select(comid_form, season, year, contains("se_")) %>%
          rename_with(.fn = ~gsub("se_", "", .), .cols = -comid_form) %>%
          rename_with(.fn = ~paste0(., "_Buffer"), .cols = -c(comid_form, season, year)) %>%
          mutate(across(any_of(RiparianBuffer_BMPs), ~case_when(. == -999 ~ 0, . >= 0 ~ .)))
        
        # Find missing comids
        existing_comids <- unique(riparian_buffer_removal_P[[i]]$comid)
        missing_comids <- setdiff(all_comids_needed, existing_comids)
      } else {
        # No existing data - all comids are missing
        tmp1 <- NULL
        missing_comids <- all_comids_needed
      }
      
      # Add zero-filled rows for missing comids
      if (length(missing_comids) > 0 && !is.null(temp_inc_tp_dat[[i]])) {
        # Create template with all season/year combinations for missing comids
        missing_rows <- temp_inc_tp_dat[[i]] %>%
          filter(comid %in% missing_comids) %>%
          select(comid, season, year) %>%
          distinct() %>%
          mutate(comid_form = paste0("'", comid, "'"))
        
        # Add zero columns for each riparian BMP
        for (bmp in RiparianBuffer_BMPs) {
          missing_rows[[bmp]] <- 0
        }
        
        # Select columns to match tmp1 structure
        missing_rows <- missing_rows %>%
          select(comid_form, season, year, any_of(RiparianBuffer_BMPs))
        
        # Combine existing and missing data
        if (!is.null(tmp1)) {
          tmp1 <- bind_rows(tmp1, missing_rows) %>%
            arrange(comid_form, season, year)
        } else {
          tmp1 <- missing_rows %>%
            arrange(comid_form, season, year)
        }
      }
      
      tmp1
    }
  }
}
## area uncertainty ####

### Select urban area and incremental area from SPARROW input data
temp_sparrow_area <- sparrow_cons_out_tn %>% 
  select(c("comid","inc_area")) %>%
  distinct()

length(unique(temp_sparrow_area$comid)) # 20977, there are 20978 in new TN SPARROW...

### Select percentage of incremental area that is cropland
streamcat_ag_urban <- StreamCat_api %>% 
 select(c("comid","PctCrop2019Cat", "PctUrbHi2019Cat", "PctUrbLo2019Cat",
          "PctUrbMd2019Cat", "PctUrbOp2019Cat")) #*#           



### Sum urban sub-designations to get total urban area
#streamcat_ag_urban$urban_pct <- with(streamcat_ag_urban, 
#  PctUrbHi2019Cat+PctUrbLo2019Cat+PctUrbMd2019Cat+PctUrbOp2019Cat)
# ND: Revise to be consistent with definition of urban area in dynamic SPARROW
streamcat_ag_urban$urban_pct <- with(streamcat_ag_urban, 
                                     PctUrbHi2019Cat+PctUrbMd2019Cat)

### Convert the sparrow input area from KM to acres
temp_sparrow_area$inc_ac <- temp_sparrow_area$inc_area*km2_to_ac #*#

### Use the streamcat area percentages to idenitfy ag and urban areas in each catchment
# temp_area <- merge(temp_sparrow_area, streamcat_ag_urban,by="comid")
temp_area <- left_join(temp_sparrow_area, streamcat_ag_urban, by="comid") #ND get rid of excess LU data not in area of interest

temp_area$ag_ac <- with(temp_area,(PctCrop2019Cat/100)*inc_ac) #*#
temp_area$urban_ac <- with(temp_area,(urban_pct/100)*inc_ac) #*#

temp_area_se <- temp_area  %>%
  mutate(cv_numpixels_nlcd = 0.17) %>% # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)
  mutate(cv_km2_nlcd = cv_numpixels_nlcd) %>%
  mutate(urban_km2_se = (urban_pct/100)*inc_area * cv_km2_nlcd,
         ag_km2_se = (PctCrop2019Cat/100)*inc_area * cv_km2_nlcd) %>%
  mutate(urban_ac_se = urban_km2_se/km2_to_ac,
         ag_ac_se = ag_km2_se/km2_to_ac)

## Format area uncertainty ####
area <- temp_area_se %>% 
  select(c("comid","urban_ac","ag_ac", "urban_ac_se", "ag_ac_se")) %>%
  group_by(comid) %>%
  summarize(across(.fns = ~mean(.))) # Removes duplicates

temp_area_dat <- area[area$comid %in% reaches_all$comid,]
temp_area_dat <- temp_area_dat[order(temp_area_dat$comid),]
temp_area_dat$comid_form <- paste0("'", temp_area_dat$comid, "'")
area_dat <- temp_area_dat %>% 
  select(c("comid_form", "urban_ac", "ag_ac", "urban_ac_se", "ag_ac_se"))

## find standard error of AG & riparian costs over years ####

temp_bmp_costs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X") %>%
  select(
    BMP_Category, BMP, contains(c("capital", "operations")), UserSpec_RD_in
  )

### Conversion from area-specific costs to cost per acre for ag BMPs and to costs per linear foot for Riparian BMPs
temp_bmp_costs_rev <- temp_bmp_costs %>%
  mutate(
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~replace_na(., NA_real_)
    ),
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          capital_units == "ft2" ~ . * ft2_to_ac,
          capital_units == "km2" ~ . * km2_to_ac,
          capital_units == "yd2" ~ . * yd2_to_ac,
          capital_units == "ac" ~ .
        ),
        BMP_Category == "urban" ~ .,
        BMP_Category == "point" ~ .,
        BMP_Category == "ripbuf" ~ case_when(
          capital_units == "ft2" ~ . * UserSpec_RD_in,
          capital_units == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_RD_in,
          capital_units == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_RD_in,
          capital_units == "ac" ~ . / ft2_to_ac * UserSpec_RD_in
        )
      ), 
      .names = "{.col}_rev"
    )
  )

temp1 <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_rev", "", .))
  ) %>%
  # Annualize capital costs based on planning horizon and interest rate
  mutate(
    across(
      .cols = contains("capital_"), 
      .fns = ~(
        . * (
          interest_rate * ((1 + interest_rate) ^ horizon) / (
            ((1 + interest_rate) ^ horizon) - 1
          )
        )
      )
    )
  )

temp2 <- temp1[temp1$category == 'point',]
temp2[is.na(temp2)] <- 0
temp1[temp1$category == 'point',] <- temp2
bmp_costs <- copy(temp1)
rm(temp1, temp2)

bmp_costs_se <- bmp_costs %>% ## KM: when septic BMPs are selected, this will also calculate standard errors 
  pivot_longer(
    cols = -c(category, bmp), 
    names_to = c("costtype", "State"), 
    names_sep = "_", 
    values_to = "Value"
  ) %>%
  group_by(
    category, bmp, costtype
  ) %>%
  mutate(mean = mean(Value, na.rm = T), se = (sd(Value, na.rm = T) / sqrt(4))) %>%
  pivot_wider(
    id_cols = c(category, bmp), 
    names_from = c(costtype, State), 
    values_from = se
  ) %>%
  ungroup()


## Separate parameters for ag_capital_se and ag_operations_se and format costs data ####


if(length(Ag_BMPs) > 0) {
  temp_bmp_costs_ag_se <- merge(
    bmp_costs_se %>%
      filter(category == "ag") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  )  %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))#*#
  
  if(
    any(is.na(temp_bmp_costs_ag_se$capital)) | 
    any(is.na(temp_bmp_costs_ag_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for ag ",
        if(
          any(is.na(temp_bmp_costs_ag_se$capital)) & 
          any(is.na(temp_bmp_costs_ag_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ag_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `UserSpecs_BMPs.csv`"
      )
    )
  }
  bmp_costs_ag_capital_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ag_operations_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  bmp_costs_ag_capital_rev_se <- bmp_costs_ag_capital_se[
    order(bmp_costs_ag_capital$comid),
  ]
  ag_costs_cap_dat_se <- bmp_costs_ag_capital_rev_se[
    ,names(bmp_costs_ag_capital_rev_se) != "comid"
  ]
  ag_costs_cap_dat_se <- ag_costs_cap_dat_se %>% select(comid_form, everything())
  
  bmp_costs_ag_operations_rev_se <- bmp_costs_ag_operations_se[
    order(bmp_costs_ag_operations_se$comid),
  ]
  ag_costs_op_dat_se <- bmp_costs_ag_operations_rev_se[ 
    ,names(bmp_costs_ag_operations_rev_se) != "comid"
  ]
  ag_costs_op_dat_se <- ag_costs_op_dat_se %>% select(comid_form, everything())
} else {
  ag_costs_cap_dat_se <- data.frame(
    comid_form = NA, none = NA
  )
  ag_costs_op_dat_se <- data.frame(
    comid_form = NA, none = NA
  )
}


## Format riparian costs ####
if(length(RiparianBuffer_BMPs) > 0) {
  temp_bmp_costs_ripbuf_se <- merge(
    bmp_costs_se %>%
      filter(category == "ripbuf") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  ) %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  if(
    any(is.na(temp_bmp_costs_ripbuf_se$capital)) | 
    any(is.na(temp_bmp_costs_ripbuf_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for Riparian Buffer ",
        if(
          any(is.na(temp_bmp_costs_ripbuf_se$capital)) & 
          any(is.na(temp_bmp_costs_ripbuf_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ripbuf_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `UserSpecs_BMPs.csv`"
      )
    )
  }
  
  bmp_costs_ripbuf_capital_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ripbuf_operations_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ripbuf_capital_rev_se <- bmp_costs_ripbuf_capital_se[
    order(bmp_costs_ripbuf_capital_se$comid),
  ]
  ripbuf_costs_cap_dat_se <- bmp_costs_ripbuf_capital_rev_se[
    ,names(bmp_costs_ripbuf_capital_rev_se) != "comid"
  ]
  ripbuf_costs_cap_dat_se <- ripbuf_costs_cap_dat_se %>% 
    select(comid_form, everything())
  
  bmp_costs_ripbuf_operations_rev_se <- bmp_costs_ripbuf_operations_se[
    order(bmp_costs_ripbuf_operations_se$comid),
  ]
  ripbuf_costs_op_dat_se <- bmp_costs_ripbuf_operations_rev_se[ 
    ,names(bmp_costs_ripbuf_operations_rev_se) != "comid"
  ]
  ripbuf_costs_op_dat_se <- ripbuf_costs_op_dat_se %>% select(comid_form, everything())
  
} else {
  ripbuf_costs_cap_dat_se <- data.frame(
    comid_form = NA, none = NA
  )
  ripbuf_costs_op_dat_se <- data.frame(
    comid_form = NA, none = NA
  )
}

## Format point cost standard errors

if("point" %in% bmp_costs_se$category) {
  
  temp_bmp_costs_point <- merge(
    bmp_costs_se[bmp_costs_se$category =="point",], 
    point_comid,
    by = "bmp",
    all.x = TRUE
  ) %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) 
  
  if(any(is.na(temp_bmp_costs_point))) {
    stop(
      paste0(
        "Point costs standard errors for ", 
        paste(
          temp_bmp_costs_point %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`"
      )
    )
  }
  
  bmp_costs_point_se <- temp_bmp_costs_point %>% 
    select(c("category", "comid", "capital", "operations", "State"))
  
} else {
  bmp_costs_point_se <- data.frame(
    category = "point", comid = NA, capital = NA, operations = NA, State = NA
  )
}

## Format point source BMP costs data -----
temp_point_costs_dat_se <- bmp_costs_point_se[#@HP: duplicate point costs tracked to the temp_point_costs_dat data frame
  bmp_costs_point_se$comid %in% streamcat_subset_all$comid,
] %>% 
  select(c("comid", "State", "capital", "operations"))
temp_point_costs_other_se <- as.data.frame(
  streamcat_subset_all[
    !(streamcat_subset_all$comid %in% temp_point_costs_dat_se$comid),
  ]
)
names(temp_point_costs_other_se) <- "comid"
temp_point_costs_other_rev_se <- merge(
  temp_point_costs_other_se, COMID_State, by = c("comid"), all.x = TRUE
)
temp_point_costs_other_rev_se$capital <- 0
temp_point_costs_other_rev_se$operations <- 0
names(temp_point_costs_other_rev_se) <- c(
  "comid", "State", "capital", "operations"
)
temp_point_costs_dat_rev_se <- rbind(
  temp_point_costs_dat_se, temp_point_costs_other_rev_se
) %>%
  group_by(comid, State) %>%
  summarize(
    capital = sum(capital, na.rm = TRUE),
    operations = sum(operations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(comid)
temp_point_costs_dat_rev_se$comid_form <- paste0(
  "'", temp_point_costs_dat_rev_se$comid, "'"
)
point_costs_dat_se <- temp_point_costs_dat_rev_se %>% 
  select(c("comid_form", "capital", "operations"))


## Format urban BMP costs -----

if(length(Urban_BMPs) > 0) {
  temp_urban_costs_dat_se <- bmp_costs_se %>%
    filter(category == "urban") %>%
    select(
      bmp, contains(c("capital", "operations"))
    ) %>%
    pivot_longer(
      ., 
      cols = contains(c("capital", "operations")), 
      names_to = c("CostType", "State"), 
      names_sep = "_"
    ) %>%
    group_by(bmp, CostType) %>%
    summarise(
      index = if(length(which(!is.na(value)) > 0)) {min(which(!is.na(value)))} else {NA},
      Cost = value[index], 
      State = State[index],
      .groups = "keep"
    )
  
  if(
    any(is.na(temp_urban_costs_dat_se)) | 
    any(!(c("capital", "operations") %in% temp_urban_costs_dat_se$CostType))
  ) {
    stop(
      paste0(
        "Urban costs (capital and/or operations) for ", 
        paste(
          temp_urban_costs_dat_se %>% 
            filter(is.na(Cost)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        paste(
          c("capital", "operations")[
            which(
              !(c("capital", "operations") %in% temp_urban_costs_dat_se$CostType)
            )
          ]
        ),
        " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`"
      )
    )
  }
  
  print(
    paste0(
      "Urban base costs are assumed to be the same across states. RBEROST is using values from ",
      paste(unique(temp_urban_costs_dat_se$State), collapse = ", "), "."
    )
  )
  
  urban_costs_dat_se <- temp_urban_costs_dat_se %>%
    pivot_wider(id_cols = bmp, names_from = CostType, values_from = Cost)
  
  urban_costs_dat_se$bmp_form  <- paste0("'", urban_costs_dat$bmp, "'") 
} else {
  urban_costs_dat_se <- data.frame(
    bmp = NA, capital = NA, operations = NA, bmp_form = NA
  )
}

## Format Septic BMP Costs ----
if(length(Septic_BMPs) > 0){
  
  septic_bmp_costs_temp_se <- user_specs_BMPs %>%
    filter(BMP_Selection == "X" & BMP_Category == "septic") %>%
    select(BMP_Category, BMP, capital_VT:operations_RI) %>%
    unique() %>%
    pivot_longer(
      cols = -c(BMP_Category, BMP), 
      names_to = c("costtype", "State"), 
      names_sep = "_", 
      values_to = "Value"
    ) %>%
    group_by(
      BMP_Category, BMP, costtype
    ) %>%
    mutate(mean = mean(Value, na.rm = T), se = (sd(Value, na.rm = T) / sqrt(4))) %>%
    pivot_wider(
      id_cols = c(BMP_Category, BMP), 
      names_from = c(costtype, State), 
      values_from = se
    ) %>%
    ungroup() %>%
    group_by(BMP_Category, BMP)%>%
    summarise(
      cap_cost = mean(capital_VT:capital_RI, na.rm = TRUE),
      op_cost = mean(operations_VT:operations_RI, na.rm = TRUE),
      .groups = "drop"
    )%>%
    merge(., user_specs_BMPs%>%
            filter(BMP_Selection == "X" & BMP_Category == "septic")%>%
            select(BMP_Category, BMP, capital_VT, operations_VT), by = c("BMP_Category", "BMP")
    )%>%
    mutate(
      cap_check = ifelse(cap_cost == capital_VT, 0, 1),
      op_check = ifelse(op_cost == operations_VT, 0, 1)
    )%>%
    select(-capital_VT, -operations_VT, -cap_check, -op_check)
  
  sep_convert_bmp_costs_temp <- septic.conversion %>%
    select(-parcels, -efficiency_per_parcel) %>%
    mutate(
      BMP = "Sewer_convert"
    ) %>%
    merge(., septic_bmp_costs_temp_se%>%select(BMP, cap_cost, op_cost), by = c("BMP")
    ) %>%
    select(comid, everything())
  
  sep_upgrade_bmp_costs_temp <- septic.upgrade%>%
    rename(BMP = SepticUpgradeClass)%>%
    mutate(
      BMP = paste0("class_", BMP, "_upgrade")
    )%>%
    select(-parcels, -efficiency_per_parcel)%>%
    merge(., septic_bmp_costs_temp_se%>%select(BMP, cap_cost, op_cost), by = c("BMP")
    )%>%
    select(comid, everything())
  
  septic_costs_temp_se <- rbind(sep_convert_bmp_costs_temp, sep_upgrade_bmp_costs_temp)
  
  septic_cap_costs_se <- septic_costs_temp_se %>%
    select(comid, BMP, cap_cost) %>%
    pivot_wider(
      id_cols = comid,
      names_from = BMP, 
      values_from = cap_cost
    ) %>%
    right_join(
      data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
      by = "comid"
    ) %>%
    mutate(comid_form = paste0("'", comid, "'")) %>%
    select(comid_form, contains("convert"), contains("class"))
  
  septic_op_costs_se <- septic_costs_temp_se %>%
    select(comid, BMP, op_cost)%>%
    pivot_wider(
      id_cols = comid,
      names_from = BMP, 
      values_from = op_cost
    ) %>%
    right_join(
      data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
      by = "comid"
    ) %>%
    mutate(comid_form = paste0("'", comid, "'"))%>%
    select(comid_form, contains("convert"), contains("class"))
  
  # replace NAs in the septic_bmp_costs dataframes with 0
  septic_cap_costs_se[is.na(septic_cap_costs_se)] <- 0
  septic_op_costs_se[is.na(septic_op_costs_se)] <- 0
} else {
  septic_cap_costs_se <- data.frame(
    comid_form = NA, none = NA
  )
  septic_op_costs_se <- data.frame(
    comid_form = NA, none = NA
  )
}


## Efficiency data ####

## ACRES data - choose ####

temp_acre <- if(AgBMPcomparison == "No Practice") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice_ICF25.csv"))
} else if(AgBMPcomparison == "Baseline") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline_ICF25.csv"))
} else {
  stop(
    'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
  )
}#*#

# Read in efficiency data for ACRE database BMPs
temp_acre$bmp <- with(
  temp_acre,
  ifelse(
    Scenario=="CONSERVATION",
    "Conservation",
    ifelse(
      Scenario=="Contour Farming",
      "Contour_Farming",
      ifelse(
        Scenario=="Terraces and Waterway",
        "Terrace_Waterway",
        ifelse(
          Scenario=="Terraces Only",
          "Terrace_Only",
          ifelse(
            Scenario=="Waterway Only", "Waterway_Only", as.character(Scenario)
          )
        )
      )
    )
  )
) #*#


temp_acre$Scenario <- NULL
temp_acre$HUC12_Rev <- str_pad(temp_acre$HUC12, width=12, pad="0")
temp_acre$HUC10_Rev <- str_pad(temp_acre$HUC10, width=10, pad="0")
temp_acre$HUC8_Rev <- str_pad(temp_acre$HUC8, width=8, pad="0")

temp_acre_cast_tn_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic_se"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    } 
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tn_HUC8_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12_se <- temp_acre_cast_tn_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

ACRE_BMPs <- names(temp_acre_cast_tn_se[, -c(1:3)])

temp_acre_reaches_HUC12_tn_se <- merge(
  reaches_huc12_tn %>% select(comid, HUC12),
  temp_acre_cast_tn_HUC12_se %>% select(HUC12, any_of(ACRE_BMPs)),
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn_se <- merge(
  reaches_huc12_tn %>% select(comid, HUC10),
  temp_acre_cast_tn_HUC10_se %>% select(HUC10, any_of(ACRE_BMPs)),
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn_se <- merge(
  reaches_huc12_tn %>% select(comid, HUC8),
  temp_acre_cast_tn_HUC8_se %>% select(HUC8, any_of(ACRE_BMPs)),
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tn_se <- merge(
  merge(
    merge(
      reaches_huc12_tn, 
      temp_acre_reaches_HUC12_tn_se, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tn_se,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tn_se,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tn_se[ , ACRE_BMPs] <- NA

acre_reaches_tn_se <- temp_acre_reaches_tn_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

temp_acre_cast_tp_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic_se"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    }
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tp_HUC8_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12_se <- temp_acre_cast_tp_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

temp_acre_reaches_HUC12_tp_se <- merge(
  reaches_huc12_tp %>% select(comid, HUC12),
  temp_acre_cast_tp_HUC12_se %>% select(HUC12, any_of(ACRE_BMPs)),
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp_se <- merge(
  reaches_huc12_tp %>% select(comid, HUC10),
  temp_acre_cast_tp_HUC10_se %>% select(HUC10, any_of(ACRE_BMPs)),
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp_se <- merge(
  reaches_huc12_tp %>% select(comid, HUC8),
  temp_acre_cast_tp_HUC8_se %>% select(HUC8, any_of(ACRE_BMPs)),
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tp_se <- merge(
  merge(
    merge(
      reaches_huc12_tp, 
      temp_acre_reaches_HUC12_tp_se, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tp_se,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tp_se,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tp_se[ , ACRE_BMPs] <- NA

acre_reaches_tp_se <- temp_acre_reaches_tp_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

## Fert20 & ManureInjection ####

# Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_FertManure.csv", sep = "")
) #*#

temp_ag_effic_fert_man_se <- data.frame(
  Category = temp_ag_effic_fert_man$Category,
  BMP = temp_ag_effic_fert_man$BMP, 
  N_Efficiency = c(0.14, 0.14), 
  P_Efficiency = c(NA_real_, 0.14)
)

temp_ag_effic_fert_man_cast_tn_se <- reshape2::dcast(
  temp_ag_effic_fert_man_se, Category ~ BMP, value.var = "N_Efficiency"
) #*#
temp_ag_effic_fert_man_cast_tp_se <- reshape2::dcast(
  temp_ag_effic_fert_man_se, Category ~ BMP, value.var = "P_Efficiency"
) #*#

## Combine ACRE bmps with Manure_Injection and Fert_20 BMP-specific efficiencies ####


ag_effic_bycomid_tn_se <- add_column(
  acre_reaches_tn_se, temp_ag_effic_fert_man_cast_tn_se[-c(1)]
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) 

ag_effic_bycomid_tp_se <- add_column(
  acre_reaches_tp_se, temp_ag_effic_fert_man_cast_tp_se[-c(1)]
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) 

## format ag effic data ####

temp_ag_effic_dat_tn_se <- ag_effic_bycomid_tn_se[
  ag_effic_bycomid_tn_se$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
] %>%
  arrange(comid) %>%
  mutate(comid_form = paste0("'", comid, "'"))

ag_effic_dat_tn_se <- temp_ag_effic_dat_tn_se %>% 
  select(-comid) %>% 
  select(comid_form, everything()) 

temp_ag_effic_dat_tp_se <- ag_effic_bycomid_tp_se[
  ag_effic_bycomid_tp_se$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
] %>%
  arrange(comid) %>%
  mutate(comid_form = paste0("'", comid, "'"))

ag_effic_dat_tp_se <- temp_ag_effic_dat_tp_se %>% 
  select(-comid) %>% 
  select(comid_form, everything())

## get urban efficiency ses ####
if(length(Urban_BMPs) > 0) { 
  temp_urban_effic <-  user_specs_BMPs[
    user_specs_BMPs$BMP_Category == "urban" & user_specs_BMPs$BMP_Selection == "X",
    c("BMP_Category", "BMP", "Min_RD_in", "Max_RD_in", "UserSpec_RD_in")
  ] 
  
  urban_effic <- merge(
    temp_urban_effic, urban.effic.curves, by = "BMP"
  ) %>%
    mutate(
      expression = gsub(
        ' x', ' UserSpec_RD_in ', gsub('y ~ ', '', Best.Fit.Curve)
      )
    ) %>%
    rowwise() %>%
    mutate(iter = 1, effic = eval(parse(text = expression))) %>%
    mutate(
      standarderror = case_when(
        expression == "Coef.1 + Coef.2 * log( UserSpec_RD_in  )" ~ 
          my_propogateerror(
            vals = list(
              c(Coef.1, Coef.1_se),
              c(
                Coef.2 * log(UserSpec_RD_in), 
                case_when(
                  Coef.2 > 0 & log(UserSpec_RD_in) > 0 ~ my_propogateerror(
                    vals = list(c(Coef.2, Coef.2_se), c(log(UserSpec_RD_in), 0)),
                    method = "mult"
                  ),
                  Coef.2 == 0 | log(UserSpec_RD_in) == 0 ~ 0
                )
              )
            ), 
            method = "addsub"
          ),
        expression == 
          "Coef.1 + Coef.2 * ( UserSpec_RD_in  ^2) + Coef.3 * UserSpec_RD_in " ~ 
          my_propogateerror(
            vals = list(
              c(Coef.1, Coef.1_se),
              c(
                Coef.2 * (UserSpec_RD_in ^ 2), 
                case_when(
                  Coef.2 != 0 & UserSpec_RD_in ^ 2 != 0 ~ my_propogateerror(
                    vals = list(
                      c(Coef.2, Coef.2_se), 
                      c(
                        (UserSpec_RD_in^2),
                        my_propogateerror(
                          vals = list(c(UserSpec_RD_in, 0), c(UserSpec_RD_in, 0)),
                          method = "mult"
                        )
                      )
                    ),
                    method = "mult"
                  ),
                  Coef.2 == 0 | UserSpec_RD_in ^ 2 == 0 ~ 0
                )
              ),
              c(
                Coef.3 * UserSpec_RD_in, 
                case_when(
                  Coef.3 != 0 & UserSpec_RD_in != 0 ~  my_propogateerror(
                    vals = list(c(Coef.3, Coef.3_se), c(UserSpec_RD_in, 0)), 
                    method = "mult"
                  ),
                  Coef.3 == 0 | UserSpec_RD_in == 0 ~ 0
                )
              )
            ),
            method = "addsub"
          ),
        expression == 
          "Coef.1 + Coef.2 * (1 - exp(Coef.3 * UserSpec_RD_in  ))" ~ my_propogateerror(
            vals = list(
              c(Coef.1, Coef.1_se), 
              c(
                Coef.2 * (1 - exp(Coef.3 * UserSpec_RD_in)), 
                case_when(
                  Coef.2 != 0 & (1 - exp(Coef.3 * UserSpec_RD_in)) != 0 ~
                    my_propogateerror(
                      vals = list(
                        c(Coef.2, Coef.2_se), 
                        c(
                          (1 - exp(Coef.3 * UserSpec_RD_in)), 
                          my_propogateerror(
                            vals = list(
                              c(1,0), 
                              c(
                                exp(Coef.3 * UserSpec_RD_in), 
                                exp(Coef.3 * UserSpec_RD_in) * UserSpec_RD_in * Coef.3_se
                              )
                            ), 
                            method = "addsub"
                          )
                        )
                      ), 
                      method = "mult"
                    ),
                  Coef.2 == 0 | (1 - exp(Coef.3 * UserSpec_RD_in)) == 0 ~ 0
                )
              )
            ),
            method = "addsub"
          ),
        expression == "Coef.1 + Coef.2 * UserSpec_RD_in " ~ my_propogateerror(
          vals = list(
            c(Coef.1, Coef.1_se), 
            c(
              Coef.2 * UserSpec_RD_in, 
              case_when(
                Coef.2 != 0 & UserSpec_RD_in != 0 ~ my_propogateerror(
                  vals = list(c(Coef.2, Coef.2_se), c(UserSpec_RD_in, 0)),
                  method = "mult"
                ),
                Coef.2 == 0 | UserSpec_RD_in == 0 ~ 0
              )
            )
          ), 
          method = "addsub"
        )
      )
    ) %>%
    filter(BMP %in% user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection == "X"]) %>%
    select(
      category = BMP_Category, Pollutant,  bmp = BMP, effic, se = standarderror, InfiltrationRate_inperhr
    ) %>%
    group_by(bmp) %>%
    pivot_wider(
      id_cols = c(InfiltrationRate_inperhr, Pollutant), names_from = bmp, values_from = c(effic, se)
    ) %>%
    add_row(InfiltrationRate_inperhr = 0, Pollutant = c("N", "P")) %>%
    mutate(
      across(
        contains(
          c("Infiltration_Basin", 
            "Infiltration_Chamber",
            "Infiltration_Trench", 
            "Porous_Pavement_w_subsurface_infiltration")
        ), 
        ~ case_when(InfiltrationRate_inperhr == 0 ~ 0, TRUE ~ .)
      )
    ) %>%
    group_by(Pollutant) %>%
    fill(contains(Urban_BMPs)) %>%
    ungroup()
  
  urban_effic.n <- urban_effic %>% filter(Pollutant == "N")
  urban_effic.p <- urban_effic %>% filter(Pollutant == "P")
  
  
  ## distribution of matched infiltration rates ####
  comid.infiltrationrates.matched <- comid.infiltrationrates %>%
    rename(
      InfiltrationRate_inperhr = infiltrationrate_inperhr,
      InfiltrationRate_inperhr_dist = infiltrationrate_inperhr_dist
    ) %>%
    mutate(
      InfiltrationRate_inperhr = custom.round(
        x = InfiltrationRate_inperhr, 
        breaks = c(0, 0.17, 0.27, 0.52, 1.02, 2.41, 8.27)  # NH BMPs calculated at 0.17 in/hr, 0.27 in/hr, 0.52 in/hr, 1.02 in/hr, 2.41 in/hr and 8.27 in/hr
      )
    ) %>%
    rowwise() %>%
    mutate(
      InfiltrationRate_inperhr_dist = list(
        custom.round(
          x = strsplit(
            str_extract(
              gsub(
                pattern = "'", 
                replacement = "", 
                x = InfiltrationRate_inperhr_dist
              ), 
              pattern = "(?<=c[(]).*(?=[)])"
            ), 
            ","
          )[[1]], 
          breaks = c(0, 0.17, 0.27, 0.52, 1.02, 2.41, 8.27)  # NH BMPs calculated at 0.17 in/hr, 0.27 in/hr, 0.52 in/hr, 1.02 in/hr, 2.41 in/hr and 8.27 in/hr
        )
      )
    ) %>%
    ungroup()
  
  ## urban effic by comid ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_effic_bycomid_tn <- merge(
      comid.infiltrationrates.matched, 
      data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)), 
      by = 'comid',
      all = TRUE
    ) %>%
      full_join(., urban_effic.n, by = "InfiltrationRate_inperhr") %>%
      filter(comid %in% unlist(streamcat_subset_tn, use.names = FALSE)) %>%
      rowwise() %>%
      mutate(
        across(
          .cols = any_of(
            c(
              "se_Infiltration_Basin",
              "se_Infiltration_Chamber",
              "se_Infiltration_Trench",
              "se_Porous_Pavement_w_subsurface_infiltration"
            )
          ),
          .fns = ~ (
            sd(
              unlist(
                urban_effic.n[[
                  paste0(
                    "effic_",
                    gsub(pattern = "se_", replacement = "", x = cur_column())
                  )
                ]][
                  unlist(
                    lapply(
                      X = InfiltrationRate_inperhr_dist, 
                      FUN = match,
                      table = urban_effic.n$InfiltrationRate_inperhr
                    )
                  )
                ]
              )
            ) / sqrt(10)
          )
        )
      ) %>%
      ungroup() %>%
      mutate(across(contains("se_"), .fns = ~ replace_na(., replace = mean(., na.rm = TRUE)))) %>%
      mutate(across(contains("se_"), .fns = ~ replace_na(., replace = 0)))
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    urban_effic_bycomid_tp <- merge(
      comid.infiltrationrates.matched, 
      data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)), 
      by = 'comid',
      all = TRUE
    ) %>%
      full_join(., urban_effic.p, by = "InfiltrationRate_inperhr") %>%
      filter(comid %in% unlist(streamcat_subset_tp, use.names = FALSE)) %>%
      rowwise() %>%
      mutate(
        across(
          .cols = any_of(
            c(
              "se_Infiltration_Basin",
              "se_Infiltration_Chamber",
              "se_Infiltration_Trench",
              "se_Porous_Pavement_w_subsurface_infiltration"
            )
          ),
          .fns = ~ (
            sd(
              unlist(
                urban_effic.p[[
                  paste0(
                    "effic_",
                    gsub(pattern = "se_", replacement = "", x = cur_column())
                  )
                ]][
                  unlist(
                    lapply(
                      X = InfiltrationRate_inperhr_dist, 
                      FUN = match,
                      table = urban_effic.p$InfiltrationRate_inperhr
                    )
                  )
                ]
              )
            ) / sqrt(10)
          )
        )
      ) %>%
      ungroup() %>%
      mutate(across(contains("se_"), .fns = ~ replace_na(., replace = mean(., na.rm = TRUE)))) %>%
      mutate(across(contains("se_"), .fns = ~ replace_na(., replace = 0)))
  }
  
  ## Format urban BMP efficiency data ####
  
  temp_urban_effic_dat_tn_se <- urban_effic_bycomid_tn[
    urban_effic_bycomid_tn$comid %in% 
      unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tn_se <- temp_urban_effic_dat_tn_se %>%
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list( ~gsub("se_", "", .)))
  
  temp_urban_effic_dat_tp_se <- urban_effic_bycomid_tp[
    urban_effic_bycomid_tp$comid %in% 
      unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tp_se <- temp_urban_effic_dat_tp_se %>% 
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list( ~gsub("se_", "", .)))
  
  ##  Define urban cost correction coefficients ####
  # These are based on the type of urban land. The costs given in UserSpecs are for new development. 
  # Retrofitting is multiplied by a factor of 2 and difficult retrofit by a factor of 3.
  # Open and low density development we assume will be new development, med density will be retrofits, and high density will be difficult retrofits.
  
  urban_cost_coeffs_se <- StreamCat_api %>%
    filter(comid %in% unlist(streamcat_subset_all)) %>%
    select(comid, PctUrbOp2019Cat, PctUrbLo2019Cat, PctUrbMd2019Cat, PctUrbHi2019Cat) %>%
    filter(!duplicated(comid)) %>% # was 'unique()', but that passed some corruption at the 15th decimal place and included extra lines in the dataset
    rowwise() %>%
    mutate(
      total_urban = PctUrbOp2019Cat + 
        PctUrbLo2019Cat + 
        PctUrbMd2019Cat + 
        PctUrbHi2019Cat,
      urban_cost_coef_se = case_when(
        total_urban == 0 ~ 0,
        total_urban > 0 ~ sd(
          c(sample(
            x = c(
              rep(
                x = 1,
                length.out = floor(
                  ((PctUrbOp2019Cat + PctUrbLo2019Cat) / total_urban) * 100
                )
              ) ,
              rep(x = 2, length.out = floor((PctUrbMd2019Cat / total_urban) * 100)) ,
              rep(x = 3, length.out = floor((PctUrbHi2019Cat / total_urban) * 100))
            ),
            size = 10,
            replace = TRUE
          )
          )
        ) / sqrt(10)
      )
    ) %>%
    select(comid, urban_cost_coef_se) %>%
    ungroup()
  
  # Format Urban Cost Coefficient Error Data
  urban_cost_coeffs_dat_se <- urban_cost_coeffs_se %>%
    arrange('comid') %>%
    mutate(comid_form = paste0("'", comid, "'")) %>%
    select(comid_form, urban_cost_coef_se)
  
}

## KM: Set Point BMP efficiencies to 0
if("point" %in% bmp_costs$category) {
  point_effic_bycomid_tn_se <- point_effic_bycomid_tn %>% 
    mutate(effic = 0)
  
  point_effic_bycomid_tp_se <- point_effic_bycomid_tp %>% 
    mutate(effic = 0)
} else {
  point_effic_bycomid_tn_se <- data.frame(
    BMP_Category = "point", comid = NA, effic = NA
  )
  point_effic_bycomid_tp_se <- data.frame(
    BMP_Category = "point", comid = NA, effic = NA
  )
}

### TN point Source Efficiencies
if (any(point_effic_bycomid_tn_se$comid %in% 
        unlist(lapply(
          streamcat_subset_tn,
          function(x) x[!is.na(x$comid), ]
        ), use.names = FALSE))) {
  temp_point_effic_dat_tn_se <- point_effic_bycomid_tn_se[
    point_effic_bycomid_tn_se$comid %in%
      unlist(lapply(
        streamcat_subset_tn,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE),
  ] %>%
    select(c("comid", "effic")) 
} else {
  temp_point_effic_dat_tn_se <- data.frame(
    comid = point_effic_bycomid_tn_se$comid,
    effic = rep(0, nrow(point_effic_bycomid_tn_se))
  )
}

temp_point_effic_other_tn_se <- data.frame(
  comid = unlist(
    streamcat_subset_tn, use.names = FALSE
  )[
    !(
      unlist(streamcat_subset_tn, use.names = FALSE) %in% 
        temp_point_effic_dat_tn_se$comid
    )
  ]
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  temp_point_effic_other_tn_se$effic <- 0
  temp_point_effic_dat_rev_tn_se <- rbind(
    temp_point_effic_dat_tn_se, temp_point_effic_other_tn_se
  ) %>% 
    group_by(comid) %>%
    summarize(effic = mean(effic, na.rm = TRUE), .groups = "drop") %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  point_effic_dat_tn_se <- temp_point_effic_dat_rev_tn %>% 
    select(c("comid_form", "effic"))
}

### TP point Source Efficiencies
if (any(point_effic_bycomid_tp_se$comid %in% 
        unlist(lapply(
          streamcat_subset_tp,
          function(x) x[!is.na(x$comid), ]
        ), use.names = FALSE))) {
  temp_point_effic_dat_tp_se <- point_effic_bycomid_tp_se[
    point_effic_bycomid_tp_se$comid %in%
      unlist(lapply(
        streamcat_subset_tp,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE),
  ] %>%
    select(c("comid", "effic")) 
} else {
  temp_point_effic_dat_tp_se <- data.frame(
    comid = point_effic_bycomid_tp_se$comid,
    effic = rep(0, nrow(point_effic_bycomid_tp_se))
  )
}

temp_point_effic_other_tp_se <- data.frame(
  comid = unlist(
    lapply(
      streamcat_subset_tp,
      function(x) x[!is.na(x$comid), ]
    ), use.names = FALSE
  )[
    !(
      unlist(lapply(
        streamcat_subset_tp,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE) %in% 
        temp_point_effic_dat_tp$comid
    )
  ]
)

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(nrow(temp_point_effic_dat_tp_se) > 0) {
    
    if(exists("temp_point_effic_other_tp_se") && nrow(temp_point_effic_other_tp_se) > 0) {
      temp_point_effic_other_tp_se$effic <- 0
      
      temp_point_effic_dat_rev_tp_se <- rbind(
        temp_point_effic_dat_tp_se, temp_point_effic_other_tp_se
      ) %>% 
        group_by(comid) %>%
        summarize(effic = mean(effic, na.rm = TRUE), .groups = "drop") %>%
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
      point_effic_dat_tp_se <- temp_point_effic_dat_rev_tp_se %>% 
        select(c("comid_form", "effic"))
    } else {
      temp_point_effic_dat_rev_tp_se <- temp_point_effic_dat_tp_se %>% 
        group_by(comid) %>%
        summarize(effic = mean(effic, na.rm = TRUE), .groups = "drop") %>%
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
      point_effic_dat_tp_se <- temp_point_effic_dat_rev_tp_se %>% 
        select(c("comid_form", "effic"))
    }
    
  } else {
    point_effic_dat_tp_se <- data.frame(
      comid_form = character(),
      effic = numeric()
    )
  }
}

## KM: Set Septic BMP efficiencies to 0
if(length(Septic_BMPs) > 0) {
  septic_effic_dat_tn_se <- septic_effic_dat_tn %>% 
    mutate(across(any_of(Septic_BMPs), ~ 0))
  
} else {
  septic_effic_dat_tn_se <- data.frame(
    comid = NA, comid_form = NA, effic = NA
  )
}

# Write Command Script ####

cat(
  "
#WMOST Optimization Screening Tool AMPL command file with uncertainty

option display_transpose -10000;
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "\n"
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("\ndisplay loads_lim_N", i, ";"),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0("\ndisplay loads_lim_P", i, ";"),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

cat(
  "
for {x in Scenarios} {

if x > 1
  then {
  ",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "",
  append = T
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("\nfor {s in seasons, y in years} {
               \nlet loads_lim_N", i, "[s,y, 'limits']:= loads_lim_N", i, "[s,y,'limits'] * ", scenarioincrement, ";}"),
        file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0("\nfor {s in seasons, y in years} {\nlet loads_lim_P", i, "[s,y,'limits']:= loads_lim_P", i, "[s,y,'limits'] * ", scenarioincrement, ";}"),
        file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

cat(
  "
  };
solve;

for {b in Bootstraps} {
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "",
  append = T
)

cat(
  "
	let agcost_frac_rev[b] := Uniform(1,5/3);

  for {u in urban_bmp} {
	  for {o in cost_type} {
	    repeat {
	      let urban_costs_rev[u,o,b] := Normal(urban_costs[u,o], urban_costs_se[u,o]);
	    } until urban_costs_rev[u,o,b] >= 0;
	  };
	 };
	 ",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "",
  append = T
)


invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nfor {c in comid_N",
          i,
          "} {\nfor {s in seasons} {
            for {y in years} {
              for {l in loads_N} {\nrepeat {\nlet  baseloads_N",
          i,
          "_rev[c,s,y,l,b] := Normal(baseloads_N",
          i,
          "[c,s,y,l], baseloads_N",
          i,
          "_se[c,s,y,l]);\n} until baseloads_N",
          i,
          "_rev[c,s,y,l,b] >= 0;\n};\nrepeat {
          let transfer_coefficients_N",
          i,
          "_rev[c,s,y,'coeff',b] := Normal(transfer_coefficients_N",
          i,
          "[c,s,y,'coeff'], transfer_coefficients_N",
          i,
          "_se[c,s,y,'coeff']);\n} until transfer_coefficients_N",
          i,
          "_rev[c,s,y,'coeff',b] <= 1;\nfor {r in ripbuf_bmp} {\nlet riparianremoval_N",
          i,
          "_rev[c,s,y,r,b] := Normal(riparianremoval_N",
          i,
          "[c,s,y,r], riparianremoval_N",
          i,
          "_se[c,s,y,r]);\n  }; \n};  \n};  \n};"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
for {c in comid_all_N} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_N_rev[c,a,b] := Normal(ag_effic_N[c,a], ag_effic_N_se[c,a]);
    } until ag_effic_N_rev[c,a,b] <= 1;
  };
  for {p in point_c} {
    repeat {
      let point_effic_N_rev[c,p,b] := Normal(point_effic_N[c,p], point_effic_N_se[c,p]);
    } until point_effic_N_rev[c,p,b] <= 1;
  };
  for {u in urban_bmp} {
    repeat {
      let urban_effic_N_rev[c,u,b] := Normal(urban_effic_N[c,u], urban_effic_N_se[c,u]);
    } until urban_effic_N_rev[c,u,b] <= 1;
  };
  for {e in septic_bmp} {
    repeat {
      let septic_effic_N_rev[c,e,b] := Normal(septic_effic_N[c,e], septic_effic_N_se[c,e]);
    } until septic_effic_N_rev[c,e,b] <= 1;
  };
};
",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    sep = "",
    append = T
  )
}

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\nfor {c in comid_P",
          i,
          "} {\nfor {s in seasons} {
          for {y in years} { 
          for {l in loads_P} {\nrepeat {\nlet  baseloads_P",
          i,
          "_rev[c,s,y,l,b] := Normal(baseloads_P",
          i,
          "[c,s,y,l], baseloads_P",
          i,
          "_se[c,s,y,l]);\n} until baseloads_P",
          i,
          "_rev[c,s,y,l,b] >= 0;\n};\nrepeat {
          let transfer_coefficients_P",
          i,
          "_rev[c,s,y,'coeff',b] := Normal(transfer_coefficients_P",
          i,
          "[c,s,y,'coeff'], transfer_coefficients_P",
          i,
          "_se[c,s,y,'coeff']);\n} until transfer_coefficients_P",
          i,
          "_rev[c,s,y,'coeff',b] <= 1;\nfor {r in ripbuf_bmp} {\nlet riparianremoval_P",
          i,
          "_rev[c,s,y,r,b] := Normal(riparianremoval_P",
          i,
          "[c,s,y,r], riparianremoval_P",
          i,
          "_se[c,s,y,r]);\n  };\n};\n};\n};"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "",
        append = T
      )
    }
  }
)

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
for {c in comid_all_P} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_P_rev[c,a,b] := Normal(ag_effic_P[c,a], ag_effic_P_se[c,a]);
    } until ag_effic_P_rev[c,a,b] <=1;
  };
  for {p in point_c} {
    let point_effic_P_rev[c,p,b] := Normal(point_effic_P[c,p], point_effic_P_se[c,p]);
  };
  for {u in urban_bmp} {
    repeat {
      let urban_effic_P_rev[c,u,b] := Normal(urban_effic_P[c,u], urban_effic_P_se[c,u]);
    } until urban_effic_P_rev[c,u,b] <= 1;
  };
};",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    sep = "\n",
    append = TRUE
  )
}

cat(
  "
for {c in comid_all} {

  for {o in cost_type} {
    repeat {
      let point_costs_rev[c,o,b] := Normal(point_costs[c,o], point_costs_se[c,o]);
    } until point_costs_rev[c,o,b] >= 0;
  };

  for {r in ripbuf_bmp} {
    repeat {
      let ripbuf_costs_capital_rev[c,r,b] := Normal(ripbuf_costs_capital[c,r], ripbuf_costs_capital_se[c,r]);
    } until ripbuf_costs_capital_rev[c,r,b] >= 0;
    repeat {
      let ripbuf_costs_operations_rev[c,r,b] := Normal(ripbuf_costs_operations[c,r], ripbuf_costs_operations_se[c,r]);
    } until ripbuf_costs_operations_rev[c,r,b] >= 0;
  };

  for {ar in area_sub} {
    repeat {
      let area_rev[c,ar,b] := Normal(area[c,ar], area_se[c,ar]);
    } until area_rev[c,ar,b] >= 0;
  };

  for {a in ag_bmp} {
    repeat {
      let ag_costs_capital_rev[c,a,b] := Normal(ag_costs_capital[c,a], ag_costs_capital_se[c,a]);
    } until ag_costs_capital_rev[c,a,b] >= 0;
    repeat {
      let ag_costs_operations_rev[c,a,b] :=  Normal(ag_costs_operations[c,a], ag_costs_operations_se[c,a]);
    } until ag_costs_operations_rev[c,a,b] >= 0;
  };

  repeat {
    let runoff_coeff_urban_rev[c,'urban',b] := Normal(runoff_coeff_urban[c,'urban'], runoff_coeff_urban_se[c,'urban']);
  } until 1 >= runoff_coeff_urban_rev[c,'urban',b] >= 0;

  repeat {
    let urban_cost_adjustment_coef_rev[c,b] := Normal(urban_cost_adjustment_coef[c], urban_cost_adjustment_coef_se[c]);
  } until urban_cost_adjustment_coef_rev[c,b] >= 0;

};
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "\n",
  append = TRUE
)

# Add septic costs loop only for TN watersheds
if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
for {c in comid_all_N} {
  for {e in septic_bmp} {
    repeat {
      let septic_costs_capital_rev[c,e,b] := Normal(septic_costs_capital[c,e], septic_costs_capital_se[c,e]);
    } until septic_costs_capital_rev[c,e,b] >= 0;
    repeat {
      let septic_costs_operations_rev[c,e,b] :=  Normal(septic_costs_operations[c,e], septic_costs_operations_se[c,e]);
    } until septic_costs_operations_rev[c,e,b] >= 0;
  };
};
",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    sep = "\n",
    append = TRUE
  )
}

cat(
  "
};
option cplex_options 'absmipgap 1.0';
option display_precision 10;
display solve_result_num, solve_result;
display cost.result;
display cost;
option display_1col 10000000000;
option omit_zero_rows 1;
option omit_zero_cols 1;
",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "\n",
  append = TRUE
)

### Create Optimization Equation ------
cat(
  "display {b in Bootstraps} (sum {c in comid_all} (ps_coef_rev[c,b] * point_dec[c]) + 
sum {c in comid_all, u in urban_bmp} (urban_coef_rev[c,u,b] * urban_frac[c,u]) + 
sum {c in comid_all, a in ag_bmp} (ag_coef_rev[c,a,b] * ag_frac[c,a]) +",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
  sep = "\n", 
  append = T
)

# Add septic cost component only if septic BMPs are enabled
if(length(Septic_BMPs) > 0) {
  cat(
    "sum {c in comid_all, e in septic_bmp} (septic_coef_rev[c,e,b] * septic_frac[c,e]) +",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
    sep = "\n", 
    append = T
  )
}

cat(
  "sum {c in comid_all, r in ripbuf_bmp} (ripbuf_coef_rev[c,r,b] * ripbuf_length[c,r]));
  \n",
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
  sep = "\n", 
  append = T
)

# Function to create revised septic terms conditionally
create_septic_terms_rev <- function() {
  if(length(Septic_BMPs) > 0) {
    return(paste0(
      "(baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'point',b] + ((baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'septic',b]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N_rev[c,'Sewer_convert',b])) * (1 - (point_effic_N_rev[c,'point',b] * point_dec[c])) + \n",
      "          (baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'septic',b] - sum {e in septic_bmp} (baseloads_N", "PLACEHOLDER_I", "_rev [c,s,y,'septic',b]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N_rev[c,e,b])"
    ))
  } else {
    return("baseloads_NPLACEHOLDER_I_rev[c,s,y,'point',b] * (1 - (point_effic_N_rev[c,'point',b] * point_dec[c]))")
  }
}


if(actionable_load == TRUE){
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        
        # Create septic terms for this watershed
        septic_terms_rev <- create_septic_terms_rev()
        septic_terms_rev <- gsub("PLACEHOLDER_I", i, septic_terms_rev)
        
        cat(
          paste0(
            "for {c in comid_N", i, ", s in seasons, y in years, b in Bootstraps} {\n
            let inc_storage_load_N",i,"_rev[c,s,y,b] :=
          if s=first(seasons) then (if y = first(years) then ((baseloads_N", i, "_rev[c,s,y,'storage',b] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,b] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_N_rev[c,u,b] * urban_frac[c,u])) + \n
          ", septic_terms_rev, " - \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "_rev[c,s,y,r,b]))) else ((inc_storage_load_N", i, "_rev[c,4,y-1,b] * transfer_coefficients_N", i, "_rev[c,4,y-1,'coeff',b]) + \n
          baseloads_N", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,b] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_N_rev[c,u,b] * urban_frac[c,u])) + \n
          ", septic_terms_rev, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "_rev[c,s,y,r,b]))) else ((inc_storage_load_N", i, "_rev[c,s-1,y,b] * transfer_coefficients_N", i, "_rev[c,s-1,y,'coeff',b]) + \n
          baseloads_N", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,b] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_N_rev[c,u,b] * urban_frac[c,u])) + \n
          ", septic_terms_rev, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "_rev[c,s,y,r,b]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  ) #equation version without other load component
  print("Basing TN incremental equations only on actionable load components")
}else{
  print("Basing TN incremental equations on all load components")
}
if(actionable_load == FALSE){
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        # Create conditional septic terms for complex equation
        if(length(Septic_BMPs) > 0) {
          septic_point_term_rev <- paste0("((baseloads_N", i, "_rev[c,s,y,'septic',b]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N_rev[c,'Sewer_convert',b])")
          septic_reduction_term_rev <- paste0("(baseloads_N", i, "_rev[c,s,y,'septic',b] - sum {e in septic_bmp} (baseloads_N", i, "_rev[c,s,y,'septic',b]/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N_rev[c,e,b])")
        } else {
          septic_point_term_rev <- "0"
          septic_reduction_term_rev <- "0"
        }
        
        cat(
          paste0(
            "for {c in comid_N", i, ", s in seasons, y in years, b in Bootstraps} {\n
            let inc_storage_load_N",i,"_rev[c,s,y,b] :=
          if s=first(seasons) then (if y = first(years) then ((baseloads_N", i, "_rev[c,s,y,'storage',b] + \n
          baseloads_N", i, "_rev[c,s,y,'other',b] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,b] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_N_rev[c,u,b] * urban_frac[c,u])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',b] + ", septic_point_term, ") * (1 - (point_effic_N_rev[c,'point',b] * point_dec[c])) + \n
          ", septic_reduction_term, " - \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "_rev[c,s,y,r,b]))) else ((inc_storage_load_N", i, "_rev[c,4,y-1,b] * transfer_coefficients_N", i, "_rev[c,4,y-1,'coeff',b]) + \n
          baseloads_N", i, "_rev[c,s,y,'other',b] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,b] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_N_rev[c,u,b] * urban_frac[c,u])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',b] + ", septic_point_term, ") * (1 - (point_effic_N_rev[c,'point',b] * point_dec[c])) + \n
          ", septic_reduction_term, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "_rev[c,s,y,r,b]))) else ((inc_storage_load_N", i, "_rev[c,s-1,y,b] * transfer_coefficients_N", i, "_rev[c,s-1,y,'coeff',b]) + \n
          baseloads_N", i, "_rev[c,s,y,'other',b] + \n
          baseloads_N", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,b] * ag_frac[c,a])) + \n
          baseloads_N", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_N_rev[c,u,b] * urban_frac[c,u])) + \n
          (baseloads_N", i, "_rev[c,s,y,'point',b] + ", septic_point_term, ") * (1 - (point_effic_N_rev[c,'point',b] * point_dec[c])) + \n
          ", septic_reduction_term, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "_rev[c,s,y,r,b]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  )#equation version with other load component
  print("TN Incremental equations based on all load components")
}else{
  print("TN Incremental equations based only on actionable load components")
}

if(actionable_load == TRUE){
  invisible(
    if ("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        cat(
          paste0(
            "for {c in comid_P", i, ", s in seasons, y in years, b in Bootstraps} {\n
            let inc_storage_load_P",i,"_rev[c,s,y,b] :=
      if s=first(seasons) then (if y = first(years) then (baseloads_P", i, "_rev[c,s,y,'storage',b] + \n
      baseloads_P", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,b] * ag_frac[c,a])) + \n
      baseloads_P", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_P_rev[c,u,b] * urban_frac[c,u])) + \n 
      baseloads_P", i, "_rev[c,s,y,'point',b] * (1 - (point_effic_P_rev[c,'point',b] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "_rev[c,s,y,r,b])) else ((inc_storage_load_P", i, "_rev[c,4,y-1,b] * transfer_coefficients_P", i, "_rev[c,4,y-1,'coeff',b]) + \n
      baseloads_P", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,b] * ag_frac[c,a])) + \n
      baseloads_P", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_P_rev[c,u,b] * urban_frac[c,u])) + \n
      baseloads_P", i, "_rev[c,s,y,'point',b] * (1 - (point_effic_P_rev[c,'point',b] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "_rev[c,s,y,r,b]))) else ((inc_storage_load_P", i, "_rev[c,s-1,y,b] * transfer_coefficients_P", i, "_rev[c,s-1,y,'coeff',b]) + \n
      baseloads_P", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,b] * ag_frac[c,a])) + \n
      baseloads_P", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_P_rev[c,u,b] * urban_frac[c,u])) + \n
      baseloads_P", i, "_rev[c,s,y,'point',b] * (1 - (point_effic_P_rev[c,'point',b] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "_rev[c,s,y,r,b]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  ) #equation version without other load component
  print("Basing TP incremental equations only on actionable load components")
}else{
  print("Basing TP incremental equations on all load components")
}

if(actionable_load == FALSE){
  invisible(
    if ("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        cat(
          paste0(
            "for {c in comid_P", i, ", s in seasons, y in years, b in Bootstraps}{\n
            let inc_storage_load_P",i,"_rev[c,s,y,b] :=
      if s=first(seasons) then (if y = first(years) then (baseloads_P", i, "_rev[c,s,y,'storage',b] + \n
      baseloads_P", i, "_rev[c,s,y,'other',b] + \n
      baseloads_P", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,b] * ag_frac[c,a])) + \n
      baseloads_P", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_P_rev[c,u,b] * urban_frac[c,u])) + \n 
      baseloads_P", i, "_rev[c,s,y,'point',b] * (1 - (point_effic_P_rev[c,'point',b] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "_rev[c,s,y,r,b])) else ((inc_storage_load_P", i, "_rev[c,4,y-1,b] * transfer_coefficients_P", i, "_rev[c,4,y-1,'coeff',b]) + \n
      baseloads_P", i, "_rev[c,s,y,'other',b] + \n
      baseloads_P", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,b] * ag_frac[c,a])) + \n
      baseloads_P", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_P_rev[c,u,b] * urban_frac[c,u])) + \n
      baseloads_P", i, "_rev[c,s,y,'point',b] * (1 - (point_effic_P_rev[c,'point',b] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "_rev[c,s,y,r,b]))) else ((inc_storage_load_P", i, "_rev[c,s-1,y,b] * transfer_coefficients_P", i, "_rev[c,s-1,y,'coeff',b]) + \n
      baseloads_P", i, "_rev[c,s,y,'other',b] + \n
      baseloads_P", i, "_rev[c,s,y,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a,b] * ag_frac[c,a])) + \n
      baseloads_P", i, "_rev[c,s,y,'urban',b] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u,b] * urban_frac[c,u])) + \n
      baseloads_P", i, "_rev[c,s,y,'point',b] * (1 - (point_effic_P[c,'point',b] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "_rev[c,s,y,r,b]));\n};"
          ),
          file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  ) #equation version with other load component
  print("Incremental TP equations based on all load components")
}else{
  print("Incremental TP equations based only on actionable load components")
}

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\ndisplay {b in Bootstraps, s in seasons, y in years} (sum{c in comid_N",i,"} (inc_storage_load_N",i,"_rev[c,s,y,b]));"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "\n",
        append = TRUE
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\ndisplay {b in Bootstraps, s in seasons, y in years} (sum{c in comid_P",i,"} (inc_storage_load_P",i,"_rev[c,s,y,b]));"
        ),
        file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
        sep = "\n",
        append = TRUE
      )
    }
  }
)

cat(
  paste0("\ndisplay seasons;\ndisplay years;
  \ndisplay point_dec;
         \noption display_1col 0; \ndisplay ripbuf_length;\noption display_width 100000000000;\ndisplay urban_frac;
         \ndisplay ag_frac;\n}"),
  file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
  sep = "\n",
  append = TRUE
)
# If septic data is available, add septic-specific commands
if(length(Septic_BMPs) > 0 & septic_data_available) {
  cat(
    "display septic_frac;\n",
    file = paste(OutPath, "STcommand_dynamic_uncertainty.amp", sep=""),
    append = TRUE
  )
}

# Write Data Script ####

orig.dat <- read.table(
  file = paste(OutPath, "STdata_seasonal.dat", sep=""), 
  header = FALSE,
  sep = "\n",
  quote = ""
)

write.table(
  orig.dat, # selecting forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(inc_tn_dat_se)) %do% {
      write(
        paste0("\nparam baseloads_N", i, "_se : 'point' 'urban' 'ag' 'septic' 'storage' 'other' :="), 
        file =  paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
        append = T
      )
      write.table(
        inc_tn_dat_se[[i]] %>% 
          mutate(year = as.numeric(year)) %>%
          select(comid_form, season, year, point = sin_poin, urban = sin_urb, ag = sin_ag, septic = sin_septic, storage = sin_storage, other = sin_other), # renaming also forces the order incase they are disordered in processing code above
        file =  paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file =   paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(inc_tp_dat_se)) %do% {
      if (!is.null(inc_tp_dat_se[[i]]) && nrow(inc_tp_dat_se[[i]]) > 0) {
        write(
          paste0("\nparam baseloads_P", i, "_se : 'point' 'urban' 'ag' 'storage' 'other' :="), 
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          inc_tp_dat_se[[i]] %>% 
            mutate(year = as.numeric(year)) %>%
            select(comid_form, season, year, point = sip_poin, urban = sip_urb, ag = sip_ag, storage = sip_storage, other = sip_other), # renaming also forces the order incase they are disordered in processing code above
          file =  paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
        write( ";", file =  paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
      }
    }
  }
)


### TN Storage Coefficients ------
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(sm_N_dat_se)) %do% {
      
      temp_sm_N_dat_se <- sm_N_dat_se[[i]]
      
      if (!is.null(temp_sm_N_dat_se) && nrow(temp_sm_N_dat_se) > 0) {
        
        # Write the header for the storage coefficients
        write(
          paste0("\nparam transfer_coefficients_N", i, "_se : 'coeff' :="),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
        
        # Write the data
        write.table(
          temp_sm_N_dat_se %>% 
            mutate(year = as.numeric(year)) %>%
            mutate(across(everything(), ~replace(., is.na(.), 0))),  # Replace NA with 0
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "",
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = TRUE)
      } else {
        # If the data is NULL or empty, write a message
        write(
          paste0("\nparam transfer_coefficients_N", i, "_se : 'coeff' :=\n;"),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
      }
    }
  }
)

### TP Storage Coefficients -----
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(sm_P_dat_se)) %do% {
      
      temp_sm_P_dat_se <- sm_P_dat_se[[i]]
      
      if (!is.null(temp_sm_P_dat_se) && nrow(temp_sm_P_dat_se) > 0) {
        
        # Write the header for the storage coefficients
        write(
          paste0("\nparam transfer_coefficients_P", i, "_se : 'coeff' :="),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
        
        # Write the data
        write.table(
          temp_sm_P_dat_se %>% 
            mutate(year = as.numeric(year)) %>%
            mutate(across(everything(), ~replace(., is.na(.), 0))),  # Replace NA with 0
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "",
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = TRUE)
      } else {
        # If the data is NULL or empty, write a message
        write(
          paste0("\nparam transfer_coefficients_P", i, "_se : 'coeff' :=\n;"),
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
      }
    }
  }
)

### TN Riparian Data ------
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(riparian_tn_dat)) %do% {
      if (!is.null(riparian_tn_dat[[i]]) && nrow(riparian_tn_dat[[i]]) > 0) {
        
        temp_ripar <- riparian_tn_dat[[i]]
        
        # Write the header for the table
        write(
          paste0("\nparam riparianload_N", i, "_se : 'riparian' :="), #*#,
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_ripar %>%
          mutate(year = as.numeric(year)) %>%
          select(
            comid_form,
            season,
            year,
            riparian = sin_riparian
          ) %>%
          arrange(year, comid_form, season) %>%
          # replace NA values with 0
          mutate(riparian = ifelse(is.na(riparian), 0, riparian))
        
        if(length(RiparianBuffer_BMPs) > 0) {
          # Write the formatted data to the file
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
            append = TRUE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
        } else {
          write.table(
            ripbuf_bmp_dummy_tn[[i]] %>%
              select(comid, none = none), # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = TRUE)
      }
    }
  }
)

### TP Riparian Data -----
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(riparian_tp_dat)) %do% {
      
      if (!is.null(riparian_tp_dat[[i]]) && nrow(riparian_tp_dat[[i]]) > 0) {
        
        temp_ripar <- riparian_tp_dat[[i]]
        
        # Write the header for the table
        write(
          paste0("\nparam riparianload_P", i, "_se : 'riparian' :="), #*#,
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = TRUE
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_ripar %>%
          mutate(year = as.numeric(year)) %>%
          select(
            comid_form,
            season,
            year,
            riparian = sip_riparian
          ) %>%
          arrange(year, comid_form, season) %>%
          # replace NA values with 0
          mutate(riparian = ifelse(is.na(riparian), 0, riparian))
        
        if(length(RiparianBuffer_BMPs) > 0) {
          # Write the formatted data to the file
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
            append = TRUE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
        } else {
          write.table(
            ripbuf_bmp_dummy_tp[[i]] %>%
              select(comid, none = none), # renaming also forces the order incase they are disordered in preprocessing code above
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = TRUE)
      }
    }
  }
)

write( 
  "\n\nparam area_se : 'urban' 'ag' :=", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T
)
write.table(
  area_dat %>% select(comid_form, urban = urban_ac_se, ag = ag_ac_se), # renaming also forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      if(length(RiparianBuffer_BMPs) > 0) {
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            "_se : ", 
            paste(bmp_ripbuf_vec, collapse  = "  "), 
            " :="
          ), 
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = T
        )
        # Check if riparian_tn_removal_se[[i]] is not NULL and has rows
        if (!is.null(riparian_tn_removal_se[[i]]) && nrow(riparian_tn_removal_se[[i]]) > 0) {
          write.table(
            riparian_tn_removal_se[[i]] %>% 
              mutate(year = as.numeric(year)) %>%
              select(comid_form, season, year, any_of(RiparianBuffer_BMPs)), # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
      } else {
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            "_se : 'none' :="
          ), 
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          ripbuf_bmp_dummy_tn[[i]] %>% 
            select(comid, none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      if(length(RiparianBuffer_BMPs) > 0) {
        if (!is.null(riparian_tp_removal_se[[i]]) && nrow(riparian_tp_removal_se[[i]]) > 0) {
          write(
            paste0(
              "\nparam riparianremoval_P", 
              i, 
              "_se : ", 
              paste(bmp_ripbuf_vec, collapse  = "  "), 
              " :="
            ), 
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
            append = T
          )
          write.table(
            riparian_tp_removal_se[[i]] %>% 
              mutate(year = as.numeric(year)) %>%
              select(comid_form, season, year, any_of(RiparianBuffer_BMPs)), # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
      } else {
        write(
          paste0(
            "\nparam riparianremoval_P", 
            i, 
            "_se : 'none' :="
          ), 
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          ripbuf_bmp_dummy_tp[[i]] %>% 
            select(comid, none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
    }
  }
)

if(length(Ag_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_N_se : ",paste(bmp_ag_vec, collapse  = "  ")," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tn_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_P_se : ",paste(bmp_ag_vec, collapse  = "  ")," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tp_se %>% select(comid_form, all_of(bmp_ag_vec_direct)),  # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    cat(
      "\nparam ag_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tp)) %>% select(comid, none),  # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_N_se : ",paste(bmp_urban_vec, collapse = " ")," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(urban_effic_dat_tn_se %>% select(comid_form, all_of(bmp_urban_vec_direct))), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tn) %>% 
               select(comid, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_P_se : ",paste(bmp_urban_vec, collapse = " ")," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(urban_effic_dat_tp_se %>% select(comid_form, all_of(bmp_urban_vec_direct))), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tp) %>% 
               select(comid, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(length(Septic_BMPs) > 0) {
    cat(
      "\nparam septic_effic_N_se : ",paste(bmp_septic_vec, collapse  = "  ")," :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(septic_effic_dat_tn_se %>% select(comid_form, all_of(bmp_septic_vec_direct))), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam septic_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", septic_bmp_dummy_tn) %>% 
               select(comid_form, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}
# Check if WWTPs exist in current watershed for SE parameters
has_wwtp_tn_se <- exists("point_effic_dat_tn_se") && 
  nrow(point_effic_dat_tn_se %>% filter(comid_form %in% paste0("'", streamcat_subset_tn$comid, "'"))) > 0

has_wwtp_tp_se <- exists("point_effic_dat_tp_se") && 
  nrow(point_effic_dat_tp_se %>% filter(comid_form %in% paste0("'", streamcat_subset_tp$comid, "'"))) > 0

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(has_wwtp_tn_se) {
    cat(
      "\nparam point_effic_N_se : 'point' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(point_effic_dat_tn_se %>% 
        filter(comid_form %in% paste0("'", streamcat_subset_tn$comid, "'")) %>%  # Filter to only current watershed COMIDs
        select(comid_form, point = effic)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  } else {
    # No WWTPs in watershed - write dummy parameter with 'point' column header and zero efficiency
    cat(
      "\nparam point_effic_N_se : 'point' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep = "", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", lapply(streamcat_subset_tn, function(x) data.frame(comid = x$comid)))) %>%
        mutate(comid_form = paste0("'", comid, "'"), point = 0) %>%
        select(comid_form, point),
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  if(has_wwtp_tp_se) {
    cat(
      "\nparam point_effic_P_se : 'point' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(point_effic_dat_tp_se %>% 
        filter(comid_form %in% paste0("'", streamcat_subset_tp$comid, "'")) %>%  # Filter to only current watershed COMIDs
        select(comid_form, point = effic)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  } else {
    # No WWTPs in watershed - write dummy parameter with 'point' column header and zero efficiency
    cat(
      "\nparam point_effic_P_se : 'point' :=	", 
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
      sep = "", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", lapply(streamcat_subset_tp, function(x) data.frame(comid = x$comid)))) %>%
        mutate(comid_form = paste0("'", comid, "'"), point = 0) %>%
        select(comid_form, point),
      file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "\nparam ripbuf_costs_capital_se : ", paste(bmp_ripbuf_vec, collapse  = "  "), " :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_cap_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : ", paste(bmp_ripbuf_vec, collapse  = "  ")," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_op_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ripbuf_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
}

### Point Cost Coefficients ------
write( 
  "\nparam point_costs_se : 'capital' 'operations' :=	", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
  append = T
)
write.table(
  point_costs_dat_se %>% select(comid_form, capital, operations), # selecting forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)

write( 
  "\nparam urban_costs_se : 'capital' 'operations' :=	", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_costs_dat_se[, c("bmp_form", "capital", "operations")],
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'      0      0",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    append = T
  )
}
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)

### Septic BMP Costs Standard Errors ------
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
if(length(Septic_BMPs) > 0) {
  cat(
    paste0("\nparam septic_costs_capital_se : ",paste(bmp_septic_vec, collapse = " ")," :=	"),
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    septic_cap_costs_se %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  
  cat(
    paste0("\nparam septic_costs_operations_se : ", paste(bmp_septic_vec, collapse = " ")," :=	"),
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    septic_op_costs_se %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
} else {
  cat(
    "\nparam septic_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
  
  cat(
    "\nparam septic_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)
}
}

### Ag BMP Cost Standard Errors -------------
if(length(Ag_BMPs) > 0) {
  cat(
    "\nparam ag_costs_capital_se : ",paste(bmp_ag_vec, collapse  = "  ")," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_cap_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : ",paste(bmp_ag_vec, collapse  = "  ")," :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_op_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ag_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)
}


write(  
  "\nparam runoff_coeff_urban_se : 'urban' :=", 
  file =  paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T
)
write.table(
  runoffcoeff_dat_se %>% select(comid_form, urban = runoffcoeff_se),  # renaming also forces the order incase they are disordered in preprocessing code above
  file =  paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file =  paste(OutPath, "STdata_dynamic_uncertainty.dat", sep=""), append = T)

write( 
  "\nparam: urban_cost_adjustment_coef_se :=	", 
  file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_cost_coeffs_dat_se,
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    urban_bmp_dummy,
    file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_dynamic_uncertainty.dat", sep = ""), append = T)

# Write Model Script ####

file.copy(from = paste0(OutPath, 'STmodel_seasonal.mod'), to = paste0(OutPath, 'STmodel_dynamic_uncertainty.mod'),
          overwrite = TRUE)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam baseloads_N", 
          i, 
          "_se {comid_N", 
          i, 
          ", seasons, years, loads_N} >= 0;"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      if (!is.null(param_loads_lim_tp[[i]]) && nrow(param_loads_lim_tp[[i]]) > 0) {
        cat(
          paste0(
            "\nparam baseloads_P", 
            i, 
            "_se {comid_P", 
            i, 
            ", seasons, years, loads_P} >= 0;"
          ),
          file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
          sep = "", 
          append = T
        )
      }
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("\nparam riparianload_N", i, "_se {comid_N", i, ", seasons, years, loads_riparian} >= 0;"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("\nparam riparianload_P", i, "_se {comid_P", i, ", seasons, years, loads_riparian} >= 0;"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  foreach(i = 1:length(sm_N_dat_se)) %do% {
    cat(
      paste0("param transfer_coefficients_N", i, "_se {comid_N", i, ", seasons, years, coeff};"),
      file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

cat("\n", file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""), append = T)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(sm_P_dat_se)) %do% {
      cat(
        paste0("param transfer_coefficients_P", i, "_se {comid_P", i, ", seasons, years, coeff};"),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_N", 
          i, 
          "_se {c in comid_N", 
          i, 
          ", s in seasons, y in years, r in ripbuf_bmp};"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_P", 
          i, 
          "_se {c in comid_P", 
          i, 
          ", s in seasons, y in years, r in ripbuf_bmp};"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
param ag_effic_N_se {comid_all_N ,ag_bmp};
param urban_effic_N_se {comid_all_N, urban_bmp};
param point_effic_N_se {comid_all_N, point_c};
param septic_effic_N_se {comid_all_N, septic_bmp};",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
  
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
param ag_effic_P_se {comid_all_P ,ag_bmp};
param urban_effic_P_se {comid_all_P, urban_bmp};
param point_effic_P_se {comid_all_P, point_c};",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param ripbuf_costs_capital_se {comid_all, ripbuf_bmp};
param ripbuf_costs_operations_se {comid_all, ripbuf_bmp};

param point_costs_se {comid_all,cost_type};
param urban_costs_se {urban_bmp,cost_type};

param septic_costs_capital_se {comid_all_N, septic_bmp} >= 0;
param septic_costs_operations_se {comid_all_N, septic_bmp} >= 0;

param area_se {comid_all,area_sub} >=0;

param ag_costs_capital_se {comid_all,ag_bmp};
param ag_costs_operations_se {comid_all,ag_bmp};

param runoff_coeff_urban_se {comid_all,urban_imp} >=0;

param urban_cost_adjustment_coef_se {comid_all} >= 0;


",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  paste0("set Scenarios := 1 .. ", n.scenarios, " by 1;"),
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  "
set Bootstraps := 1 .. 200 by 1;
",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam baseloads_N", 
          i, 
          "_rev {comid_N", 
          i, 
          ", seasons, years, loads_N, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\nparam baseloads_P", 
          i, 
          "_rev {comid_P", 
          i, 
          ", seasons, years, loads_P, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam riparianload_N", 
          i, 
          "_rev {seasons, years, loads_riparian, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\nparam riparianload_P", 
          i, 
          "_rev {seasons, years, loads_riparian, Bootstraps};"
        ),   
        file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  foreach(i = 1:length(sm_N_dat)) %do% {
    cat(
      paste0("param transfer_coefficients_N", i, "_rev {comid_N", i, ", seasons, years, coeff, Bootstraps};"),
      file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

cat("\n", file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""), append = T)

invisible(
  foreach(i = 1:length(sm_P_dat)) %do% {
    cat(
      paste0("param transfer_coefficients_P", i, "_rev  {comid_P", i, ", seasons, years, coeff, Bootstraps};"),
      file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
param ag_effic_N_rev {comid_all_N ,ag_bmp, Bootstraps};
param point_effic_N_rev {comid_all_N, point_c, Bootstraps};
param urban_effic_N_rev {comid_all_N, urban_bmp, Bootstraps};
param septic_effic_N_rev {comid_all_N, septic_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
  cat(
    "
param ag_effic_P_rev {comid_all_P, ag_bmp, Bootstraps};
param point_effic_P_rev {comid_all_P, point_c, Bootstraps};
param urban_effic_P_rev {comid_all_P, urban_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param area_rev {comid_all, area_sub, Bootstraps};
param ag_costs_capital_rev {comid_all, ag_bmp, Bootstraps};
param ag_costs_operations_rev {comid_all, ag_bmp, Bootstraps};
param septic_costs_capital_rev {comid_all_N, septic_bmp, Bootstraps};
param septic_costs_operations_rev {comid_all_N, septic_bmp, Bootstraps};
param point_costs_rev {comid_all, cost_type, Bootstraps};
param urban_costs_rev {urban_bmp, cost_type, Bootstraps};
param ripbuf_costs_capital_rev {comid_all, ripbuf_bmp, Bootstraps};
param ripbuf_costs_operations_rev {comid_all, ripbuf_bmp, Bootstraps};
param runoff_coeff_urban_rev {comid_all, urban_imp, Bootstraps};
param urban_cost_adjustment_coef_rev {comid_all, Bootstraps};
",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_N", 
          i, 
          "_rev {c in comid_N", 
          i, 
          ", s in seasons, y in years, r in ripbuf_bmp, Bootstraps};"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_P", 
          i, 
          "_rev {c in comid_P", 
          i, 
          ", s in seasons, y in years, r in ripbuf_bmp, Bootstraps};"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

cat(
  "
param agcost_frac_rev {Bootstraps} >=0;

param ps_coef_rev {c in comid_all, b in Bootstraps} := point_costs_rev[c,'capital',b] + point_costs_rev[c,'operations',b];
param septic_coef_rev {c in comid_all, e in septic_bmp, b in Bootstraps} := septic_costs_capital_rev[c,e,b] + septic_costs_operations_rev[c,e,b];
param urban_coef_rev {c in comid_all, u in urban_bmp, b in Bootstraps} := acfttoft3 * pcp * urban_design_depth[c,u] *  runoff_coeff_urban_rev[c,'urban', b] * area_rev[c,'urban',b] * (urban_costs_rev[u,'capital',b] + urban_costs_rev[u,'operations',b]) * urban_cost_adjustment_coef_rev[c,b];
param ag_coef_rev {c in comid_all, a in ag_bmp, b in Bootstraps} := agcost_frac_rev[b] * area_rev[c,'ag',b] *  (ag_costs_capital_rev[c,a,b] + ag_costs_operations_rev[c,a,b]) ; 
param ripbuf_coef_rev {c in comid_all, r in ripbuf_bmp, b in Bootstraps} := agcost_frac_rev[b] * (ripbuf_costs_capital_rev[c, r,b] +  ripbuf_costs_operations_rev[c, r,b]);


",
  file = paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0(
          "\nvar inc_storage_load_N", 
          i, 
          "_rev {c in comid_N", 
          i, 
          ", s in seasons, y in years, b in Bootstraps} >= 0;"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0(
          "\nvar inc_storage_load_P", 
          i, 
          "_rev {c in comid_P", 
          i, 
          ", s in seasons, y in years, b in Bootstraps} >= 0;"
        ), 
        file =  paste(OutPath, "STmodel_dynamic_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

print(
  paste(
    "RBEROST has finished writing AMPL scripts with uncertainty at",
    Sys.time()
  )
)
