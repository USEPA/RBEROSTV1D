### Estuary Data Mapper Preparation Code
### Date Created: 5/10/2024 (Hunter Parker)
### Purpose: This code helps prepare the files for the Estuary Data Mapper portion of the RBEROST TO

# Set Setting, Options and Packages ----

### Settings ----
rm(list=ls())
options(stringsAsFactors=FALSE)
projection <-  "ESRI: 102003"
user <- "56407"
projection_map <- "EPSG:4326"

### Packages -----
pacman::p_load(openxlsx, tigris, tidyverse, readxl, sf, dplyr, janitor, fuzzyjoin, beepr, nhdplusTools, ggplot2, spData, leaflet,dataRetrieval)

### Long Island Sound File - these were created in the LIS loading targets code since it was easier to write them there as opposed to loading them into a new file and then working on them -----
LIS_upstream_csv <- read_csv("C:/Users/56407/ICF/Optimization Tools - General/RBEROST/Estuary Data Mapper/Inputs/LIS_Upstream_lines.csv")

LIS_upstream_shp <- st_read("C:/Users/56407/ICF/Optimization Tools - General/RBEROST/Estuary Data Mapper/Inputs/LIS_Upstream_lines.shp")

# Northeast Model -----

### Create list of input files -----
LIS_gitpath <- "C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Preprocessing/Inputs/"

LIS_csv_files <- list.files(path = LIS_gitpath,  # Identify all csv files
                            pattern = "*.csv", full.names = TRUE)

##### Read csvs -----
if(exists("LIS_data_all_csv")){
  rm(LIS_data_all_csv)
}

for(i in 1:length(LIS_csv_files)){
  csv_file <- read_csv(LIS_csv_files[[i]])
  og_source <- basename(LIS_csv_files[[i]])
  if(exists("LIS_data_all_csv")){
    LIS_data_all_csv <- append(LIS_data_all_csv,list(csv_file))
    names(LIS_data_all_csv)[[i]] <- og_source
  }else{
    LIS_data_all_csv <- list(csv_file)
    names(LIS_data_all_csv)[[i]] <- og_source
  }
}

##### Combine all files (only had CSVs for Northeast Model) -----
LIS_data_all_files <- LIS_data_all_csv

### Get variable list ------
if (exists("LIS_var_list")) {
  rm(LIS_var_list)
}

for (i in 1:length(LIS_data_all_files)){
  source_file <- names(LIS_data_all_files)[[i]]
  LIS_var <- c(colnames(LIS_data_all_files[[i]]))
  
  if (exists("LIS_var_list")) {
    LIS_var_list <- rbind(LIS_var_list,as.data.frame(LIS_var)%>%mutate(source_file=source_file))
  } else {
    LIS_var_list <- as.data.frame(LIS_var)%>%mutate(source_file=source_file)
  }
}

### Create data dictionary for Northeast ----
LIS_dictionary <- LIS_var_list%>%
  select("RBEROST Source File"="source_file","RBEROST Variable Name" = "LIS_var")%>%
  mutate(
    Model="Northeast"
  )%>%
  select(Model, 'RBEROST Source File', 'RBEROST Variable Name')

View(LIS_dictionary) # 994 variables

### remove intermediates/temp files
rm(list=setdiff(ls(), c("projection", "user", "projection_map", "PS_dictionary", "LIS_dictionary")))

# Create full RBEROST variable list ----

# Merge together Northeast and Pacific dictionaries
full_dictionary <- LIS_dictionary %>%
  mutate(
    'EDM Variable' = "",
    Assigned_To = "",
    Modification = ""
  )%>%
  select('EDM Variable', Model, 'RBEROST Source File', 'RBEROST Variable Name', Modification, Assigned_To)  #1524

full_dictionary$key <- paste0(full_dictionary$Model, full_dictionary$`RBEROST Source File`)

View(full_dictionary) # 1524 variables

#
#
#

# Filter by Known/Used Input Files -----
### Standard Preprocessing Code inputs -----
lis_list <- read_xlsx("C:/Users/56407/ICF/Optimization Tools - General/RBEROST/Estuary Data Mapper/Known 01_Preprocessing Inputs_updated.xlsx", sheet="Northeast")

#28 inputs for the Pacific Model
#22 inputs for the Northeast Model

known_list <- lis_list %>%
  mutate(
    key=paste0(Model, File)
  )

qa <- full_dictionary%>%
  filter(key %in% known_list$key)
length(unique(qa$key)) # all 50 input data sets known to be used in the 2 models have been captured here

### Additional information for Uncertainty Preprocessing codes -----
lis_additional_list <- read_xlsx("C:/Users/56407/ICF/Optimization Tools - General/RBEROST/Estuary Data Mapper/Known 01_Preprocessing Inputs_updated.xlsx", sheet="Northeast_Uncertainty")

#1 additional inputs for Pacific Model

#2 additional inputs for Northeast Model

known_additional_list <- lis_additional_list %>%
  mutate(
    key=paste0(Model, File)
  )

additional_qa <- full_dictionary%>%filter(key %in% known_additional_list$key)

length(unique(additional_qa$key)) #3

# Finalized EDM Data Dictionary ----
### Create full list that contains updated list, and uncertainty additional inputs ----
V4_list <- rbind(known_list, known_additional_list)
length(unique(V4_list$key)) #24

### View final EDM data dictionary ----
final_EDM_dictionary <- full_dictionary%>%
  filter(key %in% V4_list$key)%>%
  select(-key)

View(final_EDM_dictionary) # 24 variables

### Write out EDM data dictionary (commented out to prevent overwrite)
# write.xlsx(full_dictionary%>%filter(key %in% V4_list$key)%>%select(-key), "C:/Users/61199/ICF/Optimization Tools - General/RBEROST/Estuary Data Mapper/Data Dictionaries/EDM Data Crosswalk V5_Final.xlsx")
