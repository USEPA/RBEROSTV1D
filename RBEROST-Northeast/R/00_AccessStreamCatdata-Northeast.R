###########################################################################################
# PURPOSE: Download StreamCat data for states covered by LIS Model  ####
# BY: Hunter Parker, Sam Ennett ####
# ORIGINAL DATE: 7/20/2023 ####
###########################################################################################

# Lines of code tagged with #*# may need to be edited by the user.
# NOTE: this code only needs to be re-run if the user needs a data vintage other than 2019.

##########################################################
# 1. Setup #####
##########################################################

# install.packages("dplyr") #*# #May need to be run if packages are not installed
# install.packages("stringr") #*# #May need to be run if packages are not installed

library("tidyverse")
library("stringr")
options(stringsAsFactors = FALSE)

##########################################################
# 2. Read CSV files ####
##########################################################

# Working directory
setwd("./") #*# This defaults to the project directory folder if run in an RProject in RStudio

## Updates made 7/20/23 by Hunter Parker

## Download STREAMCAT API data ------------------------------------------------

year <- "2019"

StreamLake_Var <- paste0("pctow", year, ",pctice", year, ",pcturbop", year, ",
pcturblo", year, ",pcturbmd", year, ",pcturbhi", year, ",
pctbl", year, ",pctdecid", year, ",pctconif", year, ",pctmxfst", year, ",
pctshrb", year, ",pctgrs", year, ",pcthay", year, ",pctcrop", year, ",
pctwdwet", year, ",pcthbwet", year, ",pctimp", year, "")
#"*" #You can add/remove variables here as desired, the data year can be changed by amending the year variable 

states <- c("MA","CT","VT","RI","NY","NH")
cat_api <- data.frame()
ws_api <- data.frame()

# Download catchment data
for (i in (1:length(states))) {
  StreamCat_tempcat <- sc_get_data(metric=StreamLake_Var, aoi='catchment', state=states[i])
  cat_api <- rbind(cat_api, StreamCat_tempcat)
}

# Download watershed data
for (i in (1:length(states))) {
  StreamCat_tempws <- sc_get_data(metric=StreamLake_Var, aoi='watershed', state=states[i])
  ws_api <- rbind(ws_api, StreamCat_tempws)
}

### Format API data for consistency with other files ---------------------------

# Merge watershed and catchment level data by COMID
raw_api <- merge(cat_api,ws_api,by =c("COMID","STATE", "WSAREASQKM"))

# Format API data for use with RBEROST code
StreamCat_api <- raw_api%>%
  rowwise() %>%
  mutate(
    CATPCTFULL = sum(c_across(PCTURBOP2019CAT:PCTCONIF2019CAT)),
    WSPCTFULL = sum(c_across(PCTURBOP2019WS:PCTCONIF2019WS)))%>%
  select(comid=COMID,State=STATE,CatAreaSQqKm=CATAREASQKM,WsAreaSqKm=WSAREASQKM,PctImp2019Cat=PCTIMP2019CAT,PctImp2019Ws=PCTIMP2019WS,CatPctFull=CATPCTFULL,WsPctFull=WSPCTFULL,PctOw2019Cat=PCTOW2019CAT,PctIce2019Cat=PCTICE2019CAT,
         PctUrbOp2019Cat=PCTURBOP2019CAT,PctUrbLo2019Cat=PCTURBLO2019CAT,PctUrbMd2019Cat=PCTURBMD2019CAT,PctUrbHi2019Cat=PCTURBHI2019CAT,PctBl2019Cat=PCTBL2019CAT,
         PctDecid2019Cat=PCTDECID2019CAT,PctConif2019Cat=PCTCONIF2019CAT,PctMxFst2019Cat=PCTMXFST2019CAT,PctShrb2019Cat=PCTSHRB2019CAT,PctGrs2019Cat=PCTGRS2019CAT,
         PctHay2019Cat=PCTHAY2019CAT,PctCrop2019Cat=PCTCROP2019CAT,PctWdWet2019Cat=PCTWDWET2019CAT,PctHbWet2019Cat=PCTHBWET2019CAT,
         PctOw2019Ws=PCTOW2019WS,PctIce2019Ws=PCTICE2019WS,PctUrbOp2019Ws=PCTURBOP2019WS,PctUrbLo2019Ws=PCTURBLO2019WS,PctUrbMd2019Ws=PCTURBMD2019WS, 
         PctUrbHi2019Ws=PCTURBHI2019WS,PctBl2019Ws=PCTBL2019WS,PctDecid2019Ws=PCTDECID2019WS,PctConif2019Ws=PCTCONIF2019WS,PctMxFst2019Ws=PCTMXFST2019WS,
         PctShrb2019Ws=PCTSHRB2019WS,PctGrs2019Ws=PCTGRS2019WS,PctHay2019Ws=PCTHAY2019WS,PctCrop2019Ws=PCTCROP2019WS,PctWdWet2019Ws=PCTWDWET2019WS,PctHbWet2019Ws=PCTHBWET2019WS)%>%
  relocate(State,.after=comid)%>%
  relocate(PctImp2019Cat, .after=State)%>%
  relocate(PctImp2019Ws, .after=PctImp2019Cat)


### View and Export Final Data Set ---------------------------------------------

#Remove intermediate step data frames and view final data set format
remove(list= "cat_api","raw_api","ws_api","StreamCat_tempcat","StreamCat_tempws","i","states","StreamLake_Var")

#You can export data set as csv file. This file will be stored to current working directory
write.csv(StreamCat_api, "RBEROST-Northeast/Preprocessing/Inputs/CT_NH_MA_VT_NY_RI_StreamCatData_2019.csv", row.names = F) # "*" #remove "#" at beginnimg of line if you want a csv file of the dataset

