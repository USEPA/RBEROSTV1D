########################################################################
# PURPOSE: Download NHDplusV2 data and subset to region of interest ####
# BY: Cathy Chamberlin                                              ####
# ORIGINAL DATE: 11/17/2021                                         ####
########################################################################

# Code tagged with #*# should be editted by the user before running the script

##########################################################
# 1. User Inputs                                     #####
##########################################################

filepath_loadingtargets <- 
  "./RBEROST-Northeast/Preprocessing/Inputs/01_UserSpecs_loadingtargets.csv"#*#
filepath_NHDplusV2CONUS <- 
  "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/NHDplusV2/" #*#
filename_geopackage <- 
  "upperCTwatershedNHDplusV2.gpkg" #*# as NAME.gpkg

##########################################################
# 2. Setup                                           #####
##########################################################

packages <- c(
  "ggplot2", "readr", "dplyr", "nhdplusTools", "archive", "sf", "maps", "sp" #'ggmap', 'nhdplusTools', 'tidyverse', 'sf', 'maps', 'sp', 'data.table'
)
lapply(packages, library, character.only = TRUE)

theme_set(theme_classic())

##########################################################
# 2. Read in Loading Targets                         #####
##########################################################
loading.targets <- read_csv(filepath_loadingtargets)
terminal.comid <- loading.targets %>% filter(TermFlag_X == "X") %>% .$ComID

##########################################################
# 3. Download NHD+v2 CONUS data                      #####
##########################################################
# # Note: This should only be run once as it takes a very long time.
# download_nhdplusv2(outdir = filepath_NHDplusV2CONUS)
# archive_extract(
#   paste0(
#     filepath_NHDplusV2CONUS, 
#     "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
#     ), dir = filepath_NHDplusV2CONUS
#   ) # This line of code may, or may not work. User might need to unzip through a different program.

##########################################################
# 4. Subset NHD+v2 data to geographic region         #####
##########################################################
# Note: Expect this to take up to an hour
nhdpluscomid <- list(
  featureSource = "comid", featureID = as.character(terminal.comid)
)
flowline <- navigate_nldi(
  nhdpluscomid,
  mode = "upstreamTributaries",
  distance_km = 9999
)
# 
# # Not rerun since July
#
nhdplus <- subset_nhdplus(
  comids = flowline$UT_flowlines$nhdplus_comid,
  output_file = paste0("./Data/", filename_geopackage),
  nhdplus_data = paste0(
    filepath_NHDplusV2CONUS,
    "NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
    ),
  overwrite = TRUE,
  return_data = FALSE,
  status = TRUE,
  flowline_only = FALSE
  )

st_layers(nhdplus)

#############################
# 5. Plot to verify data ####
#############################

# Read data
flowline <- read_sf(paste0("./Data/", filename_geopackage), "NHDFlowline_Network")
catchments <- read_sf(paste0("./Data/", filename_geopackage), "CatchmentSP")

basin <- get_nldi_basin(nhdpluscomid)

states <- st_as_sf(
  map(database = "state", plot = TRUE, fill = TRUE, col = "white")
)

# Check projections
basin <- st_transform(basin, st_crs(states))
flowline <- st_transform(flowline, st_crs(states))
catchments <- st_transform(catchments, st_crs(states))

st_crs(states) == st_crs(basin) 
st_crs(states) == st_crs(flowline)
st_crs(states) == st_crs(catchments)
  
# Get bounding box
sp_bbox <- bbox(as_Spatial(basin))

sp_bbox[,1] <- sp_bbox[,1] - 0.1 # Expand the bounding box slightly
sp_bbox[,2] <- sp_bbox[,2] + 0.2 # Expand the bounding box slightly

# Plot flowlines
ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = basin, inherit.aes = FALSE, color = "black", fill = "white", lwd = 2
  ) + 
  geom_sf(
    data = flowline %>% filter(StreamOrde == 1), 
    inherit.aes = FALSE, color = "grey90"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 2), 
    inherit.aes = FALSE, color = "grey80"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 3), 
    inherit.aes = FALSE, color = "grey70"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 4), 
    inherit.aes = FALSE, color = "grey60"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 5), 
    inherit.aes = FALSE, color = "grey50"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 6), 
    inherit.aes = FALSE, color = "grey40"
  ) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

# Plot catchments
ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = basin, inherit.aes = FALSE, color = "black", fill = "white", lwd = 2
  ) + 
  geom_sf(
    data = catchments, 
    mapping = aes(fill = AreaSqKM),
    inherit.aes = FALSE, 
    color = NA,
    lwd = 0.01
  ) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

#########################################
### 6. Get Waterbody and Area Layers ####
#########################################
# The subset_nhdplus function is not picking up the waterbody or area layers because they are not in the list of provided comids, even though they overlap geographically
# These can be gathered separately and written to the geopackage
# The all_basins area is larger than the revised_basin. It will be better to use this for this purpose so nothing gets missed.

# Note, this is not necessary to run at this time for the Upper Connecticut, because the code previously included these layers.

NHDArea <- get_nhdarea(all_basins)
NHDWaterbody <- get_waterbodies(all_basins) 

# Plot waterbodies and area
ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = revised_basin, 
    inherit.aes = FALSE, 
    color = "black", 
    fill = "white", 
    lwd = 2
  ) + 
  geom_sf(
    data = NHDArea, 
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = NHDWaterbody, 
    inherit.aes = FALSE,
    fill = "steelblue",
    color = "steelblue4"
  ) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

# add layers to the .gpkg

st_layers(paste0("./Data/", filename_geopackage))

# from nhdplusTools:
clean_bbox <- function(x) {
  if("bbox" %in% names(x) && class(x$bbox[1]) == "list") {
    x$bbox <- sapply(x$bbox, paste, collapse = ",")
  }

  return(x)
}

write_sf(
  obj = clean_bbox(NHDArea), 
  dsn = paste0("./Data/", filename_geopackage),
  layer = "NHDArea"
  )

write_sf(
  obj = clean_bbox(NHDWaterbody), 
  dsn = paste0("./Data/", filename_geopackage),
  layer = "NHDWaterbody"
  )

st_layers(paste0("./Data/", filename_geopackage))

  