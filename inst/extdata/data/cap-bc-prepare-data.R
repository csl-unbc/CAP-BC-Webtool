# set the current working directory to "/home/xavier/Projects/UNBC/CAP-BC-Webtool"
setwd("/home/xavier/Projects/UNBC/CAP-BC-Webtool")

### Load packages
library(raster)
library(here)
library(stringr)
library(glue)
library(dplyr)
library(tidyr)
library(readr)

rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")  # not xml file

### data type
# LOG1S FALSE (0) TRUE (1)
# INT1S -127 127
# INT1U 0 255
# INT2S -32,767 32,767
# INT2U 0 65,534
# INT4S -2,147,483,647 2,147,483,647
# INT4U 0 4,294,967,296
# FLT4S -3.4e+38 3.4e+38
# FLT8S -1.7e+308 1.7e+308

cap_bc_input <- "/home/xavier/Dropbox/CAB-BC/input"
# cap_bc_output <- "/home/xavier/Projects/UNBC/CAP-BC-Webtool/inst/extdata/data/cap-bc-data/"
# cap_bc_output <- "/home/xavier/Data/test_capbc/"

PU_1km <- raster(here::here("inst/extdata/data/cap-bc-data-1km/Planning Units.tif"))  # %>% raster::projectRaster(crs = as(sf::st_crs(3857), "CRS"), method = "ngb")
PU_5km <- raster(here::here("inst/extdata/data/cap-bc-data-5km/Planning Units.tif"))  # %>% raster::projectRaster(crs = as(sf::st_crs(3857), "CRS"), method = "ngb")

# convert zero to NA
PU_1km[PU_1km==0] <- NA
PU_5km[PU_5km==0] <- NA

prepare_raster_file <- function(raster_file, raster_layer = NA, out_path, norm = TRUE, dtype, fill_nodata = NA, res) {

  if (class(raster_layer)[1]!="RasterLayer") {
    rl <- raster_file %>% raster()
  }

  if (res == "1km") {
    PU <- PU_1km
    cap_bc_dir <- "cap-bc-data-1km"
  } else {
    PU <- PU_5km
    cap_bc_dir <- "cap-bc-data-5km"
  }

  file_name <- basename(raster_file)
  # replace suffix from 1km and 5km Vector2PU convertion
  file_name <- sub(' - 1km.tif', '.tif', file_name)
  file_name <- sub(' - 5km.tif', '.tif', file_name)


  #layer_name <- sub('\\..[^\\.]*$', '', file_name)

  if (!is.na(fill_nodata)) {
    rl[is.na(rl[])] <- fill_nodata
  }
  rl <- rl %>% raster::mask(PU)

  # normalize layer to 0-1 km2 or 0-25 km2
  if (norm) {
    if (res == "1km") {
      rl <- (rl-cellStats(rl, min))/(cellStats(rl, max)-cellStats(rl, min))
    } else {
      rl <- (rl-cellStats(rl, min))/(cellStats(rl, max)-cellStats(rl, min)) * 25
    }
  }

  full_out_path <- file.path(here::here("inst/extdata/data/"), cap_bc_dir, out_path)
  if (!dir.exists(full_out_path)) {dir.create(full_out_path, recursive = TRUE)}
  writeRaster(rl, filename=file.path(full_out_path, file_name), format="GTiff", overwrite=TRUE, datatype=dtype)
}

prepare_raster_file_1km_and_5km <- function(raster_file, raster_layer = NA, out_path, norm = TRUE, dtype = c("FLT4S", "FLT4S"), fill_nodata = NA) {

  if (class(raster_layer)[1]!="RasterLayer") {
    raster_layer <- raster_file %>% raster()
  }

  # 1km resolution
  dt <- dtype[1]
  rl <- raster_layer
  prepare_raw_raster_file(rl, raster_layer, out_path, norm, dt, fill_nodata, "1km")

  # 5km resolution
  dt <- dtype[2]
  method <- if (dt == "FLT4S") "bilinear" else "ngb"
  rl <- raster::projectRaster(raster_layer, to = PU, method = method)
  prepare_raw_raster_file(rl, raster_layer, out_path, norm, dt, fill_nodata, "5km")
}


###################### CAP-BC Data ######################

########### YALE 1 - Current patterns of biodiversity

yale_path <- "yale_1"

# BEC zones current - 1km
BEC_current_1km <- file.path(cap_bc_input, "Layers - Current/Ecosystems/BEC Zones/By Zones/") %>%
  list.files(pattern = "*1km.tif$", full.names = TRUE)
for (file in BEC_current_1km) {
  prepare_raster_file(file, NA, out_path = file.path(yale_path, "BEC_zones"), norm = TRUE,
                      dtype = "FLT4S", fill_nodata = 0, res = "1km")
}
# BEC zones current - 5km
BEC_current_5km <- file.path(cap_bc_input, "Layers - Current/Ecosystems/BEC Zones/By Zones/") %>%
  list.files(pattern = "*5km.tif$", full.names = TRUE)
for (file in BEC_current_5km) {
  prepare_raster_file(file, NA, out_path = file.path(yale_path, "BEC_zones"), norm = TRUE,
                      dtype = "FLT4S", fill_nodata = 0, res = "5km")
}

# under_represented_BEC_zones - 1km
under_represented_BEC_zones_1km <- file.path(cap_bc_input, "Layers - Current/Ecosystems/BEC Zones/Under-represented BEC zones - 1km.tif")
prepare_raster_file(under_represented_BEC_zones_1km, NA, out_path = yale_path, norm = TRUE,
                    dtype = "FLT4S", fill_nodata = NA, res = "1km")
# under_represented_BEC_zones - 5km
under_represented_BEC_zones_5km <- file.path(cap_bc_input, "Layers - Current/Ecosystems/BEC Zones/Under-represented BEC zones - 5km.tif")
prepare_raster_file(under_represented_BEC_zones_5km, NA, out_path = yale_path, norm = TRUE,
                    dtype = "FLT4S", fill_nodata = NA, res = "5km")

# Critical habitat
critical_habitat <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Critical Habitat - 1km.tif")
prepare_raster_file(critical_habitat, NA, out_path = yale_path, norm = TRUE, dtype = "FLT4S", fill_nodata = NA, "1km")
critical_habitat <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Critical Habitat - 5km.tif")
prepare_raster_file(critical_habitat, NA, out_path = yale_path, norm = TRUE, dtype = "FLT4S", fill_nodata = NA, "5km")

# Bison Capability
bison_capability <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Bison Capability.tif")
prepare_raster_file(bison_capability, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = NA)

# Moose Capability - Alaskan
moose_capability_alaskan <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Moose Capability - Alaskan.tif")
prepare_raster_file(moose_capability_alaskan, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = NA)

# Moose Capability - Northwestern
moose_capability_northwestern <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Moose Capability - Northwestern.tif")
prepare_raster_file(moose_capability_northwestern, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = NA)

# Moose Capability - Shiras
moose_capability_shiras <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Moose Capability - Shiras.tif")
prepare_raster_file(moose_capability_shiras, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = NA)

# Caribou Capability - Boreal
caribou_capability_boreal <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Caribou Capability - Boreal.tif")
prepare_raster_file(caribou_capability_boreal, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = NA)

# Caribou Capability - Northern
caribou_capability_northern <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Caribou Capability - Northern.tif")
prepare_raster_file(caribou_capability_northern, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = NA)

# Elk Capability
elk_capability <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Elk Capability.tif")
prepare_raster_file(elk_capability, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = NA)

# Bird Species Richness
bird_species_richness <- file.path(cap_bc_input, "Layers - Current/Species/Birds/Bird Species Richness.tif")
prepare_raster_file(bird_species_richness, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Tree Species Richness
tree_species_richness <- file.path(cap_bc_input, "Layers - Current/Species/Trees/Tree Species Richness.tif")
prepare_raster_file(tree_species_richness, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Birds Species Distribution Current
birds_path <- "/run/media/xavier/Xavier-ext4/CAP-BC/CAP-BC-Data/original-input/Layers - Projection/Species/Birds"
birds_current_list <- file.path(birds_path, "Current") %>% list.files(pattern = "*.tif$", full.names = TRUE)
birds_names <- file.path(birds_path, "Birds.csv") %>% read_csv()
birds_table <- data.frame("Type"=numeric(0), "Theme"=numeric(0), "File"=numeric(0),
                          "Name"=numeric(0), "Color"=numeric(0), "Legend"=numeric(0),
                          "Labels"=numeric(0), "Unit"=numeric(0), "Visible"=numeric(0),
                          "Status"=numeric(0), "Provenance"=numeric(0), "Description"=numeric(0),
                          "Notes"=numeric(0))
for (file in birds_current_list) {
  # prepare the raster file
  prepare_raster_file(file, NA, out_path = file.path(yale_path, "Birds"), norm = TRUE,
                      dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)
  # add to the table
  bird_code <- unlist(strsplit(basename(file), " "))[[4]]
  bird_name <- birds_names[birds_names$CODE == bird_code,]$NAME
  birds_table[nrow(birds_table)+1, ] <- c("theme", "Current patterns of biodiversity", glue("yale_1/Birds/{basename(file)}"),
                                           glue("Bird: {bird_name}"), "Blues", "continuous", "", "km2*", "FALSE", "TRUE", "missing", "", "")
}
write.csv(birds_table, file.path(here::here("inst/extdata/data/cap-bc-metadata/"), "cap-bc-yale1-birds-metadata.csv"), row.names=FALSE)

# Trees Species Distribution Current
trees_path <- "/run/media/xavier/Xavier-ext4/CAP-BC/CAP-BC-Data/original-input/Layers - Projection/Species/Trees"
trees_current_list <- file.path(trees_path, "Current") %>% list.files(pattern = "*.tif$", full.names = TRUE)
trees_names <- file.path(trees_path, "Trees.csv") %>% read_csv()
trees_table <- data.frame("Type"=numeric(0), "Theme"=numeric(0), "File"=numeric(0),
                          "Name"=numeric(0), "Color"=numeric(0), "Legend"=numeric(0),
                          "Labels"=numeric(0), "Unit"=numeric(0), "Visible"=numeric(0),
                          "Status"=numeric(0), "Provenance"=numeric(0), "Description"=numeric(0),
                          "Notes"=numeric(0))
for (file in trees_current_list) {
  # prepare the raster file
  raster_layer <- file %>% raster()
  raster_layer[raster_layer == 200] <- 0
  prepare_raster_file(file, raster_layer, out_path = file.path(yale_path, "Trees"), norm = TRUE,
                      dtype = c("FLT4S", "INT1U"), fill_nodata = 0)
  # add to the table
  tree_code <- unlist(strsplit(basename(file), " "))[[4]]
  tree_name <- trees_names[trees_names$Code == tree_code,]$Common.Name
  trees_table[nrow(trees_table)+1, ] <- c("theme", "Current patterns of biodiversity", glue("yale_1/Trees/{basename(file)}"),
                                           glue("Tree: {tree_name}"), "Greens", "continuous", "", "km2*", "FALSE", "TRUE", "missing", "", "")
}
write.csv(trees_table, file.path(here::here("inst/extdata/data/cap-bc-metadata/"), "cap-bc-yale1-trees-metadata.csv"), row.names=FALSE)

########### YALE 2 - Natural landscapes and ecological processes

yale_path <- "yale_2"

# Forest Age
forest_age <- file.path(cap_bc_input, "Layers - Current/Species/Trees/Forest Age.tif")
prepare_raster_file(forest_age, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Intact Forest Landscapes
intact_forest_landscapes <- file.path(cap_bc_input, "Layers - Current/Biodiversity/Intact Forest Landscapes.tif")
ifl<-raster(intact_forest_landscapes)
ifl[ifl<=50]<-0
ifl[ifl>50]<-1
prepare_raster_file(intact_forest_landscapes, ifl, out_path = yale_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = NA)

# Wilderness
wilderness <- file.path(cap_bc_input, "Layers - Current/Human/Impact/Wilderness.tif")
prepare_raster_file(wilderness, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = 0)

########### YALE 3 - Geophysical setting

yale_path <- "yale_3"

# Land Facet Diversity
land_facets_diversity <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Land Facet Diversity.tif")
prepare_raster_file(land_facets_diversity, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Karst Potential Areas
karst_potential_areas <- file.path(cap_bc_input, "Layers - Current/Geophysical/Geological Feature/Karst Potential Areas.tif")
prepare_raster_file(karst_potential_areas, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = NA)

########### YALE 4 - Future climate space

yale_path <- "yale_4"

# BEC zone projections
BEC_future_list <- file.path(cap_bc_input, "Layers - Projection/Ecosystems/BEC Zones Projection/By Zones/") %>%
  list.files(pattern = "*2071-2100.tif$", full.names = TRUE)
for (file in BEC_future_list) {
  prepare_raster_file(file, NA, out_path = file.path(yale_path, "BEC projections"), norm = TRUE,
                      dtype = c("INT1U", "INT1U"), fill_nodata = 0)
}

# Pinch Point BEC Zone {16 zones}
BEC_pinch_points <- file.path(cap_bc_input, "Layers - Projection/Ecosystems/Pinch Point BEC Zones/") %>%
  list.files(pattern = "*.tif$", full.names = TRUE)
for (file in BEC_pinch_points) {
  prepare_raster_file(file, NA, out_path = file.path(yale_path, "Pinch Point BEC Zones"), norm = TRUE,
                      dtype = c("INT1U", "INT1U"), fill_nodata = 0)
}

# Bird Species Richness
bird_species_richness <- file.path(cap_bc_input, "Layers - Projection/Species/Birds/Bird Species Richness 2071-2100 rcp45.tif")
prepare_raster_file(bird_species_richness, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Tree Species Richness
tree_species_richness <- file.path(cap_bc_input, "Layers - Projection/Species/Trees/Tree Species Richness 2071-2100 rcp45.tif")
prepare_raster_file(tree_species_richness, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Birds Species Distribution Future
birds_path <- "/run/media/xavier/Xavier-ext4/CAP-BC/CAP-BC-Data/original-input/Layers - Projection/Species/Birds"
birds_current_list <- file.path(birds_path, "rcp45") %>% list.files(pattern = "*2100.tif$", full.names = TRUE)
birds_names <- file.path(birds_path, "Birds.csv") %>% read_csv()
birds_table <- data.frame("Type"=numeric(0), "Theme"=numeric(0), "File"=numeric(0),
                          "Name"=numeric(0), "Color"=numeric(0), "Legend"=numeric(0),
                          "Labels"=numeric(0), "Unit"=numeric(0), "Visible"=numeric(0),
                          "Status"=numeric(0), "Provenance"=numeric(0), "Description"=numeric(0),
                          "Notes"=numeric(0))
for (file in birds_current_list) {
  # prepare the raster file
  prepare_raster_file(file, NA, out_path = file.path(yale_path, "Birds"), norm = TRUE,
                      dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)
  # add to the table
  bird_code <- unlist(strsplit(basename(file), " "))[[4]]
  bird_name <- birds_names[birds_names$CODE == bird_code,]$NAME
  birds_table[nrow(birds_table)+1, ] <- c("theme", "Future climate space", glue("yale_4/Birds/{basename(file)}"),
                                           glue("Bird future: {bird_name}"), "Blues", "continuous", "", "km2*", "FALSE", "TRUE", "missing", "", "")
}
write.csv(birds_table, file.path(here::here("inst/extdata/data/cap-bc-metadata/"), "cap-bc-yale4-birds-metadata.csv"), row.names=FALSE)

# Trees Species Distribution Future
trees_path <- "/run/media/xavier/Xavier-ext4/CAP-BC/CAP-BC-Data/original-input/Layers - Projection/Species/Trees"
trees_current_list <- file.path(trees_path, "rcp45") %>% list.files(pattern = "*100.tif$", full.names = TRUE)
trees_names <- file.path(trees_path, "Trees.csv") %>% read_csv()
trees_table <- data.frame("Type"=numeric(0), "Theme"=numeric(0), "File"=numeric(0),
                          "Name"=numeric(0), "Color"=numeric(0), "Legend"=numeric(0),
                          "Labels"=numeric(0), "Unit"=numeric(0), "Visible"=numeric(0),
                          "Status"=numeric(0), "Provenance"=numeric(0), "Description"=numeric(0),
                          "Notes"=numeric(0))
for (file in trees_current_list) {
  # prepare the raster file
  raster_layer <- file %>% raster()
  raster_layer[raster_layer == 200] <- 0
  prepare_raster_file(file, raster_layer, out_path = file.path(yale_path, "Trees"), norm = TRUE,
                      dtype = c("FLT4S", "INT1U"), fill_nodata = 0)
  # add to the table
  tree_code <- unlist(strsplit(basename(file), " "))[[4]]
  tree_name <- trees_names[trees_names$Code == tree_code,]$Common.Name
  trees_table[nrow(trees_table)+1, ] <- c("theme", "Future climate space", glue("yale_4/Trees/{basename(file)}"),
                                           glue("Tree future: {tree_name}"), "Greens", "continuous", "", "km2*", "FALSE", "TRUE", "missing", "", "")
}
write.csv(trees_table, file.path(here::here("inst/extdata/data/cap-bc-metadata/"), "cap-bc-yale4-trees-metadata.csv"), row.names=FALSE)

########### YALE 5 - Climate refugia

yale_path <- "yale_5"

# BEC Zones Overlap
BEC_zones_overlap <- file.path(cap_bc_input, "Layers - Projection/Refugia/BEC Zones Overlap/BEC Zones Overlap.tif")
prepare_raster_file(BEC_zones_overlap, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = 0)

# Songbird Macrorefugia rcp4.5 2071-2100
Birds_refugia <- file.path(cap_bc_input, "Layers - Projection/Refugia/Tree and Songbird Macrorefugia/",
                           "Songbird Macrorefugia rcp4.5 2071-2100.tif")
prepare_raster_file(Birds_refugia, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Tree Macrorefugia rcp4.5 2071-2100
Trees_refugia <- file.path(cap_bc_input, "Layers - Projection/Refugia/Tree and Songbird Macrorefugia/",
                           "Tree Macrorefugia rcp4.5 2071-2100.tif")
prepare_raster_file(Trees_refugia, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Potencial Climatic Refugia
Climatic_refugia <- file.path(cap_bc_input, "Layers - Projection/Refugia/Future climate refugia/",
                              "Potencial Climatic Refugia rcp8.5 2071-2100.tif")
prepare_raster_file(Climatic_refugia, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "INT1U"), fill_nodata = 0)

# Ecoregional Refugia Index rcp4.5 2071-2100
Ecoregional_Refugia_Index <- file.path(cap_bc_input, "Layers - Projection/Refugia/Ecoregional Refugia Index/",
                                       "Ecoregional Refugia Index rcp4.5 2071-2100.tif")
prepare_raster_file(Ecoregional_Refugia_Index, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

########### YALE 6 - Ecological connectivity

yale_path <- "yale_6"

# Species movement to analog climates
Species_movement_to_analog_climates <- file.path(cap_bc_input, "Layers - Projection/Connectivity/Climate connectivity priority areas/",
                                        "Species movement to analog climates rcp8.5 2071-2100.tif")
prepare_raster_file(Species_movement_to_analog_climates, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Current-flow centrality climate analogs rcp8.5 2071-2100
Current_flow_centrality <- file.path(cap_bc_input, "Layers - Projection/Connectivity/Centrality climate analogs/",
                                      "Current-flow centrality climate analogs rcp8.5 2071-2100.tif")
prepare_raster_file(Current_flow_centrality, NA, out_path = yale_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

########### Ecosystem services

out_path <- "ecosystem_services"

# Ecosystem Carbon
ecosystem_carbon <- file.path(cap_bc_input, "Layers - Current/Carbon Storage/Ecosystem Carbon.tif")
prepare_raster_file(ecosystem_carbon, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Net Primary Productivity
net_primary_productivity <- file.path(cap_bc_input, "Layers - Current/Carbon Storage/Biomass Ecosystem/Net Primary Productivity.tif")
prepare_raster_file(net_primary_productivity, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Recreation Provision
recreation_provision <- file.path(cap_bc_input, "Layers - Current/Ecosystem Services/Recreation Provision.tif")
prepare_raster_file(recreation_provision, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Freshwater Provision
freshwater_provision <- file.path(cap_bc_input, "Layers - Current/Ecosystem Services/Freshwater Provision.tif")
prepare_raster_file(freshwater_provision, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)


########### COSTS

out_path <- "costs"

# Human Footprint
human_footprint <- file.path(cap_bc_input, "Layers - Current/Human/Impact/Human Footprint.tif")
prepare_raster_file(human_footprint, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Road Density (log)
road_density <- file.path(cap_bc_input, "Layers - Current/Human/Impact/Road Density (log).tif")
prepare_raster_file(road_density, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Forward multivariate climatic velocity
forward_multivariate_climatic_velocity <- file.path(cap_bc_input, "Layers - Projection/Climate Variables/Multivariate climatic velocity/Forward multivariate climatic velocity rcp4.5 2071-2100.tif")
prepare_raster_file(forward_multivariate_climatic_velocity, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Backward multivariate climatic velocity
backward_multivariate_climatic_velocity <- file.path(cap_bc_input, "Layers - Projection/Climate Variables/Multivariate climatic velocity/Backward multivariate climatic velocity rcp4.5 2071-2100.tif")
prepare_raster_file(backward_multivariate_climatic_velocity, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Absolute climatic dissimilarity
absolute_climatic_dissimilarity <- file.path(cap_bc_input, "Layers - Projection/Climate Variables/Absolute climatic dissimilarity/Absolute climatic dissimilarity Ensemble rcp4.5 2071-2100.tif")
prepare_raster_file(absolute_climatic_dissimilarity, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

# Relative climatic dissimilarity
relative_climatic_dissimilarity <- file.path(cap_bc_input, "Layers - Projection/Climate Variables/Relative climatic dissimilarity/Relative climatic dissimilarity rcp4.5 2071-2100.tif")
prepare_raster_file(relative_climatic_dissimilarity, NA, out_path = out_path, norm = TRUE,
                    dtype = c("FLT4S", "FLT4S"), fill_nodata = 0)

########### INCLUDES

out_path <- "includes"

# Official BC Parks and Protected Areas
current_protected_areas <- file.path(cap_bc_input, "Layers - Current/Protected Areas/BC Parks, Ecological Reserves, and Protected Areas/Official BC Parks and Protected Areas.tif")
prepare_raster_file(current_protected_areas, NA, out_path = out_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = NA)

# Canadian Protected and Conserved Areas Database
current_protected_areas <- file.path(cap_bc_input, "Layers - Current/Protected Areas/Canadian Protected and Conserved Areas Database.tif")
prepare_raster_file(current_protected_areas, NA, out_path = out_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = NA)

# NGO Conservation Areas
ngo_conservation_areas <- file.path(cap_bc_input, "Layers - Current/Protected Areas/NGO Conservation Areas.tif")
prepare_raster_file(ngo_conservation_areas, NA, out_path = out_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = NA)

###########

# Private Lands
private_lands <- file.path(cap_bc_input, "Layers - Current/Human/Impact/Private Lands.tif")
prepare_raster_file(private_lands, NA, out_path = out_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = 0)

# Public Lands
public_lands <- file.path(cap_bc_input, "Layers - Current/Human/Impact/Public Lands.tif")
prepare_raster_file(public_lands, NA, out_path = out_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = 0)

# Natural Resource Extraction Tenures
natural_resource_extraction_tenures <- file.path(cap_bc_input, "Layers - Current/Human/Impact/Natural Resource Extraction Tenures.tif")
prepare_raster_file(natural_resource_extraction_tenures, NA, out_path = out_path, norm = TRUE,
                    dtype = c("INT1U", "INT1U"), fill_nodata = 0)

