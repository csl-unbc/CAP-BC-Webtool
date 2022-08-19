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

PU_1km <- raster(here::here("inst/extdata/data/cap-bc-data-1km/Planning Units.tif")) %>% na_if(0)  # %>% raster::projectRaster(crs = as(sf::st_crs(3857), "CRS"), method = "ngb")
PU_5km <- raster(here::here("inst/extdata/data/cap-bc-data-5km/Planning Units.tif")) %>% na_if(0)  # %>% raster::projectRaster(crs = as(sf::st_crs(3857), "CRS"), method = "ngb")

prepare_raster_file <- function(raster_file, raster_layer = NA, out_path, norm = TRUE, dtype = "FLT4S", fill_nodata = NA) {

  if (class(raster_layer)[1]!="RasterLayer") {
    raster_layer <- raster_file %>% raster()
  }

  idx_list <- list(list(PU_1km, PU_5km), c("cap-bc-data-1km", "cap-bc-data-5km"))
  idx_vect <- c(1:length(idx_list[[1]]))

  for(i in idx_vect){
    PU <- idx_list[[1]][[i]]
    cap_bc_dir <- idx_list[[2]][[i]]

    if (cap_bc_dir == "cap-bc-data-5km") {
      rl <- raster::projectRaster(raster_layer, to = PU, method = "ngb")
    } else {
      rl <- raster_layer
    }

    file_name <- basename(raster_file)
    #layer_name <- sub('\\..[^\\.]*$', '', file_name)

    if (!is.na(fill_nodata)) {
      rl[is.na(rl[])] <- fill_nodata
    }
    rl <- rl %>% raster::mask(PU)

    # normalize layer to 0-1 km2 or 0-25 km2
    if (norm) {
      if (cap_bc_dir == "cap-bc-data-1km") {
        rl <- (rl-cellStats(rl, min))/(cellStats(rl, max)-cellStats(rl, min))
      } else {
        rl <- (rl-cellStats(rl, min))/(cellStats(rl, max)-cellStats(rl, min)) * 25
      }
    }

    full_out_path <- file.path(here::here("inst/extdata/data/"), cap_bc_dir, out_path)
    if (!dir.exists(full_out_path)) {dir.create(full_out_path, recursive = TRUE)}
    writeRaster(rl, filename=file.path(full_out_path, file_name), format="GTiff", overwrite=TRUE, datatype=dtype)
  }
}

###################### CAP-BC Data ######################

########### YALE 1 - Current patterns of biodiversity

yale_path <- "yale_1"

# BEC now
BEC_now <- file.path(cap_bc_input, "Layers - Current/Ecosystems/BEC Zones/By Zones/") %>%
  list.files(pattern = "*.tif$", full.names = TRUE)
for (file in BEC_now) {
  prepare_raster_file(file, NA, out_path = file.path(yale_path, "BEC_zones"), norm = TRUE, dtype = 'INT1U', fill_nodata = 0)
}
# unrepresentative_BEC_zones
unrepresentative_BEC_zones <- file.path(cap_bc_input, "Layers - Current/Ecosystems/BEC Zones/Unrepresentative BEC Zones.tif")
prepare_raster_file(unrepresentative_BEC_zones, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Critical habitat
critical_habitat <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Critical Habitat.tif")
prepare_raster_file(critical_habitat, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Bison Capability
bison_capability <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Bison Capability.tif")
prepare_raster_file(bison_capability, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Moose Capability - Alaskan
moose_capability_alaskan <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Moose Capability - Alaskan.tif")
prepare_raster_file(moose_capability_alaskan, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Moose Capability - Northwestern
moose_capability_northwestern <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Moose Capability - Northwestern.tif")
prepare_raster_file(moose_capability_northwestern, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Moose Capability - Shiras
moose_capability_shiras <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Moose Capability - Shiras.tif")
prepare_raster_file(moose_capability_shiras, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Caribou Capability - Boreal
caribou_capability_boreal <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Caribou Capability - Boreal.tif")
prepare_raster_file(caribou_capability_boreal, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Caribou Capability - Northern
caribou_capability_northern <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Caribou Capability - Northern.tif")
prepare_raster_file(caribou_capability_northern, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Elk Capability
elk_capability <- file.path(cap_bc_input, "Layers - Current/Ecosystems/Habitats/Elk Capability.tif")
prepare_raster_file(elk_capability, NA, out_path = yale_path, norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

# Birds Species Distribution Current
birds_path <- "/run/media/xavier/Xavier-ext4/CAP-BC/CAP-BC-Data/original-input/Layers - Projection/Species/Birds"

birds_current_list <- file.path(birds_path, "Current") %>% list.files(pattern = "*.tif$", full.names = TRUE)

birds_names <- file.path(birds_path, "Birds.csv") %>% read_csv()
birds_table <- data.frame("Type"=numeric(0), "Theme"=numeric(0), "File"=numeric(0),
                          "Name"=numeric(0), "Color"=numeric(0), "Legend"=numeric(0),
                          "Labels"=numeric(0), "Unit"=numeric(0), "Visible"=numeric(0),
                          "Provenance"=numeric(0))
for (file in birds_current_list) {
  # prepare the raster file
  # prepare_raster_file(file, NA, out_path = file.path(yale_path, "Birds"), norm = TRUE, dtype = 'FLT4S', fill_nodata = 0)
  # add to the table
  bird_code <- unlist(strsplit(basename(file), " "))[[4]]
  bird_name <- birds_names[birds_names$CODE == bird_code,]$NAME
  birds_table[nrow(birds_table)+1, ] <- c("theme", "Current patterns of biodiversity", glue("yale_1/Birds/{basename(file)}"),
                                           glue("Bird: {bird_name}"), "Blues", "continuous", "", "km2*", "FALSE", "missing")
}
write.csv(birds_table, file.path(here::here("inst/extdata/data/"), "cap-bc-yale1-birds-metadata.csv"), row.names=FALSE)

# Trees Species Distribution Current
trees_path <- "/run/media/xavier/Xavier-ext4/CAP-BC/CAP-BC-Data/original-input/Layers - Projection/Species/Trees"

trees_current_list <- file.path(trees_path, "Current") %>% list.files(pattern = "*.tif$", full.names = TRUE)

trees_names <- file.path(trees_path, "Trees.csv") %>% read_csv()
trees_table <- data.frame("Type"=numeric(0), "Theme"=numeric(0), "File"=numeric(0),
                          "Name"=numeric(0), "Color"=numeric(0), "Legend"=numeric(0),
                          "Labels"=numeric(0), "Unit"=numeric(0), "Visible"=numeric(0),
                          "Provenance"=numeric(0))
for (file in trees_current_list) {
  # prepare the raster file
  raster_layer <- file %>% raster()
  raster_layer[raster_layer == 200] <- 0
  prepare_raster_file(file, raster_layer, out_path = file.path(yale_path, "Trees"), norm = TRUE, dtype = 'FLT4S', fill_nodata = 0)
  # add to the table
  tree_code <- unlist(strsplit(basename(file), " "))[[4]]
  tree_name <- trees_names[trees_names$Code == tree_code,]$Common.Name
  trees_table[nrow(trees_table)+1, ] <- c("theme", "Current patterns of biodiversity", glue("yale_1/Trees/{basename(file)}"),
                                           glue("Tree: {tree_name}"), "Greens", "continuous", "", "km2*", "FALSE", "missing")
}
write.csv(trees_table, file.path(here::here("inst/extdata/data/"), "cap-bc-yale1-trees-metadata.csv"), row.names=FALSE)






########### YALE 2 - Natural landscapes and ecological processes
########### YALE 3 - Geophysical setting
########### YALE 4 - Future climate space
########### YALE 5 - Climate refugia
########### YALE 6 - Ecological connectivity

########### WEIGHT
prepare_raster_file(file.path(cap_bc_input, "Layers - Current/Human/Impact/Human Footprint.tif"),
                    NA, out_path = "weight", norm = TRUE, dtype = 'FLT4S', fill_nodata = NA)

########### INCLUDE

prepare_raster_file(file.path(cap_bc_input, "Layers - Current/Protected Areas/BC Parks, Ecological Reserves, and Protected Areas/BC Parks, Ecological Reserves, and Protected Areas.tif"),
                    NA, out_path = "include", norm = TRUE, dtype = 'INT1U', fill_nodata = NA)
