# Initialization
## load packages
devtools::load_all()


#### YALE 1 #####

metadata_path <- file.path("inst", "extdata", "data", "cap-bc-yale1-metadata.csv")
metadata <- tibble::as_tibble(
  utils::read.table(metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE, comment.char = "")
)
## Birds current
metadata_path <- file.path("inst", "extdata", "data", "cap-bc-yale1-birds-metadata.csv")
metadata_to_add <- tibble::as_tibble(
  utils::read.table(metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE, comment.char = "")
)
metadata %>% add_row(metadata_to_add) -> metadata
## Trees current
metadata_path <- file.path("inst", "extdata", "data", "cap-bc-yale1-trees-metadata.csv")
metadata_to_add <- tibble::as_tibble(
  utils::read.table(metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE, comment.char = "")
)
metadata %>% add_row(metadata_to_add) -> metadata

#### WEIGHTS #####

## prepare raster data
metadata_path <- file.path("inst", "extdata", "data", "cap-bc-weights-metadata.csv")
## Import formatted csv (metadata) as tibble
metadata_to_add <- tibble::as_tibble(
  utils::read.table(metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE, comment.char = "")
)
metadata %>% add_row(metadata_to_add) -> metadata

#### INCLUDES #####

## prepare raster data
metadata_path <- file.path("inst", "extdata", "data", "cap-bc-includes-metadata.csv")
## Import formatted csv (metadata) as tibble
metadata_to_add <- tibble::as_tibble(
  utils::read.table(metadata_path, stringsAsFactors = FALSE, sep = ",", header = TRUE, comment.char = "")
)
metadata %>% add_row(metadata_to_add) -> metadata