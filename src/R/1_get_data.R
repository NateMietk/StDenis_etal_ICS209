
x <- c("data.table", "tidyverse", "tidyverse", "magrittr", "sf", "gridExtra", "raster", "lme4", 'RColorBrewer',
       'lubridate', "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel", "broom", 'openxlsx', 'reshape', 'pdftools')
lapply(x, library, character.only = TRUE, verbose = FALSE)

# Load all external custom functions
file_sources <- list.files(file.path('src', 'functions'), pattern="*.R", 
                           full.names=TRUE, ignore.case=TRUE)
invisible(sapply(file_sources, source, .GlobalEnv))

proj_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs "

# Raw data folders
data_dir <- "data"
raw_dir <- file.path(data_dir, "raw")
raw_dir_us <- file.path(raw_dir, "cb_2016_us_state_20m")
raw_dir_gacc <- file.path(raw_dir, "gacc")
raw_dir_mtbs <- file.path(raw_dir, "mtbs")
raw_dir_ecoregion <- file.path(raw_dir, "ecoregions")
raw_dir_ecoregionl4 <- file.path(raw_dir, "us_eco_l4")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(data_dir, raw_dir, raw_dir_ag, raw_dir_us, raw_dir_mtbs, raw_dir_gacc, raw_dir_ecoregion, raw_dir_ecoregionl4)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Input data folders
bounds_dir <- file.path(data_dir, 'bounds')
fire_dir <- file.path(data_dir, 'fire')
ics209_dir <- file.path(data_dir, "ics_209")
ics209_input_dir <- file.path(ics209_dir, "input_tbls")
ics209_output_dir <- file.path(ics209_dir, "output_tbls")
ics209_spatial_dir <- file.path(ics209_dir, "spatial")
ecoregion_dir <- file.path(bounds_dir, "ecoregions")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(bounds_dir, ics209_dir, ics209_input_dir, ics209_output_dir, ics209_spatial_dir, ecoregion_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Figure and Table directories
results_dir <- 'results'
draft_figs_dir <- file.path(results_dir, 'draft_figures')
draft_table_dir <- file.path(results_dir, 'draft_tables')

var_dir <- list(results_dir, draft_figs_dir, draft_table_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

