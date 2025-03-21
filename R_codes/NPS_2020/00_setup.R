# Authors: Abigail O. Asare and  Laura Schürer

# Date:26/02/2025

# Carbon Pricing in Tanzania ----------------------------------------

# General Setup -----------------------------------------------------------
library("parallel")

# Detect Cores available for parallel computing
cores <- ifelse("Windows" %in% Sys.info(), 0L, parallel::detectCores())

# Display error messages in English
Sys.setenv(LANG = "en")

# Directories -------------------------------------------------------------
dir <- list()

## Raw Data locations ------------------------------------------------------
dir[["tza"]]  <- file.path("data-raw", "TZA_2020_NPS-R5_v02_M_STATA14")
dir[["other"]]  <- file.path("data-raw")

## Processed Data locations ----------------------------------------------------
dir[["tza_processed"]]  <- file.path("data-processed")


## Analysis Data locations ------------------------------------------------------
dir[["analysis"]] <- file.path("data-4a")

## Output locations ------------------------------------------------------
dir[["tables"]]  <- file.path("output", "tables")
dir[["figures"]] <- file.path("output", "figures")


## Make directories --------------------------------------------------------
lapply(dir, function(d) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
})
