# install.packages(c(
#   "devtools",
#   "dplyr",
#   "tidyr",
#   "sf",
#   "stars"
#   "vroom",
#   "here"
# ))
# devtools::install_github("ecosystem-assessments/motifcensus")
# devtools::install_github("ecosystem-assessments/rcea")
library(rcea)
library(dplyr)
library(tidyr)
library(stars)

# Specify and create output folder
per <- "2016_2021"
output <- here::here("output_nceamm_pam_wsdb", "ncea", per)
# output <- "~/scratch/output_nceamm_pam_wsdb/ncea/2010_2015/"
rcea::chk_create(output)

# Load files for analysis
load("data/FormatData_nceamm_pam_wsdb/biotic.RData")
load("data/FormatData_nceamm_pam_wsdb/species_sensitivity.RData")
load("data/FormatData_nceamm_pam_wsdb/metaweb.RData")
load("data/FormatData_nceamm_pam_wsdb/TrophicSensitivity.RData")
load("data/FormatData_nceamm_pam_wsdb/driversRaster.RData")
drivers <- drivers[[per]]

# Network-scale cumulative effects assessment
# focus <- "uria.137041"
# i <- which(names(biotic) == focus)

i <- as.numeric(commandArgs(trailingOnly = TRUE))
rcea::ncea_species(
  focus = names(biotic)[i],
  drivers,
  biotic,
  species_sensitivity,
  metaweb,
  trophic_sensitivity,
  w_d = 0.5,
  w_i = 0.25,
  output = output
)
