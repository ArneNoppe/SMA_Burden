## Setup of environment - non-confidential
# ---------------------------------------------
# Packages
library(ggplot2)
library(csv)
library(readxl)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(devtools)
devtools::install_github("sciensanogit/BeBOD")

# Define disability weights by health state - ordered from "mild" to "severe"
weightsSMA <- data.frame(MOT = c(0.010, 0.061, 0.268, NA, NA, NA),
                  PULM = c(0.025, 0.284, 0.418, NA, NA, NA),
                  MSK = c(0.079, 0.112, 0.028, 0.117, 0.317, 0.581),
                  SCOL = c(0.372, NA, NA, NA, NA, NA),
                  SPEECH  = c(0.051, NA, NA, NA, NA, NA),
                  FEED = c(0.163, NA, NA, NA, NA, NA))
