rm(list=ls())

# Load libraries ===============================================================
library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)

# App options ==================================================================
# Define the maximum number of interventions and cohorts possible
max_n_INT = 16
max_n_COH = 16
## Debugging
do.plot = T

# Shiny Options ================================================================
options(shiny.fullstacktrace = TRUE)

# Create default data =========================================================
default = list()

# Default values for study setup
default$study$n_INT = NA_integer_
default$study$n_COH = NA_integer_
default$study$time_units = ""
default$study$COH_units = "Group"
default$study$INT_length = 1
default$study$INT_start_max = Inf
default$study$INT_end_max = Inf
default$study$INT_gap = 0
default$study$INT_null_offset = NA # When offset is uninitialized, prevents phantom study from being generated
default$study$INT_offset = 1

# Default values for visualization setup
