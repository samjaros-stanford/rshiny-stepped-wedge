rm(list=ls())

# Load libraries ===============================================================
library(shiny)
library(ggplot2)
library(dplyr)

# Shiny Options ================================================================
options(shiny.fullstacktrace = TRUE)
do.debug = T

## Create default data =========================================================
default = list()

# Default values for study setup
default$study$n_intervenions = NA_integer_
default$study$n_groups = NA_integer_
default$study$time_units = ""
default$study$group_units = "Group"

# Default values for visualization setup
