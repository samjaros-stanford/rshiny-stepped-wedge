rm(list=ls())

# Load libraries ===============================================================
library(dplyr)
library(ggplot2)
library(shiny)
library(sortable)

# App options ==================================================================
# Page title
page_title = "Stepped Wedge Trial Emulator"
# Define the maximum number of interventions and cohorts possible
max_n_INT = 8
max_n_COH = 16
## Change what is outputted for debugging purposes
## In normal operations, these should all be FALSE
do.input = F        # Should the input list be displayed?
suppress.plot = F   # Should the plot be suppressed?
do.table = F        # Should the configuration and study tables be displayed?

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
default$study$INT_start_max = 1
default$study$INT_end_max = 1
default$study$INT_gap = 0
default$study$INT_null_offset = NA # When offset is uninitialized, prevents phantom study from being generated
default$study$INT_offset = 1

# Default values for visualization setup
default$plot$colors <- c("#332288","#88CCEE","#44AA99","#117733","#999933",
                         "#DDCC77","#CC6677","#882255","#AA4499","#DDDDDD")
