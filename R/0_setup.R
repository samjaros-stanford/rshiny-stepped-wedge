rm(list=ls())

options(shiny.fullstacktrace = TRUE)

do.debug = T

## --- Load libraries --------------------
library(shiny)
library(ggplot2)
library(stringr)
#library(grid) 
library(gridBase)
library(gridExtra)
#library(gridExtra) #https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
#library(shinythemes)

options(shiny.fullstacktrace = TRUE)

## --- Create default data --------------------
LoadSavedInputValues <- FALSE

all.my.files <- list.files() 
all.inputVals.Files <- all.my.files[grep("inputVals",all.my.files)]

if(length(all.inputVals.Files) > 0) {
  StartingInputValueFilename <- rev(sort(all.inputVals.Files))[1]
} else {
  StartingInputValueFilename <- ""
}

if(LoadSavedInputValues & file.exists(StartingInputValueFilename)) {
  load(file = StartingInputValueFilename)
  GetExistingInputCheck <- inputCheck
} else {
  GetExistingInputCheck <- list()
  GetExistingInputCheck$AddDateAndTime <- TRUE
  GetExistingInputCheck$Alpha <- 0.05
  GetExistingInputCheck$AssignmentPlotDirectory <- ""
  GetExistingInputCheck$AssignmentPlotFileName <- "AssignmentPlot"
  GetExistingInputCheck$AssignmentPlotTitle <- ""
  GetExistingInputCheck$AssignmentPlotXaxisLabel <- ""
  GetExistingInputCheck$AssignmentPlotYaxisLabel <- ""
  GetExistingInputCheck$Blocking <- ""
  GetExistingInputCheck$CatchUpEndingSteps <- "n"
  GetExistingInputCheck$ChangeLabels <- FALSE
  # GetExistingInputCheck$choice <- "Run program in normal mode"
  # GetExistingInputCheck$choice-selectized <- 1
  GetExistingInputCheck$CompareInterventions <- FALSE
  GetExistingInputCheck$Control <- FALSE
  GetExistingInputCheck$DesiredPower <- 0.8
  GetExistingInputCheck$EntitiesPerStep <- 1
  GetExistingInputCheck$EntityName <- ""
  GetExistingInputCheck$FileForPlotting <- FALSE
  GetExistingInputCheck$InterventionBlocksWhereStepsOccur <- 1
  GetExistingInputCheck$ICCforClustering <- 0.02
  GetExistingInputCheck$ICCforEntities <- 0.02
  GetExistingInputCheck$ICCforTimePeriod <- 0.02
  GetExistingInputCheck$ImpactOnDifferentPopulations <- FALSE
  GetExistingInputCheck$ImpactOverTime <- FALSE
  GetExistingInputCheck$InterventionChangeOverTime <- ""
  GetExistingInputCheck$InterventionConditions <- ""
  GetExistingInputCheck$InterventionEffect <- 0.5
  GetExistingInputCheck$LevelWhereMeasured <- ""
  GetExistingInputCheck$MaxRunTime <- ""
  GetExistingInputCheck$MaxTimePeriodsInStudy <- ""
  GetExistingInputCheck$NamesOfEachStep <- ""
  GetExistingInputCheck$NamesOfEachTimePoint <- ""
  GetExistingInputCheck$Nesting <- ""
  GetExistingInputCheck$NumberOfSteps <- 4
  GetExistingInputCheck$NumberPeriodsEachCondition <- 1
  GetExistingInputCheck$OpenOutcome <- FALSE
  GetExistingInputCheck$OutcomeName <- ""
  GetExistingInputCheck$PopulationPercents <- ""
  GetExistingInputCheck$Precision <- 0.02
  GetExistingInputCheck$SaveAssignmentPlot <- 0
  GetExistingInputCheck$SetFixedEffects <- FALSE
  GetExistingInputCheck$SetLevels <- FALSE
  GetExistingInputCheck$SetRandomEffects <- FALSE
  GetExistingInputCheck$StaggerInitialSteps <- "n"
  GetExistingInputCheck$TimeElementName <- ""
  GetExistingInputCheck$TimeElementsPerPeriod <- 1
  # GetExistingInputCheck$TypeOutcome <- "Continuous"
  # GetExistingInputCheck$TypeOutcome-selectized <- 1
  GetExistingInputCheck$WhichDesignTabPanel <- "AssignNumbersTabPanel"
  GetExistingInputCheck$maxXLabelValues <- 10
  GetExistingInputCheck$MakeAdjustments <- FALSE
}