
################################################################################
## New GUI Definition
##   Emphasize constructing pages outside of the final function call
##   Element names should reflect their final position in the GUI
##   Use comments to break the specs into chunks
##   Specify from inside to the outside

# Repeated GUI elements ========================================================
## Creates a next button given the current tab as input
## Note Next buttons are always right justified
nextButton <- function(id) {
  actionButton(
    inputId = id,
    label = "Next",
    icon = icon("arrow-right"),
    class = "rightAlign"
  )
}
## Creates a back button given the current tab as input
backButton <- function(id) {
  actionButton(
    inputId = id,
    label = "Back",
    icon = icon("arrow-left")
  )
}

# Title ========================================================================
title <- titlePanel(
  list(h1("Stepped Wedge Trial Emulator"),
       "Follow the input prompts to simulate a stepped wedge trial")
)

## Input tabs ==================================================================
## Users walk through the tabs in numeric order to construct a study
## All page logic is handled in server.R
## Tabs denoted with a letter are optional offshoots of that numbered tab
##   offering advanced options

### Tab 1: Initial input -------------------------------------------------------
### Input basic numbers that allows the code to construct a very basic stepwise
###   trial. Naming and tweaking occurs in subsequent menus
tab1 <- tabPanelBody(
  value = "tab1",
  numericInput(
    inputId = "n_interventions",
    label = "How many interventions in your study?",
    value = default$study$n_interventions,
    min = 1,
    max = 256,
    step = 1
  ),
  numericInput(
    inputId = "n_groups",
    label = "How many participant groups in your study?",
    value = default$study$n_groups,
    min = 1,
    max = 256,
    step = 1
  ),
  textInput(
    inputId = "time_units",
    label = "What are the time units for your study?",
    value = default$study$time_units
  ),
  checkboxInput(
    inputId = "is_head_to_head",
    label = "Includes head-to-head intervention(s)"
  ),
  ### --- Page actions
  ### There is no back since this is the default page (for now)
  ### Advanced input presents the custom input tool
  actionLink(
    inputId = "advanced_input",
    label = "Advanced Input"
  ),
  nextButton("tab1_next")
)
### Tab 1a: Advanced data input ------------------------------------------------
tab1a <- tabPanelBody(
  value = "tab1a",
  helpText("This is where you will be able to paste the series of numbers and
           tabs that the old document could handle"),
  backButton("tab1a_back"),
  nextButton("tab1a_next")
)
### Tab 1b: Head-to-head trial -------------------------------------------------
tab1b <- tabPanelBody(
  value = "tab1b",
  helpText("tab1b"),
  backButton("tab1b_back"),
  nextButton("tab1b_next")
)
### Tab 2: Input naming --------------------------------------------------------
tab2 <- tabPanelBody(
  value = "tab2",
  helpText("Tab 2"),
  uiOutput("intervention_names"),
  ### Advanced viz presents the custom viz tool
  actionLink(
    inputId = "advanced_viz",
    label = "Advanced Visualization"
  ),
  actionButton(
    inputId = "tab2_back",
    label = "Back",
    icon = icon("arrow-left"),
    class = "rightAlign"
  )
)
### Tab 2a: Output customization -----------------------------------------------
tab2a <- tabPanelBody(
  value = "tab2a",
  helpText("Tab 2a"),
  ### --- Page actions
  ### There is no next since this is the last page (for now)
  ### Back returns to the basic viz page
  backButton("tab2a_back")
)
### Tab 2b: Study customization ------------------------------------------------

## Construct tabset
##   Users should not see all tabs at once
##   The tabs contain buttons to navigate through the system
tabs <- tabsetPanel(
  id = "input_tabs",
  selected = "tab1",
  type = "hidden",
  tab1, 
  tab1a,
  tab1b,
  tab2, 
  tab2a
)

# Sidebar ======================================================================
sidebar <- sidebarPanel(tabs)

# Main =========================================================================
main <- mainPanel(
  verbatimTextOutput("input_list"), # For testing
  plotOutput("plot"),
  textOutput("save_status")
)

# UI: Page layout ==============================================================
ui <- fluidPage(
  # Enable right justification
  tags$head(tags$style(".rightAlign{float:right;}")),
  # Use first-order elements from above to construct the page
  title, 
  sidebarLayout(sidebar, main)
)
