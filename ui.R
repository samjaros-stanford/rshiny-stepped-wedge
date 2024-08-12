
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
       "Follow the input prompts to simulate a stepped wedge trial",
       HTML("<br><br>"))
)

## Input tabs ==================================================================
## Users walk through the tabs in numeric order to construct a study
## All page logic is handled in server.R
## Tabs denoted with a letter are optional offshoots of that numbered tab
##   offering advanced options

### Tab 1: Trial Skeleton ------------------------------------------------------
### Input basic numbers that allows the code to construct a very basic stepwise
###   trial. Naming and tweaking occurs in subsequent menus
tab1 <- tabPanel(
  title = "1. Trial",
  value = "tab1",
  helpText("This basic information will construct your initial trial. Return to
           this tab if you need to add more interventions or cohorts."),
  numericInput(
    inputId = "n_INT",
    label = "How many interventions in your study?",
    value = default$study$n_INT,
    min = 1,
    max = 16,
    step = 1
  ),
  numericInput(
    inputId = "n_COH",
    label = "How many cohorts are your study?",
    value = default$study$n_COH,
    min = 1,
    max = 32,
    step = 1
  ),
  ### --- Page actions
  ### Invisible button for alignment
  ### There is no back since this is the first page (for now)
  actionButton(
    "blank", 
    "",
    class = "invisible"),
  nextButton("tab1_next")
)
### Tab 2: Naming --------------------------------------------------------------
tab2 <- tabPanel(
  title = "2. Names",
  value = "tab2",
  helpText("Provide names that will be used for the data input and plotting."),
  uiOutput("INT_names"),
  ### --- Page actions
  backButton("tab2_back"),
  nextButton("tab2_next")
)
### Tab 3: Tailoring -----------------------------------------------------------
tab3 <- tabPanel(
  title = "3. Timing",
  value = "tab3",
  helpText("Provide the duration of each intervention."),
  uiOutput("INT_timings"),
  ### --- Page actions
  actionLink(
    inputId = "advanced_COH",
    label = "Edit Interventions by Cohort"
  ),
  HTML("<br>"),
  backButton("tab3_back"),
  nextButton("tab3_next")
)
### Tab 4: Visualization -------------------------------------------------------
tab4 <- tabPanel(
  title = "4. Plot",
  value = "tab4",
  helpText("Customize the plotting and save your figure."),
  textInput(
    inputId = "time_units",
    label = "What are the time units for your study?",
    value = default$study$time_units
  ),
  ### --- Page actions
  ### There is no next since this is the last page (for now)
  backButton("tab4_back")
)
### Cohort Setup Tab -----------------------------------------------------------
### Declared in 1_dynamic_ui.R for showTab functionality

## Construct tabset
##   Users should not see all tabs at once
##   The tabs contain buttons to navigate through the system
tabs <- tabsetPanel(
  id = "input_tabs",
  selected = "tab1",
  type = "pills",
  tab1, 
  tab2,
  tab3,
  tab4
)

# Sidebar ======================================================================
sidebar <- sidebarPanel(tabs)

# Main =========================================================================
main <- mainPanel(
  plotOutput("plot"),
  # --- For testing ---
  tableOutput("config"),
  tableOutput("study"), 
  # --- For testing ---
  verbatimTextOutput("input_list")
)

# UI: Page layout ==============================================================
ui <- fluidPage(
  # Create better page title
  tags$head(HTML(paste0("<title>", page_title, "</title>"))),
  # Enable right justification and invisibility
  tags$head(tags$style(".rightAlign{float:right;}
                        .invisible{position:none;}")),
  # Use first-order elements from above to construct the page
  title, 
  sidebarLayout(sidebar, main)
)
