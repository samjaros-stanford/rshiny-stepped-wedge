
## Depreciated
##   New GUI constructed below old GUI to preserve code as a reference

# Define UI for app that simulates Stepped Wedge  ----
ui <- fluidPage(
  ###:  Open fluidPage  ----
  sidebarLayout(
    ###: :  Open sidebarLayout ---- 
    # contains sidebarPanel(),mainPanel()
    sidebarPanel( ###: : : Open sidebarPanel for all tabs ---- 
                  tabsetPanel(
                    ###: : : : Open tabsetPanel for All ----
                    # for ASSIGN, ... , POWER 
                    tabPanel(
                      "ASSIGN",
                      ###: : : : : Open tabPanel ("ASSIGN") ----
                      tabsetPanel(
                        id = "WhichDesignTabPanel",
                        ###: : : : : : Open tabsetPanel for WhichDesignTabPanel  ----
                        tabPanel(
                          "ASSIGN: Names",
                          value = "AssignNamesTabPanel",
                          ###: : : : : : : Open tabPanel ("ASSIGN: Names") ----
                          # input Name for Intervention(s)
                          textInput(
                            "InterventionConditions",
                            "Name(s) of Intervention/Implementation Condition(s) separated by ',' and ':' for head-to-head, e.g., A1:A2,B,C",
                            value = GetExistingInputCheck$InterventionConditions,
                            placeholder = "Please enter intervention name(s)."),
                          # input Name for Time Unit
                          textInput(
                            "TimeElementName",
                            "Element of Time Used for Plotting, e.g., Month",
                            value = GetExistingInputCheck$TimeElementName),
                          # input Name for Entities - For Now only 1 Unit permitted!!!
                          textInput(
                            "EntityName",
                            "Generic Name for Entity Assigned at Each Step, e.g., Clinic",
                            value = GetExistingInputCheck$EntityName)), ###: : : : : : : Close tabPanel ("ASSIGN: Names") ----
                        tabPanel(
                          "ASSIGN: Numbers",
                          value = "AssignNumbersTabPanel",
                          ###: : : : : : : Open tabPanel ("ASSIGN: Numbers") ----
                          numericInput(
                            "NumberOfSteps",
                            "Total Number of Steps in this Assignment",
                            value = GetExistingInputCheck$NumberOfSteps),
                          uiOutput("InputEntityName"), ###: : : : : : : : : uiOutput("InputEntityName") ----
                          uiOutput("UseTimeElementNameForNumberPeriodsEachCondition"), ### : : : : : uiOutput("UseTimeElementNameForNumberPeriodsEachCondition")
                          ### Adjustments##* checkbox
                          checkboxInput(
                            "MakeAdjustments",
                            "---Adjust Time, Start, End, Steps",
                            value = GetExistingInputCheck$MakeAdjustments),
                          conditionalPanel(
                            ###: : : : : : : : : Open Conditional Panel for ChangeLabels ----
                            condition = "input.MakeAdjustments == true",
                            uiOutput("UseTimeElementNameForTimeElementsPerPeriod"), ### : : : : : : : : :  uiOutput("UseTimeElementNameForTimeElementsPerPeriod")  ----
                            textInput(
                              "StaggerInitialSteps",
                              "ADJUST BEGINNING: Are Start Periods Staggered across Steps (y/n)?\n(If different starting periods by Step give number separated by ',')",
                              value = GetExistingInputCheck$StaggerInitialSteps),
                            textInput(
                              "CatchUpEndingSteps",
                              "ADJUST ENDING: Are Ending Periods Staggered across Steps (y/n)?\n(If different starting periods by Step give number separated by ',')",
                              value = GetExistingInputCheck$CatchUpEndingSteps),
                            textInput(
                              "InterventionBlocksWhereStepsOccur",
                              "ADJUST STEPS: Number of Time Periods Where Steps Occur (if more than one separate by ',')",
                              value = GetExistingInputCheck$InterventionBlocksWhereStepsOccur))), ###: : : : : : : Close tabPanel ("ASSIGN: Numbers") ----
                        tabPanel(
                          "ASSIGN: Save Plot",
                          value = "SavePlot",
                          ###: : : : : : : Open tabPanel ("ASSIGN: Save Plot") ----
                          # Change Labels
                          checkboxInput(
                            "ChangeLabels",
                            "---Change Plot Labels and Dimensions",
                            value = GetExistingInputCheck$ChangeLabels),
                          conditionalPanel(
                            ###: : : : : : : : : Open Conditional Panel for ChangeLabels ----
                            condition = "input.ChangeLabels == true",
                            textInput(
                              "AssignmentPlotTitle",
                              "Plot Title",
                              value = GetExistingInputCheck$AssignmentPlotTitle),
                            textInput(
                              "AssignmentPlotYaxisLabel",
                              "Y Axis Label",
                              value = GetExistingInputCheck$AssignmentPlotYaxisLabel),
                            textInput(
                              "AssignmentPlotXaxisLabel",
                              "X Axis Label",
                              value = GetExistingInputCheck$AssignmentPlotXaxisLabel),
                            textInput(
                              "NamesOfEachStep",
                              "Names of Each Step",
                              value = GetExistingInputCheck$NamesOfEachStep),
                            textInput(
                              "NamesOfEachTimePoint",
                              "Names of Each Time Point",
                              value = GetExistingInputCheck$NamesOfEachTimePoint),
                            textInput(
                              "MaxTimePeriodsInStudy",
                              "Set the Last Time Period to Display on Assignment Plot (blank or >0)",
                              value = GetExistingInputCheck$MaxTimePeriodsInStudy),
                            textInput(
                              "MinStartTimePeriodInStudy",
                              "Set the First Time Period to Display on Assignment Plot (blank, can be <0)",
                              value = GetExistingInputCheck$MinStartTimePeriodInStud)), ###: : : : : : : : : Close conditionalPanel for Compare Interventions  ----
                          checkboxInput(
                            "FileForPlotting",
                            "--- Specify AssignmentPlot File Name and Print",
                            value = GetExistingInputCheck$FileForPlotting),
                          conditionalPanel(
                            ###: : : : : : : : : Open Conditional Panel for FileForPlotting ----
                            condition = "input.FileForPlotting == true",
                            textInput(
                              "AssignmentPlotDirectory",
                              "Directory",
                              value = GetExistingInputCheck$AssignmentPlotDirectory),
                            textInput(
                              "AssignmentPlotFileName",
                              "Base File Name",
                              value = GetExistingInputCheck$AssignmentPlotFileName),
                            checkboxInput(
                              "AddDateAndTime",
                              "Add Date and Time to FileName",
                              value = GetExistingInputCheck$AddDateAndTime),
                            checkboxInput(
                              "Details",
                              "Details",
                              value = GetExistingInputCheck$Details),
                            conditionalPanel(
                              ###: : : : : : : :  Open conditionalPanel for Details ----
                              condition = "input.Details == true",
                              selectInput(
                                "PlotFileType",
                                "Plot File Type",
                                choices = list(".pdf", ".emf", ".wmf", ".jpg"),
                                selected = ".pdf"),
                              numericInput(
                                "maxXLabelValues",
                                "Maximum Number of X Labels on Assignment Plot",
                                value = GetExistingInputCheck$maxXLabelValues)), ###: : : : : : : : Close conditionalPanel for Details  ----
                            actionButton(
                              "SaveAssignmentPlot",
                              "Save Assignment Plot and Input Parameters")) ###: : : : : : : : : Close conditionalPanel for FileForPlotting  ----
                        ) ###: : : : : : : Close tabPanel ("ASSIGN: Save Plot") ----
                      ) ####: : : : : : Close tabsetPanel for WhichDesignTabPanel  ----
                    ), #### : : : : :  Close tabPanel ("ASSIGN") ----
                    tabPanel(
                      "MODEL",
                      ###: : : : : Open tabPanel ("MODEL") ----
                      #...................................................
                      # Model Section 
                      #...................................................
                      tabPanel(
                        "MODEL: Outcome",
                        ###: : : : : : : Open tabPanel ("MODEL: Outcome") ----
                        # Outcome
                        checkboxInput(
                          "OpenOutcome",
                          "---Outcome Variable",
                          value = GetExistingInputCheck$OpenOutcome),
                        conditionalPanel(
                          ###: : : : : : : :   Open conditionaPanel for Outcome ----
                          condition = "input.OpenOutcome == true",
                          textInput(
                            "OutcomeName",
                            "Name of Outcome Variable",
                            value = GetExistingInputCheck$OutcomeName),
                          textInput(
                            "LevelWhereMeasured",
                            "On Which Level is this Variable Measured?",
                            value = GetExistingInputCheck$LevelWhereMeasured),
                          selectInput(
                            "TypeOutcome",
                            "Type of Outcome Variable",
                            choices = list("Continuous", "Binary", "Count")))
                        ###: : : : : : : :  Close conditionalPanel for Outcome ----
                      ),
                      ###: : : : : : : Close tabPanel ("MODEL: Outcome") ----
                      # Includes Fixed Effects and Random Effects
                      # Fixed Effect
                      checkboxInput(
                        "SetFixedEffects",
                        "---Set Fixed Effects",
                        value = GetExistingInputCheck$SetFixedEffects),
                      conditionalPanel(
                        ###: : : : : : :  Open Conditional Panel for Fixed Effects ----
                        condition = "input.SetFixedEffects == true",
                        checkboxInput(
                          "CompareInterventions",
                          "------Fixed Effect- Compare Interventions Against Each Other",
                          value = GetExistingInputCheck$CompareInterventions),
                        conditionalPanel(
                          ###: : : : : : : : : Open conditionalPanel for Compare Interventions  ----
                          condition = "input.CompareInterventions == true",
                          numericInput("InterventionEffect",
                                       "Magnitude of Intervention Effect",
                                       value = GetExistingInputCheck$InterventionEffect)),
                        ###: : : : : : : : : Close conditionalPanel for Compare Interventions  ----
                        checkboxInput(
                          "ImpactOverTime",
                          "------Fixed Effect- Compare Intervention Impact Over Time",
                          value = GetExistingInputCheck$ImpactOverTime),
                        conditionalPanel(
                          ###: : : : : : : : : Open conditionalPanel for Impact Over Time ----
                          condition = "input.ImpactOverTime == true",
                          textInput(
                            "InterventionChangeOverTime",
                            "Name of Function for Intervention Change Over Time",
                            value = GetExistingInputCheck$InterventionChangeOverTime)),
                        ##: : : : : : : : : Close conditional panel for Impact Over Time  ----
                        checkboxInput(
                          "ImpactOnDifferentPopulations",
                          "------Fixed Effect- Compare Intervention Impact On Different Populations",
                          value = GetExistingInputCheck$ImpactOnDifferentPopulations),
                        conditionalPanel(
                          ###: : : : : : : : : Open conditionalPanel for Impact on Different Populations ----
                          condition = "input.ImpactOnDifferentPopulations == true",
                          textInput(
                            "InterventionChangeOverTime",
                            "Names of Populations (separate by ',')",
                            value = GetExistingInputCheck$InterventionChangeOverTime),
                          textInput(
                            "PopulationPercents",
                            "Percent of Population (separate by ',')",
                            value = GetExistingInputCheck$PopulationPercents))
                        ###: : : : : : : : : Close conditionalPanel for Impact on Different Populations ----
                      ),
                      ###: : : : : : : Close Conditional Panel for Fixed Effects ----
                      checkboxInput(
                        "SetRandomEffects",
                        "---Set Levels and Random Effects",
                        value = GetExistingInputCheck$SetRandomEffects),
                      conditionalPanel(
                        ###: : : : : : :  Open conditionaPanel for Random Effects  ----
                        condition = "input.SetRandomEffects == true",
                        checkboxInput(
                          "SetLevels",
                          "---Set Levels",
                          value = GetExistingInputCheck$SetLevels),
                        conditionalPanel(
                          ###: : : : : : : : : Open conditionaPanel for Levels  ----
                          condition = "input.SetLevels == true",
                          textInput(
                            "Blocking",
                            "Name(s) of (any) Blocking Levels Above Randomization Level, separate by ','",
                            value = GetExistingInputCheck$Blocking),
                          textInput(
                            "Nesting",
                            "Name(s) of (any) Nesting Levels below Randomization Level, separate by ','",
                            value = GetExistingInputCheck$Nesting)),
                        ## : : : : : : : : : Close conditionaPanel for Levels  ----
                        # Random Effects Section
                        numericInput(
                          "ICCforEntities",
                          "Random Effect- ICC for Entities",
                          value = GetExistingInputCheck$ICCforEntities),
                        numericInput(
                          "ICCforTimePeriod",
                          "Random Effect- ICC for Time",
                          value = GetExistingInputCheck$ICCforTimePeriod),
                        numericInput("ICCforClustering",
                                     "Random Effect- ICC for Clustering of Multiple Entities",
                                     value = GetExistingInputCheck$ICCforClustering)) # : : : : : : : Close Conditional Panel for Random Effects  ----
                    ), ###: : : : : Close tabPanel ("MODEL") ----
                    tabPanel(
                      "TEST",
                      ###: : : : : Open tabPanel ("TEST") ----
                      # Testing Section
                      conditionalPanel(
                        ###:  Open conditionaPanel for testing
                        condition = "input.TEST == true",
                        numericInput(
                          "Alpha",
                          "Alpha (Type I Error)",
                          min = 0, max = 1,
                          value = GetExistingInputCheck$Alpha),
                        numericInput(
                          "DesiredPower",
                          "Desired Power",
                          min = 0, max = 1,
                          value = GetExistingInputCheck$DesiredPower),
                        numericInput(
                          "Precision",
                          "Precision (to 95% Confidence) for Estimating Power",
                          min = 0, max = 1,
                          value = GetExistingInputCheck$Precision)) ###:  Close conditionalPanel for Test
                    ), ###: : : : : Close tabPanel ("TEST") ----
                    tabPanel(
                      "POWER",
                      ###: : : : : Open tabPanel ("POWER") ----
                      # checkboxInput(
                      #   "ShowLanguageAndGlossary", 
                      #   "Show Language and Glossary",
                      #    value = FALSE),
                      # conditionalPanel(
                      #   ###: : : : : : Open conditionalPanel Glossary -- 
                      #   condition = "input.ShowLanguageAndGlossary == true" ,
                      #   selectInput(
                      #     "Language",
                      #     "Language",
                      #     selected = "English",
                      #     choices = c("English" = "English",
                      #                 "Spanish/españole" = "Spanish")),
                      #   textInput(
                      #     "LookupDefinition", 
                      #     "Define any term used in this program", 
                      #     value = "")), ###: : : : : : Close conditionalPanel Glossary --
                      checkboxInput(
                        "Control",
                        "Control",
                        value = GetExistingInputCheck$Control),
                      conditionalPanel(
                        ###: : : : : :  Open conditionalPanel for Control ----
                        condition = "input.Control == true",
                        selectInput(
                          "choice",
                          "Running Mode",
                          choices = list("Run program in normal mode",
                                         "Run program in parallel mode",
                                         "Estimate Simulation Time",
                                         "Display All Input Values")),
                        numericInput("MaxRunTime",
                                     "Maximum Time for Simulation to Run in Minutes: ",
                                     value = GetExistingInputCheck$MaxRunTime)) ###: : : : : : Close conditionalPanel for Control  ----
                    ) ###: : : : : Close tabPanel ("POWER") ----
                  ) ###: : : :  Close tabsetPanel for All ----
    ), ###: : :  Close sidebarPanel  ---
    mainPanel(
      ###: : :  Open mainPanel ----
      # verbatimTextOutput("ReturnDefinition"),
      textOutput("FilePrintedOK"),
      plotOutput("AssignmentPlot") ##: : : : plotOutput("AssignmentPlot") ----
    ) ###: : : Close MainPanel  ----
  )  ###: :  Close sidebarLayout  ----
)  ###:  Close fluidPage  ----