################################################################################
# New Server
#  For now, just prints out the list provided by the input

server <- function(input, output){
  # Tab Functionality ==========================================================
  ## --- Next ---
  observeEvent(input$tab1_next, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  observeEvent(input$tab2_next, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab3")
  })
  observeEvent(input$tab3_next, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab4")
  })
  ## --- Back ---
  observeEvent(input$tab2_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab1")
  })
  observeEvent(input$tab3_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  observeEvent(input$tab4_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab3")
  })
  ## --- Advanced ---
  observeEvent(input$advanced_COH, {
    if(input$advanced_COH > 1){
      updateTabsetPanel(inputId = "input_tabs", selected = "tab_COH")
    } else {
      appendTab(inputId = "input_tabs",
                # Add tabPanel for cohort customization
                tabPanel(
                  title = "Cohorts",
                  value = "tab_COH",
                  helpText("Customize the interventions for each cohort."),
                  uiOutput("COH_customization")
                ),
                select = T)
    }
  })
  
  # Dynamic UI Elements ========================================================
  # Functions to create UI elements are in 2_dynamic_ui.R. Reactive elements 
  #   only contain decisions on when the UI should be updated
  ## --- Intervention Naming ---
  ## Creates a number of text inputs equal to the number of interventions
  output$INT_names <- renderUI({
    # Isolated values will only be recalculated if the tab changes
    input$input_tabs
    n_int = as.integer(isolate(input$n_INT))
    if(is.na(n_int) | n_int<1){
      return(helpText("Number of interventions is invalid"))
    }
    isolate(make_INT_name_ui(input))
  })
  ## --- Intervention Timing ---
  ## Creates a number of duration inputs equal to the number of interventions
  output$INT_timings <- renderUI({
    # Isolated values will only be recalculated if the tab or number changes
    input$input_tabs
    n_int = as.integer(input$n_INT)
    if(is.na(n_int) | n_int<1){
      return(helpText("Number of interventions is invalid"))
    }
    c(isolate(make_INT_timing_ui(input)),
      list(
        numericInput(
          inputId = "INT_offset",
          label = "Intervention Offset Between Groups",
          value = ifelse(is.null(input$INT_offset),
                         default$study$INT_offset,
                         input$INT_offset))))
  })
  ## --- Cohort Customizations ---
  ## Generates the cohort customization tabset
  output$COH_customization <- renderUI({
    # Isolated values will only be recalculated if the main tabset changes
    input$input_tabs
    n_COH = as.integer(input$n_COH)
    # Return help text if the group numbers are invalid
    if(is.na(n_COH) | n_COH<1){
      return(helpText("Number of groups is invalid"))
    }
    # Generate a tabset with a dynamic number of tabs
    # Isolated unless main tab is changed
    # do.call() required so all tabPanels are arguments, not just a list
    isolate(do.call(
      tabsetPanel,
      append(
        list(
          id = "COH_tabs",
          type = "pills"
        ),
        lapply(1:n_COH,
               make_COH_customization_tab_ui,
               input=input
        )
      )
    ))
  })
  
  # Study Creation =============================================================
  ## --- Assemble needed variables ---
  ## Dynamically observe intervention lengths based on the number of
  ##   intervention lengths, assigning them a value of 0 if uninitialized
  INT_config <- reactive({
    # --- Exceptions ---
    # Stop if intervention number is uninitialized
    # Stop if offset is uninitialized (timing tab uninitialized)
    if(is.na(input$n_INT) | is.null(input$INT_offset)){
      return(NULL)
    }
    
    data.frame(INT = 1:input$n_INT,
               INT_length = sapply(
                 1:input$n_INT, 
                 function(i){
                   input[[paste0("INT_length_",i)]]
                 }),
               INT_gap = sapply(
                 1:input$n_INT, 
                 function(i){
                  input[[paste0("INT_gap_",i)]]
                 }),
               # Offset has special case where if it is completely uninitialized,
               #   it needs to be 0/NA so that a phantom study is not created.
               #   If it has been initialized but the user has not set the
               #   value, it can be assumed to have the default offset.
               INT_offset = ifelse(is.na(input$INT_offset),
                                   default$study$INT_null_offset,
                                   input$INT_offset),
               INT_start_max = ifelse(is.null(input$INT_start_max) || is.na(input$INT_start_max),
                                      default$study$INT_start_max,
                                      input$INT_start_max),
               INT_end_max = ifelse(is.null(input$INT_end_max) || is.na(input$INT_end_max),
                                    default$study$INT_end_max,
                                    input$INT_end_max)
    )
  })
  ## --- Create tabular study ---
  ## Create study specification with intervention start/stop times
  ##   study is a reactive function to cut down on computational workload
  ##   This prevents the study from being recalculated when only visualization
  ##   parameters have changed.
  study <- reactive({
    # --- Exceptions ---
    if(is.na(input$n_COH) | is.null(INT_config())){
      return(NULL)
    }
    # --- Assemble study spec ---
    if(input$advanced_COH > 0 & F){ # Construct separately by cohort, & F to disable for now
      base_study <- data.frame()
    } else {
      base_study <- data.frame(COH = 1:input$n_COH) %>%
        cross_join(INT_config())
    }
    # --- Call construction function ---
    generate_study(base_study)
  })
  
  # Plotting ===================================================================
  viz_options <- reactive({
    # List containing options to send to make_plot()
    option_list = list()
    # Add cohort names if initialized
    if(input$advanced_COH) {
      option_list <- append(option_list,
                            list(COH_names = sapply(
                              1:input$n_COH, 
                              function(i){
                                ifelse(input[[paste0("COH_name_", i)]] == "",
                                       paste0("Cohort ", i),
                                       input[[paste0("COH_name_", i)]])
                              })))
    }
    # Add intervention names
    option_list <- append(option_list,
                          list(INT_names = sapply(
                            1:input$n_INT,
                            function(i){
                              ifelse(input[[paste0("INT_name_", i)]] == "",
                                     paste0("Intervention ", i),
                                     input[[paste0("INT_name_", i)]])
                            }
                          )))
    # Add timing unit
    if(input$time_units!="") {
      option_list <- append(option_list,
                            list(time_units = input$time_units))
    }
    # Return arguments
    option_list
  })
  ## Create plot using the assembled study and plotting options
  output$plot <- renderPlot({
    # Fail states
    if(suppress.plot | is.null(study())){
      return(NULL)
    }
    # Call plotting
    make_plot(study(), viz_options())
  })
  
  # Temp for Testing ===========================================================
  # Returns a text of all input values
  output$input_list <- renderText({
    # Check if this is wanted
    if(!do.input){
      return(NULL)
    }
    input_names = names(reactiveValuesToList(input))
    out_string = ""
    for(name in input_names){
      out_string = paste0(out_string, name, ": ", 
                          paste0(input[[name]]), "\n")
    }
    out_string
  })
  # Returns config passed to study constructor
  output$config <- renderTable({
    # Check if this is wanted
    if(!(do.table)){
      return(NULL)
    }
    INT_config()
  })
  # Returns study construction
  output$study <- renderTable({
    # Check if this is wanted
    if(!(do.table)){
      return(NULL)
    }
    study()
  })
}
