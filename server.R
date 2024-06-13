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
  ## --- Intervention Naming ---
  ## Creates a number of text inputs equal to the number of interventions
  output$INT_names <- renderUI({
    # Isolated values will only be recalculated if the tab changes
    input$input_tabs
    n_int = as.integer(isolate(input$n_INT))
    if(is.na(n_int) | n_int<1){
      return(helpText("Number of interventions is invalid"))
    }
    isolate(make_INT_name_ui(n_int, input))
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
    c(isolate(make_INT_timing_ui(n_int, input)),
      list(
        hr(),
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
    n_COH = as.integer(input$n_COH)
    # Return help text if the group numbers are invalid
    if(is.na(n_COH) | n_COH<1){
      return(helpText("Number of groups is invalid"))
    }
    # Generate a tabset with a dynamic number of tabs
    # tabsetPanel() needs tabPanels as the inputs, not a list
    # Workaround is using do.call() with all of the arguments in a list
    do.call(
      tabsetPanel,
      append(
        list(
          id = "COH_tabs",
          selected = "COH_1",
          type = "pills"
        ),
        lapply(1:n_COH,
               function(i){
                 tabPanel(
                   title = i,
                   value = paste0("COH_",i),
                   textInput(
                     inputId = paste0("COH_name_", i),
                     label = paste0("Name of cohort ", i),
                     # Use the existing input as the default value
                     value = input[[paste0("COH_name_",i)]]
                   )
                 )
               }
        )
      )
    )
  })
  
  # Study Creation =============================================================
  # Dynamically observe intervention lengths based on the number of
  #   intervention lengths, assigning them a value of 0 if uninitialized
  INT_config <- reactive({
    data.frame(INT = 1:input$n_INT,
               INT_length = sapply(1:input$n_INT, function(i){
                 len <- input[[paste0("INT_length_",i)]]
                 if(is.null(len)){
                   0
                 } else {
                   len
                 }}),
               INT_gap = sapply(1:input$n_INT, function(i){
                 gap <- input[[paste0("INT_gap_",i)]]
                 if(is.null(gap)){
                   0
                 } else {
                   gap
                 }}),
               # Offset has special case where if it is completely uninitialized,
               #   it needs to be 0/NA so that a phantom study is not created.
               #   If it has been initialized but the user has not set the
               #   value, it can be assumed to have the default offset.
               INT_offset = ifelse(is.null(input$INT_offset),
                                   default$study$INT_null_offset,
                                   ifelse(is.na(input$INT_offset),
                                          default$study$INT_offset,
                                          input$INT_offset)),
               INT_start_max = ifelse(is.null(input$INT_start_max) || is.na(input$INT_start_max),
                                      default$study$INT_start_max,
                                      input$INT_start_max),
               INT_end_max = ifelse(is.null(input$INT_end_max) || is.na(input$INT_end_max),
                                    default$study$INT_end_max,
                                    input$INT_end_max)
    )
  })
  # Create study specification with intervention start/stop times
  #   study is a reactive function to cut down on computational workload
  #   This prevents the study from being recalculated when only visualization
  #   parameters have changed.
  study <- reactive({
    # --- Exceptions ---
    if(is.na(input$n_INT) | is.na(input$n_COH)){
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
  output$plot <- renderPlot({
    # Fail states
    if(!do.plot | is.na(input$n_INT) | is.na(input$n_COH)){
      return(NULL)
    }
    # Call plotting
    make_plot(study())
  })
  # Temp for Testing ===========================================================
  # Returns a text of all input values
  output$input_list <- renderText({
    input_names = names(reactiveValuesToList(input))
    out_string = ""
    for(name in input_names){
      out_string = paste0(out_string, name, ": ", input[[name]], "\n")
    }
    out_string
  })
  # Returns study construction
  output$study <- renderTable(study())
}
