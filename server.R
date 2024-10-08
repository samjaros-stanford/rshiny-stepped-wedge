################################################################################
# RShiny Server
#   Handles tab navigation functionality
#   Ultimate caller of dynamic UI functions contained in 2_dynamic_ui.R
#   Handles processing of overall and customized intervention timings
#   Ultimate caller of study creation functions contained in 6_generate_study.R
#   Handles processing of visualization customization
#   Ultimate caller of plotting function contained in 8_make_plot.R

server <- function(input, output){
  # Tab Functionality/Observers ==========================================================
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
    # Isolated values will only be recalculated if the number of INT or tab changes
    input$n_INT
    input$input_tabs
    # If h2h button has been initialized, redraw when it is updated
    if(!is.null(input$INT_h2h)){
      input$INT_h2h
    }
    if(is.na(input$n_INT) || input$n_INT<1){
      return(helpText("Number of interventions is invalid"))
    }
    isolate(make_INT_name_ui(input))
  })
  ## --- Intervention Timing ---
  ## Creates a number of duration inputs equal to the number of interventions
  output$INT_timings <- renderUI({
    # Check if it should be drawn at all
    if(is.na(input$n_INT) || input$n_INT<1){
      return(helpText("Number of interventions is invalid"))
    }
    # Isolated values will only be recalculated if the tab or number changes
    input$input_tabs
    input$n_INT
    # Generate intervention timings
    c(list(
        numericInput(
          inputId = "INT_offset",
          label = "Intervention Offset Between Groups",
          value = ifelse(is.null(input$INT_offset),
                         default$study$INT_offset,
                         input$INT_offset))),
      isolate(make_INT_timing_ui(input)))
  })
  ## --- Cohort Customizations ---
  ## Generates the cohort customization tabset
  output$COH_customization <- renderUI({
    # Return help text if the group numbers are invalid
    if(is.na(input$n_COH) || input$n_COH<1){
      return(helpText("Number of groups is invalid"))
    }
    # Tabset should be redrawn if the main tabset or number of INTs change
    input$input_tabs
    input$n_INT
    # Tabset should also be redrawn if a cohort bucket changes
    for(i in 1:input$n_COH){
      input[[paste0("COH_bucket_group_", i)]]
    }
    # Generate a tabset with a dynamic number of tabs
    # Isolated unless one of redraw conditions above are met
    # do.call() required so all tabPanels are arguments, not just a list
    # Starting tab will be retained using 'selected'
    isolate(do.call(
      tabsetPanel,
      append(
        list(
          id = "COH_tabs",
          type = "pills",
          selected = input$COH_tabs
        ),
        lapply(1:input$n_COH,
               make_COH_customization_tab_ui,
               input=input
        )
      )
    ))
  })
  
  # Dynamic Intervention Naming ================================================
  # Intervention naming depends on several factors
  # These reactives will take care of the decisions needed to output the names
  # Returns a list up to the maximum number of interventions allowed
  INT_names <<- reactive({
    sapply(1:max_n_INT,
           function(i){
             # If name input is uninitialized, return default
             if(is.null(input[[paste0("INT_name_", i)]])){
               return(paste0("Intervention ", i))
             }
             # Decide name of initial intervention
             # If input is null or blank, use default value Intervention #
             if(is.na(input[[paste0("INT_name_", i)]]) ||
                input[[paste0("INT_name_", i)]] == ""){
               name <- paste0("Intervention ", i)
             } else {
               name <- input[[paste0("INT_name_", i)]]
             }
             # Decide if h2h needs to be added
             if(!is.null(input$INT_h2h) && input$INT_h2h &&
                !is.null(input[[paste0("INT_", i, "_h2h_name")]]) &&
                !is.na(input[[paste0("INT_", i, "_h2h_name")]]) &&
                input[[paste0("INT_", i, "_h2h_name")]] != ""){
               name <- paste0(name, " vs. ", input[[paste0("INT_", i, "_h2h_name")]])
             }
             return(name)
           })
  })
  # Study Creation =============================================================
  ## --- Create study configuration ---
  ## Assemble inputs into the format needed for study assembly
  study_config <- reactive({
    # --- Exceptions ---
    # If timing tab is uninitialized
    if(is.null(input$INT_offset)){
      return(NULL)
    }
    # Explicit update when the interventions change
    input$n_INT
    input$n_COH
    input$input_tabs
    # If the by cohort tabs are uninitialized, use basic config
    if(is.null(input$COH_tabs)){
      basic_study_config(input)
    } else {
      custom_study_config(input)
    }
  })
  ## --- Create tabular study ---
  ## Create study specification with intervention start/stop times
  ##   study is a reactive function to cut down on computational workload
  ##   This prevents the study from being recalculated when only visualization
  ##   parameters have changed.
  study <- reactive({
    # --- Exceptions ---
    # Stop if intervention number is uninitialized
    # Stop if offset is uninitialized (timing tab uninitialized)
    if(is.na(input$n_COH) || is.na(input$n_INT) || is.null(study_config())){
      return(NULL)
    }
    # --- Call construction function ---
    generate_study(study_config())
  })
  
  # Plotting ===================================================================
  viz_options <- reactive({
    # List containing options to send to make_plot()
    option_list = list()
    ## --- Conditional add ---
    # Add cohort names if initialized
    if(input$advanced_COH) {
      option_list <- append(
        option_list,
        list(COH_names = sapply(
          1:input$n_COH, 
          function(i){
            ifelse(is.null(input[[paste0("COH_name_", i)]]) || 
                     input[[paste0("COH_name_", i)]] == "",
                   paste0("Cohort ", i),
                   input[[paste0("COH_name_", i)]])
      })))
    }
    # Add head to head configuration if selected
    if(!is.null(input$INT_h2h) && input$INT_h2h){
      h2h_df <- data.frame()
      for (i in 1:input$n_INT){
        if(is.null(input[[paste0("INT_", i, "_h2h_name")]]) ||
           input[[paste0("INT_", i, "_h2h_name")]]==""){
          next
        }
        h2h_df <- h2h_df %>%
          bind_rows(data.frame(INT = as.character(i),
                               h2h_name = input[[paste0("INT_", i, "_h2h_name")]]))
      }
      
      # Only actually add to config if there are named INTs
      if(nrow(h2h_df)>0){
        option_list <- append(option_list,
                              list(h2h = TRUE,
                                   h2h_df = h2h_df))
      }
    }
    ## --- Always add ---
    # Add intervention names
    option_list <- append(
      option_list,
      list(INT_names = sapply(1:input$n_INT,
                              function(i){
                                ifelse(is.null(input[[paste0("INT_name_", i)]]) ||
                                         is.na(input[[paste0("INT_name_", i)]]) ||
                                         input[[paste0("INT_name_", i)]]=="",
                                       paste0("Intervention ", i),
                                       input[[paste0("INT_name_", i)]])
                              })))
    # Add timing unit
    if(input$time_units!="") {
      option_list <- append(option_list,
                            list(time_units = input$time_units))
    }
    # Return arguments
    return(option_list)
  })
  ## Create plot using the assembled study and plotting options
  output$plot <- renderPlot({
    # Debugging or if study cannot be made
    if(suppress.plot || is.null(study())){
      return(NULL)
    }
    # Call plotting
    make_plot(study(), viz_options())
  })
  ## Plotting help text
  output$plot_help <- renderText({
    if(!suppress.plot && is.null(study())){
      "Click through the tabs to create a study visualization."
    } else {
      NULL
    }
  })
  
  # Debugging Outputs ===================================================
  # Returns a text of all input values
  output$input_list <- renderText({
    # Check if this is wanted
    if(!do.input){
      return(NULL)
    }
    out_string = ""
    for(name in names(reactiveValuesToList(input))){
      out_string = paste0(out_string, name, ": ", 
                          paste0(input[[name]]), "\n")
    }
    out_string
  })
  # Returns config passed to study constructor
  output$config <- renderTable({
    # Check if this is wanted or if study cannot be initialized
    if(!(do.table)){
      return(NULL)
    }
    study_config()
  })
  # Returns study construction
  output$study <- renderTable({
    # Check if this is wanted or if study cannot be initialized
    if(!(do.table)){
      return(NULL)
    }
    study()
  })
}
