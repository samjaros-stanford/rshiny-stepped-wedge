################################################################################
# UI Elements or functions that need to be declared in advance for use in 
#   server.R

# Create intervention names for naming tab -------------------------------------
# Make a name slot for each intervention
# Default value in the box is the current value if initialized, otherwise blank
# Blank name values need to be handled by other functions
make_INT_name_ui <- function(input){
  c(lapply(
    1:input$n_INT,
    function(i){
      row <- list(
        tags$u(strong(paste0("Name of intervention ", i))),
        textInput(
          inputId = paste0("INT_name_", i),
          label = NULL,
          value = ifelse(is.null(input[[paste0("INT_name_", i)]]),
                         "",
                         input[[paste0("INT_name_", i)]])))
      if(!is.null(input$INT_h2h) && input$INT_h2h){
        row <- append(
          row, 
          list(textInput(
            inputId = paste0("INT_", i, "_h2h_name"),
            label = "Versus",
            value = ifelse(is.null(input[[paste0("INT_", i, "_h2h_name")]]),
                           "",
                           input[[paste0("INT_", i, "_h2h_name")]]))))
      }
      return(row)
    }),
    list(checkboxInput(
      inputId = "INT_h2h",
      label = "Allow Head-to-Head Interventions?",
      value = ifelse(is.null(input$INT_h2h),
                     FALSE,
                     input$INT_h2h)))
  )
}

# Create intervention timing tabs -----------------------------------------------
# Create input widgets with their names from input reactive
make_INT_timing_ui <- function(input){
  INT_timing_ui <- list(
    # Heading for first intervention
    tags$u(strong(INT_names()[1])),
    # First intervention specs require gap, minimum, and maximum
    fluidRow(
      column(width = 4,
             numericInput(
               inputId = "INT_gap_1",
               label = "Delay Before",
               value = ifelse(is.null(input$INT_gap_1),
                              default$study$INT_gap,
                              input$INT_gap_1))),
      column(width = 4,
             numericInput(
               inputId = "INT_length_1",
               label = "Min Duration",
               value = ifelse(is.null(input$INT_length_1),
                              default$study$INT_length,
                              input$INT_length_1))),
      column(width = 4,
             numericInput(
               inputId = "INT_start_max",
               label = "Max Duration",
               value = ifelse(is.null(input$INT_start_max),
                              default$study$INT_start_max,
                              input$INT_start_max)))))
  
  # If there's more than two interventions, we need to add "middle"
  #   interventions that have a definite length
  if(input$n_INT>2){
    INT_timing_ui <- c(
      INT_timing_ui,
      sapply(
        2:(input$n_INT - 1),
        function(i){
          list(
            tags$u(strong(INT_names()[i])),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  inputId = paste0("INT_gap_", i),
                  label = "Delay Before",
                  value = ifelse(is.null(input[[paste0("INT_gap_", i)]]),
                                 default$study$INT_gap,
                                 input[[paste0("INT_gap_", i)]])
                )),
              column(
                width = 6,
                numericInput(
                  inputId = paste0("INT_length_", i),
                  label = "Duration",
                  value = ifelse(is.null(input[[paste0("INT_length_", i)]]),
                                 default$study$INT_length,
                                 input[[paste0("INT_length_", i)]])))))}))}
  
  # If there's only 1 intervention, it was already added by the first step
  if(input$n_INT>1){
    INT_timing_ui <- c(
      INT_timing_ui,
      list(tags$u(strong(INT_names()[input$n_INT])),
           fluidRow(
             column(
               width = 4,
               numericInput(
                 inputId = paste0("INT_gap_", input$n_INT),
                 label = "Delay Before",
                 value = ifelse(is.null(input[[paste0("INT_gap_", input$n_INT)]]),
                                default$study$INT_gap,
                                input[[paste0("INT_gap_", input$n_INT)]]))),
             column(
               width = 4,
               numericInput(
                 inputId = paste0("INT_length_", input$n_INT),
                 label = "Min Duration",
                 value = ifelse(is.null(input[[paste0("INT_length_", input$n_INT)]]),
                                default$study$INT_length,
                                input[[paste0("INT_length_", input$n_INT)]]))),
             column(
               width = 4,
               numericInput(
                 inputId = "INT_end_max",
                 label = "Max Duration",
                 value = ifelse(is.null(input$INT_end_max),
                                default$study$INT_end_max,
                                input$INT_end_max))))))}
  
  return(INT_timing_ui)
}

# Create input widgets for custom timing by cohort from input reactive
#   Default values should be NA/blank which will be handled by later functions
make_INT_timing_by_COH_ui <- function(COH_id, input){
  # NOTE: Goal is to eventually use semanticUI, which can handle numeric placeholders
  #       however, this will be implemented in a future change
  
  # Get interventions/order 
  INT_order <- input[[paste0("COH_INT_incl_order_", COH_id)]]
  
  # If no interventions, no timing UI
  if(length(INT_order) < 1){
    return(NULL)
  }
  
  # Start with first intervention
  i <- INT_order[1]
  INT_timing_ui <- list(
    # Heading for first intervention
    tags$u(strong(INT_names()[1])),
    # First intervention specs require gap, minimum, and maximum
    fluidRow(
      column(width = 4,
             numericInput(
               inputId = paste0("INT_gap_", i, "_COH_", COH_id),
               label = "Delay Before",
               value = ifelse(is.na(input[[paste0("INT_gap_", i, "_COH_", COH_id)]]),
                              NA,
                              input[[paste0("INT_gap_", i, "_COH_", COH_id)]]))),
      column(width = 4,
             numericInput(
               inputId = paste0("INT_length_", i, "_COH_", COH_id),
               label = "Min Duration",
               value = ifelse(is.na(input[[paste0("INT_length_", i, "_COH_", COH_id)]]),
                              NA,
                              input[[paste0("INT_length_", i, "_COH_", COH_id)]]))),
      column(width = 4,
             numericInput(
               inputId = paste0("INT_start_max_COH_", COH_id),
               label = "Max Duration",
               value = ifelse(is.na(input[[paste0("INT_start_max_COH_", COH_id)]]),
                              NA,
                              input[[paste0("INT_start_max_COH_", COH_id)]])))))
  
  # If there's more than two interventions, we need to add "middle"
  #   interventions that have a definite length
  if(length(INT_order) > 2){
    middle_INTs <- INT_order[2:(length(INT_order)-1)]

    INT_timing_ui <- c(
      INT_timing_ui,
      sapply(
        middle_INTs,
        function(i){
          list(
            tags$u(strong(INT_names()[as.numeric(i)])),
            fluidRow(
              column(width = 6,
                     numericInput(
                       inputId = paste0("INT_gap_", i, "_COH_", COH_id),
                       label = "Delay Before",
                       value = ifelse(is.na(input[[paste0("INT_gap_", i, "_COH_", COH_id)]]),
                                      NA,
                                      input[[paste0("INT_gap_", i, "_COH_", COH_id)]]))),
              column(
                width = 6,
                numericInput(
                  inputId = paste0("INT_length_", i, "_COH_", COH_id),
                  label = "Duration",
                  value = ifelse(is.na(input[[paste0("INT_length_", i, "_COH_", COH_id)]]),
                                 NA,
                                 input[[paste0("INT_length_", i, "_COH_", COH_id)]])))))}))}
  
  # If there's only 1 intervention, it was already added by the first step
  if(length(INT_order)>1){
    i <- INT_order[length(INT_order)]
    INT_timing_ui <- c(
      INT_timing_ui,
      list(
        tags$u(strong(INT_names()[as.numeric(i)])),
        # First intervention specs require gap, minimum, and maximum
        fluidRow(
          column(width = 4,
                 numericInput(
                   inputId = paste0("INT_gap_", i, "_COH_", COH_id),
                   label = "Delay Before",
                   value = ifelse(is.na(input[[paste0("INT_gap_", i, "_COH_", COH_id)]]),
                                  NA,
                                  input[[paste0("INT_gap_", i, "_COH_", COH_id)]]))),
          column(width = 4,
                 numericInput(
                   inputId = paste0("INT_length_", i, "_COH_", COH_id),
                   label = "Min Duration",
                   value = ifelse(is.na(input[[paste0("INT_length_", i, "_COH_", COH_id)]]),
                                  NA,
                                  input[[paste0("INT_length_", i, "_COH_", COH_id)]]))),
          column(width = 4,
                 numericInput(
                   inputId = paste0("INT_end_max_COH_", COH_id),
                   label = "Max Duration",
                   value = ifelse(is.na(input[[paste0("INT_end_max_COH_", COH_id)]]),
                                  NA,
                                  input[[paste0("INT_end_max_COH_", COH_id)]]))))))}
  
  return(INT_timing_ui)
}

# Create cohort customization tabs ---------------------------------------------
make_COH_customization_tab_ui <- function(COH_id, input){
  # Setup bucket_list ==========================================================
  # First initialization just uses all interventions as included
  if(is.null(input[[paste0("COH_bucket_group_", COH_id)]])){
    incl_INT_names <- INT_names()[1:input$n_INT]
    names(incl_INT_names) <- 1:input$n_INT
    excl_INT_names <- NULL
  } else {
  # All subsequent runs need to check to see if the intervention number has
  #   changed to see if the interventions are still eligible and if interventions
  #   need to be added
    # --- Excluded interventions ---
    existing_excl_INT <- input[[paste0("COH_INT_excl_order_", COH_id)]]
    excl_INT <- existing_excl_INT[existing_excl_INT %in% 1:input$n_INT]
    
    # Get names in format required by bucket
    excl_INT_names <- INT_names()[as.numeric(excl_INT)]
    names(excl_INT_names) <- excl_INT
    # --- Included interventions ---
    existing_incl_INT <- input[[paste0("COH_INT_incl_order_", COH_id)]]
    incl_INT <- existing_incl_INT[existing_incl_INT %in% 1:input$n_INT]
    
    # Add missing interventions to the end of included
    missing_INTs <- (1:input$n_INT)[!(1:input$n_INT %in% c(excl_INT, incl_INT))] 
    incl_INT <- c(incl_INT, missing_INTs)
    
    # Get names in format required by bucket
    incl_INT_names <- INT_names()[as.numeric(incl_INT)]
    names(incl_INT_names) <- incl_INT
  }

  # Create tabPanel
  tabPanel(
    title = COH_id,
    value = paste0("COH_",COH_id),
    # Cohort name
    textInput(
      inputId = paste0("COH_name_", COH_id),
      label = paste0("Name of cohort ", COH_id),
      # Use the existing input as the default value
      value = input[[paste0("COH_name_", COH_id)]]
    ),
    # Intervention order
    bucket_list(
      header = "Drag the interventions to reorder or remove",
      group_name = paste0("COH_bucket_group_", COH_id),
      orientation = "vertical",
      add_rank_list(
        text = NULL,
        labels = incl_INT_names,
        input_id = paste0("COH_INT_incl_order_", COH_id)
      ),
      add_rank_list(
        text = "Excluded from this cohort",
        labels = excl_INT_names,
        input_id = paste0("COH_INT_excl_order_", COH_id)
      )
    ),
    make_INT_timing_by_COH_ui(COH_id, input)
  )
}
