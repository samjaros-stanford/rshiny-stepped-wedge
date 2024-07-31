################################################################################
# UI Elements or functions that need to be declared in advance for use in 
#   server.R

# Create intervention names for naming tab -------------------------------------
# Make a name slot for each intervention
# Default value in the box is the current value if initialized, otherwise blank
# Blank name values need to be handled by other functions
make_INT_name_ui <- function(input){
  lapply(1:input$n_INT,
         function(i){
           textInput(
             inputId = paste0("INT_name_", i),
             label = paste0("Name of intervention ", i),
             value = ifelse(is.null(input[[paste0("INT_name_", i)]]),
                            "",
                            input[[paste0("INT_name_", i)]]))
         })
}

# Create intervention timing tab -----------------------------------------------
# Combine existing input widgets with their names from input reactive
make_INT_timing_ui <- function(input){
  c(list(
    # Heading for first intervention
    tags$u(strong(ifelse(input$INT_name_1 == "",
                         paste0("Intervention 1"),
                         input$INT_name_1))),
    # First intervention specs require gap, minimum, and maximum
    fluidRow(
      column(width = 4,
             numericInput(
               inputId = "INT_gap_1",
               label = "Delay Before",
               value = ifelse(is.null(input$INT_gap_1),
                              default$study$INT_gap,
                              input$INT_gap_1)
             )),
      column(width = 4,
             numericInput(
               inputId = "INT_length_1",
               label = "Min Duration",
               value = ifelse(is.null(input$INT_length_1),
                              default$study$INT_length,
                              input$INT_length_1)
             )),
      column(width = 4,
             numericInput(
               inputId = "INT_start_max",
               label = "Max Duration",
               value = ifelse(is.null(input$INT_start_max),
                              default$study$INT_start_max,
                              input$INT_start_max)
             ))
    )),
    sapply(
      2:(input$n_INT - 1),
      function(i){
        list(
          tags$u(strong(ifelse(input[[paste0("INT_name_", i)]] == "",
                               paste0("Intervention ", i),
                               input[[paste0("INT_name_", i)]]))),
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
                               input[[paste0("INT_length_", i)]])
              ))
          )
        )
      }),
    list(tags$u(strong(ifelse(input[[paste0("INT_name_", input$n_INT)]] == "",
                              paste0("Intervention ", input$n_INT),
                              input[[paste0("INT_name_", input$n_INT)]]))),
         fluidRow(
           column(
             width = 4,
             numericInput(
               inputId = paste0("INT_gap_", input$n_INT),
               label = "Delay Before",
               value = ifelse(is.null(input[[paste0("INT_gap_", input$n_INT)]]),
                              default$study$INT_gap,
                              input[[paste0("INT_gap_", input$n_INT)]])
             )),
           column(
             width = 4,
             numericInput(
               inputId = paste0("INT_length_", input$n_INT),
               label = "Min Duration",
               value = ifelse(is.null(input[[paste0("INT_length_", input$n_INT)]]),
                              default$study$INT_length,
                              input[[paste0("INT_length_", input$n_INT)]])
             )),
           column(
             width = 4,
             numericInput(
               inputId = "INT_end_max",
               label = "Max Duration",
               value = ifelse(is.null(input$INT_end_max),
                              default$study$INT_end_max,
                              input$INT_end_max)))
           ))
    )
}

# Create cohort customization tabs ---------------------------------------------
make_COH_customization_tab_ui <- function(COH_id, input){
  tabPanel(
    title = COH_id,
    value = paste0("COH_",COH_id),
    textInput(
      inputId = paste0("COH_name_", COH_id),
      label = paste0("Name of cohort ", COH_id),
      # Use the existing input as the default value
      value = input[[paste0("COH_name_", COH_id)]]
    )
  )
}
