################################################################################
# UI Elements that need to be declared with the server for compatability with
#   apendTab/prependTab
### Cohort Setup Tab -----------------------------------------------------------
### Contains another tabset, 1 tab for each cohort
cohort_tab <- tabPanel(
  title = "Cohorts",
  value = "tab_cohort",
  helpText("Customize the interventions for each cohort."),
  uiOutput("cohort_customization")
)


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
  observeEvent(input$advanced_cohort, {
    if(input$advanced_cohort > 1){
      updateTabsetPanel(inputId = "input_tabs", selected = "tab_cohort")
    } else {
      appendTab(inputId = "input_tabs", cohort_tab, select = T)
    }
  })
  
  # Dynamic UI Elements ========================================================
  ## --- Intervention Naming ---
  ## Creates a number of text inputs equal to the number of interventions
  output$intervention_names <- renderUI({
    n_int = as.integer(input$n_interventions)
    if(is.na(n_int) | n_int<1){
      return(helpText("Number of interventions is invalid"))
    }
    lapply(1:as.integer(input$n_interventions),
           function(i) {
             textInput(
               inputId = paste0("intervention_name_", i),
               label = paste0("Name of intervention ", i),
               # Use the existing input as the default value
               value = input[[paste0("intervention_name_",i)]]
             )
           })
  })
  
  ## --- Intervention Timing ---
  ## Creates a number of duration inputs equal to the number of interventions
  output$intervention_timings <- renderUI({
    n_int = as.integer(input$n_interventions)
    if(is.na(n_int) | n_int<1){
      return(helpText("Number of interventions is invalid"))
    }
    lapply(1:n_int,
           function(i) {
             # If the intervention names are blank, fill in placeholder names
             this_name = ifelse(input[[paste0("intervention_name_",i)]]=="",
                                paste0("Intervention ",i),
                                input[[paste0("intervention_name_",i)]])
             numericInput(
               inputId = paste0("intervention_length_", i),
               label = this_name,
               # Use the existing input as the default
               value = ifelse(is.na(input[[paste0("intervention_length_",i)]]),
                              NA_integer_,
                              input[[paste0("intervention_length_",i)]])
             )
           })
  })
  
  ## --- Cohort Customizations ---
  ## Generates the cohort customization tabset
  output$cohort_customization <- renderUI({
    n_groups = as.integer(input$n_groups)
    # Return help text if the group numbers are invalid
    if(is.na(n_groups) | n_groups<1){
      return(helpText("Number of groups is invalid"))
    }
    # Generate a tabset with a dynamic number of tabs
    # tabsetPanel() needs tabPanels as the inputs, not a list
    # Workaround is using do.call() with all of the arguments in a list
    do.call(
      tabsetPanel,
      append(
        list(
          id = "cohort_tabs",
          selected = "cohort1",
          type = "pills"
        ),
        lapply(1:n_groups,
               function(i){
                 tabPanel(
                   title = i,
                   value = paste0("cohort",i),
                   textInput(
                     inputId = paste0("cohort_name_", i),
                     label = paste0("Name of cohort ", i),
                     # Use the existing input as the default value
                     value = input[[paste0("cohort_name_",i)]]
                   )
                 )
               }
        )
      )
    )
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
}
