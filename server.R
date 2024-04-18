################################################################################
# UI Elements that need to be declared with the server

### Cohort Setup Tab -----------------------------------------------------------
### Contains another tabset, 1 tab for each cohort
cohort_tab <- tabPanel(
  title = "Cohorts",
  value = "tab_cohort",
  helpText("Customize the interventions for each cohort."),
  tabsetPanel(
    tabPanel("C1",value="ctab_1",helpText("C1")),
    tabPanel("C2",value="ctab_2",helpText("C2"))
  )
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
    lapply(1:as.integer(input$n_interventions),
           function(i) {
             this_name = ifelse(input[[paste0("intervention_name_",i)]]=="",
                                paste0("Intervention ",i),
                                input[[paste0("intervention_name_",i)]])
             numericInput(
               inputId = paste0("intervention_length_", i),
               label = this_name,
               value = ifelse(is.na(input[[paste0("intervention_length_",i)]]),
                              NA_integer_,
                              input[[paste0("intervention_length_",i)]])
             )
           })
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
