
################################################################################
# New Server
#  For now, just prints out the list provided by the input

server <- function(input, output){
  # Tab Functionality ==========================================================
  ## --- Next ---
  observeEvent(input$tab1_next, {
    goto_tab = "tab2"
    if(input$is_head_to_head){
      goto_tab = "tab1b"
    }
    updateTabsetPanel(inputId = "input_tabs", selected = goto_tab)
  })
  observeEvent(input$tab1a_next, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  observeEvent(input$tab1b_next, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  ## --- Back ---
  observeEvent(input$tab1a_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab1")
  })
  observeEvent(input$tab1b_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab1")
  })
  observeEvent(input$tab2_back, {
    goto_tab = "tab1"
    if(input$is_head_to_head){
      goto_tab = "tab1b"
    }
    updateTabsetPanel(inputId = "input_tabs", selected = goto_tab)
  })
  observeEvent(input$tab2a_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  ## --- Advanced ---
  observeEvent(input$advanced_input, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab1a")
  })
  observeEvent(input$advanced_viz, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2a")
  })
  
  # Dynamic UI Elements ========================================================
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
               value = NA_character_,
               placeholder = paste0("Intervention ", i)
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
