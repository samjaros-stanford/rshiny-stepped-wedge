################################################################################
# UI Elements that need to be declared in advance for use in server.R
# Create intervention timing tab -----------------------------------------------
make_INT_timing_ui <- function(INT_id, input, COH_id=NULL){
  COH_prefix <- ifelse(is.null(COH_id), "", paste0("COH_", COH_id, "_"))
  
  fluidRow(
    column(width=6,
           numericInput(
             inputId = paste0(COH_prefix, "INT_gap_", INT_id),
             label = "Gap Before",
             # Use the existing input as the default
             value = ifelse(is.na(input[[paste0(COH_prefix,"INT_gap_", INT_id)]]),
                            default$study$INT_gap,
                            input[[paste0(COH_prefix,"INT_gap_", INT_id)]])
           )),
    column(width=6,
           numericInput(
             inputId = paste0(COH_prefix, "INT_length_", INT_id),
             label = "Min Length",
             # Use the existing input as the default
             value = ifelse(is.na(input[[paste0(COH_prefix,"INT_length_", INT_id)]]),
                            default$study$INT_length,
                            input[[paste0(COH_prefix,"INT_length_", INT_id)]])
           ))
  )
}


