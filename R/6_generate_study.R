################################################################################
# Functions required to process input into study specifications and overall
#   study. Functions should remain isolated so they are not called until needed.

# Basic study specifications ===================================================
# If the advanced cohort tab has not been accessed, all cohorts have the same
#   configuration for each intervention
basic_study_config <- function(input){
  cross_join(data.frame(COH = 1:input$n_COH),
             data.frame(INT = 1:input$n_INT,
               # Need to iterate through all intervention lengths
               INT_length = sapply(
                 1:input$n_INT, 
                 function(i){
                   input[[paste0("INT_length_",i)]]
                 }),
               # Need to iterate through all intervention gaps
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
               # If the maxes are uninitialized or blank, use the default
               INT_start_max = ifelse(is.null(input$INT_start_max) || is.na(input$INT_start_max),
                                      default$study$INT_start_max,
                                      input$INT_start_max),
               INT_end_max = ifelse(is.null(input$INT_end_max) || is.na(input$INT_end_max),
                                    default$study$INT_end_max,
                                    input$INT_end_max)
    )
  )
}

# Custom study specifications ==================================================
# If the advanced cohort tab has been accessed, check the cohort inputs for new
#   values. Otherwise use the values from the main intervention input
custom_study_config <- function(input){
  config <- data.frame()
  # Sometimes, the needed UI hasn't been loaded. If so, return null and wait
  #   for the UI to be loaded. Uses an arbitrary argument to check
  for(i in 1:input$n_COH){
    if(is.null(input[[paste0("COH_INT_incl_order_", i)]])){
      return(NULL)
    }
    config <- bind_rows(
      config,
      data.frame(COH = i,
                 INT = input[[paste0("COH_INT_incl_order_", i)]]))
  }
  config <- config %>%
    rowwise() %>%
    mutate(INT_length = 
             if_else(is.na(input[[paste0("INT_length_",INT,"_COH_",COH)]]),
                     input[[paste0("INT_length_",INT)]],
                     input[[paste0("INT_length_",INT,"_COH_",COH)]]),
           INT_gap = 
             if_else(is.na(input[[paste0("INT_gap_",INT,"_COH_",COH)]]),
                     input[[paste0("INT_gap_",INT)]],
                     input[[paste0("INT_gap_",INT,"_COH_",COH)]]),
           INT_offset = input$INT_offset,
           INT_start_max = 
             if_else(is.na(input[[paste0("INT_start_max_COH_",COH)]]),
                     if_else(is.na(input$INT_start_max),
                             default$study$INT_start_max,
                             input$INT_start_max),
                     input[[paste0("INT_start_max_COH_",COH)]]),
           INT_end_max = 
             if_else(is.na(input[[paste0("INT_end_max_COH_",COH)]]),
                     if_else(is.na(input$INT_end_max),
                             default$study$INT_end_max,
                             input$INT_end_max),
                     input[[paste0("INT_end_max_COH_",COH)]])) %>%
    ungroup()
  return(config)
}

# Generate study ===============================================================
# Generate a stepwise wedge study given study configuration
# base_study - data frame with study cohort configuration
#   Required columns:
#     COH - Cohort ID
#     INT - Intervention ID
#     INT_length - Intervention length
#     INT_gap - Gap before this row's intervention
#     INT_offset - Time units by which study should be stepped
#     INT_start_max - Maximum length for all first interventions
#     INT_end_max - Maximum length for all last interventions
generate_study <- function(base_study){
  #-----------------------------------------------------------------------------
  # Input checks
  if(is.null(base_study)){
    return(NULL)
  }
  # Check for needed columns
  if(!all(c("COH","INT","INT_length","INT_gap","INT_offset","INT_start_max","INT_end_max") %in%
          colnames(base_study))){
    return(NULL)
  }
  #-----------------------------------------------------------------------------
  # Calculate the first intervention separately
  #   Allows for different starting interventions (ex. Group 2 starts with 
  #   intervention #3)
  not_last_INT <- base_study %>%
    # Calculate start
    group_by(COH) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(INT_end = INT_gap + INT_length + (cumsum(INT_offset) - 1),
           INT_start = if_else(INT_end - INT_start_max < 0,
                               INT_gap,
                               INT_end - INT_start_max + INT_gap)) %>%
    select(COH, INT, INT_length, INT_gap, INT_start, INT_end) %>%
    # Calculate middle interventions
    bind_rows(base_study %>%
                group_by(COH) %>%
                filter(row_number()!=1 & row_number()!=n()) %>%
                ungroup()) %>%
    arrange(COH)
  
  for(i in 1:nrow(not_last_INT)){
    # If INT_start has already been figured out, we're good
    # This also takes care of when i=1
    if(!is.na(not_last_INT[i, "INT_start"])){
      next
    }

    not_last_INT[i, "INT_start"] =
      not_last_INT[i-1, "INT_end"] + not_last_INT[i, "INT_gap"]

    not_last_INT[i, "INT_end"] =
      not_last_INT[i, "INT_start"] + not_last_INT[i, "INT_length"]
  }
  
  last_INT <- base_study %>%
    group_by(COH) %>%
    # If there's only 1 intervention for a cohort, it has already been figured out
    filter(row_number()!=1 & row_number()==n()) %>%
    ungroup() %>%
    # To determine the study end time, we need to add the maximum time so far
    #   plus the gap and minimum length of this intervention for each cohort
    #   After getting for each cohort, add in gaps and lengths (in case they're
    #   different per cohort) and get the maximum
    left_join(not_last_INT %>%
                group_by(COH) %>%
                summarize(prev_INT_end = max(INT_end)),
              by="COH") %>%
    mutate(study_end = max(prev_INT_end + INT_gap + INT_length)) %>%
    # Get start and end times where the end time is the smaller of study_end or 
    #   the max length of the intervention
    rowwise() %>%
    mutate(INT_start = INT_gap + prev_INT_end,
           INT_end = suppressWarnings(
             min(study_end, INT_start + INT_end_max))) %>%
    ungroup() %>%
    select(COH, INT, INT_start, INT_end)

  rbind(not_last_INT %>%
          select(COH, INT, INT_start, INT_end), 
        last_INT)
}

  