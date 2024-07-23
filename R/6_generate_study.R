################################################################################
# Generate study
# 
# Generate a stepwise wedge study given study configuration
# 
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
  
  #-----------------------------------------------------------------------------
  # Calculate the first intervention separately
  #   Allows for different starting interventions (ex. Group 2 starts with 
  #   intervention #3)
  not_last_INT <- base_study %>%
    # Calculate start
    arrange(INT, COH) %>%
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
                arrange(INT, COH) %>%
                group_by(COH) %>%
                filter(row_number()!=1 & row_number()!=n()) %>%
                ungroup()) %>%
    arrange(COH, INT)
  
  # How to do this without for loop???
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
    arrange(COH, INT) %>%
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
        last_INT) %>%
    arrange(COH, INT)
}

#===============================================================================
# Testing

# base_study <- data.frame(COH = 1:3) %>%
#   cross_join(data.frame(INT = 1:5,
#                         INT_length = 1,
#                         INT_offset = 2,
#                         INT_gap = 0,
#                         INT_start_max = 2,
#                         INT_end_max = 2))
# 
# generate_study(base_study=base_study)
  