################################################################################
# Generate study
# 
# Generate a stepwise wedge study given study configuration
# 
# n_groups The number of participant groups in the study. An integer.
# n_i The number of interventions in the study. An integer.
# 
generate_study <- function(n_groups, n_i, i_lens, i_offset, i_gaps, 
                           start_max, end_max){
  #-----------------------------------------------------------------------------
  # Input checks
  
  #-----------------------------------------------------------------------------
  # Could differ by cohort
  first_length = i_lens[1]
  offsets = c(first_length, rep(i_offset, n_groups-1))
  
  # Get base dataframe where it's 1 row per group/intervention combo and the
  #   details needed to calculate the start/stop times for each intervention
  #   NOTE: Study/cohort customizations should be incorporated here
  base_study <- data.frame(group = 1:n_groups) %>%
    cross_join(data.frame(interv = 1:n_i, 
                          interv_length = i_lens,
                          gap_before = c(0, i_gaps)))
  
  # Calculate the first intervention separately
  #   Allows for different starting interventions (ex. Group 2 starts with 
  #   intervention #3)
  first_interv <- base_study %>%
    arrange(interv, group) %>%
    group_by(group) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(interv_offset = offsets,
           interv_end = cumsum(interv_offset)) %>%
    mutate(interv_max = start_max,
           interv_start = if_else(interv_end - interv_max < 0,
                                  0,
                                  interv_end - interv_max)) %>%
    select(group, interv, interv_length, interv_max, gap_before, 
           interv_start, interv_end)
            
  not_last_interv <- first_interv %>%
    bind_rows(base_study %>%
                arrange(interv, group) %>%
                group_by(group) %>%
                filter(row_number()!=1 & row_number()!=n()) %>%
                ungroup()) %>%
    arrange(group, interv) %>%
    mutate(interv_start = if_else(is.na(interv_start),
                                  lag(interv_end) + gap_before,
                                  interv_start),
           interv_end = if_else(is.na(interv_end),
                                interv_start + interv_length,
                                interv_end)) %>%
    select(group, interv, interv_start, interv_end)
  
  last_interv <- base_study %>%
    arrange(interv, group) %>%
    group_by(group) %>%
    # If there's only 1 intervention for a cohort, it has already been figured out
    filter(row_number()!=1 & row_number()==n()) %>%
    ungroup() %>%
    # To determine the study end time, we need to add the maximum time so far
    #   plus the gap and minimum length of this intervention for each cohort
    left_join(not_last_interv %>%
                group_by(group) %>%
                summarize(prev_interv_end = max(interv_end)),
              by="group") %>%
    mutate(study_end = suppressWarnings(
      max(prev_interv_end + gap_before + interv_length))) %>%
    # Get start and end times where the end time is the smaller of study_end or 
    #   the max length of the intervention
    mutate(interv_start = gap_before + prev_interv_end,
           interv_max = end_max,
           interv_end = suppressWarnings(
             min(study_end, interv_start + end_max))) %>%
    select(group, interv, interv_start, interv_end)
  
  rbind(not_last_interv, last_interv) %>%
    arrange(group, interv)
}

#===============================================================================
# Testing
generate_study(n_groups = 4,
               n_i = 1,
               i_lens = c(3),
               i_offset = 2,
               start_max = Inf,
               end_max = Inf,
               i_gaps = c())
  