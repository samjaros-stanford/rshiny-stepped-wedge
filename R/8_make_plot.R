################################################################################
# Functions related to the creation of the stepwise plot

# Generate colors --------------------------------------------------------------
generate_colors <- function(n_needed, pallete=default$plot$colors){
  # Fallback is to use Paired color pallete from RColorBrewer
  # It is colorblind friendly and has the most colors
  if(n_needed > length(pallete)){
    return(RColorBrewer::brewer.pal(n_needed, "Paired"))
  }
  # Calculate indices by multiplying the gap between colors by 0 to the number of
  # colors needed minus one. Add one to the end because R uses 1 indexing because
  # it is dumb and bad. For example, 2 colors will be 1 and 6.
  return(pallete[round((0:(n_needed-1))*(length(pallete)/n_needed))+1])
}

# Make plot --------------------------------------------------------------------
# Create stepwise wedge study plot given study specification in long format
#
make_plot <- function(study, args=list()){
  # Plotting options ===========================================================
  # Defaults
  default_args = list(
    COH_names = paste("Cohort", 1:max(study$COH)),
    INT_names = paste("Intervention", 1:max(study$INT)),
    time_units = "Time",
    h2h = FALSE
  )
  # Take input and update defaults
  viz_args = modifyList(default_args, args)

  # Add names and order study data frame =======================================
  # Add INT names, set standard column width, and arrange
  study_named <- study %>%
    left_join(data.frame(INT = as.character(1:length(viz_args$INT_names)),
                         INT_name = viz_args$INT_names),
              by=join_by("INT")) %>%
    mutate(col_min = COH-0.1,
           col_max = COH+0.1) %>%
    arrange(COH, INT_start) %>%
    mutate(is_h2h = F)
  
  # Add h2h INTs, if needed
  if(viz_args$h2h){
    all_INTs <- study_named %>%
      left_join(viz_args$h2h_df, by=join_by(INT)) %>%
      filter(!is.na(h2h_name)) %>%
      mutate(INT_name = h2h_name,
             col_min = COH) %>%
      bind_rows(study_named) %>%
      arrange(COH, INT_start, !is.na(h2h_name))
    
    unique_INTs <- unique(all_INTs$INT_name)

    study_named <- all_INTs %>%
      group_by(COH, INT_start) %>%
      mutate(is_h2h = !is.na(h2h_name)) %>%
      ungroup()
  } else {
    h2h_INTs <- NULL
    unique_INTs <- unique(study_named$INT_name)
  }

  # Create plot ================================================================
  ggplot(mapping=aes(ymin=INT_start, ymax=INT_end, xmin=col_min, xmax=col_max,
                     fill=factor(INT_name))) +
    # Draw full height lines as rectangles
    geom_rect(data=filter(study_named, !is_h2h)) +
    # Draw half lines for head-to-heads
    geom_rect(data=filter(study_named, is_h2h),
              position=position_dodge2(padding=0, reverse=T)) +
    # Scale axes & fill
    scale_fill_manual(name = NULL, 
                      values = generate_colors(length(unique_INTs)),
                      breaks = unique_INTs) +
    scale_y_continuous(viz_args$time_units, breaks=scales::breaks_pretty()) +
    scale_x_continuous("Cohort", 
                       breaks=1:max(study_named$COH),
                       labels = viz_args$COH_names,
                       transform = "reverse") +
    # Draw plot in wrong orientation to use position_dodge2, then flip
    coord_flip() +
    # Theming
    theme_classic() +
    theme(text = element_text(size = 16),
          legend.position = "bottom",
          panel.grid.major.x = element_line(color="grey90", linewidth=0.5))
}
