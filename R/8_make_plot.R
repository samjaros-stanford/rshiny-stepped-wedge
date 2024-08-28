################################################################################
#
# Make plot
# Create stepwise wedge study plot given study specification in long format
#
make_plot <- function(study, args=list()){
  # Default plotting options
  default_args = list(
    COH_names = paste("Cohort", 1:max(study$COH)),
    INT_names = paste("Intervention", 1:max(study$INT)),
    time_units = "Time",
    h2h = FALSE
  )
  
  # Take input and update defaults
  viz_args = modifyList(default_args, args)
  
  # Add INT names into study (in order)
  study_named <- study %>%
    left_join(data.frame(INT = as.character(1:max(study$INT)),
                         INT_name = viz_args$INT_names),
              by=join_by("INT")) %>%
    arrange(COH, INT_start) %>%
    mutate(is_h2h = F)
  
  # Get h2h segments
  if(viz_args$h2h){
    all_INTs <- study_named %>%
      left_join(viz_args$h2h_df, by=join_by(INT)) %>%
      filter(!is.na(h2h_name)) %>%
      mutate(INT_name = h2h_name) %>%
      bind_rows(study_named) %>%
      arrange(COH, INT_start, !is.na(h2h_name))
    
    unique_INTs <- unique(all_INTs$INT_name)

    study_named <- all_INTs %>%
      group_by(COH, INT_start) %>%
      mutate(is_h2h = n()>1) %>%
      ungroup()
  } else {
    h2h_INTs <- NULL
    unique_INTs <- unique(study_named$INT_name)
  }

  # Create plot
  ggplot(mapping=aes(x=INT_start, xend=INT_end, y=factor(COH), group=factor(COH),
                     color=factor(INT_name))) +
    # TODO: Switch to rectangles to allow for more control over drawing?
    # Draw full lines
    geom_segment(data=filter(study_named, !is_h2h), linewidth=12, key_glyph = "rect") +
    # Draw half lines for head-to-heads
    geom_segment(data=filter(study_named, is_h2h), linewidth=6, 
                 position = "dodge", 
                 key_glyph = "rect") +
    scale_color_brewer(name = NULL, 
                       palette = "Set1",
                       breaks = unique_INTs) +
    scale_x_continuous(viz_args$time_units, breaks=scales::breaks_pretty()) +
    scale_y_discrete("Cohort", 
                     breaks = 1:length(viz_args$COH_names), 
                     labels = viz_args$COH_names,
                     limits = rev) +
    theme_classic() +
    theme(text = element_text(size = 16),
          legend.position = "bottom",
          panel.grid.major.x = element_line(color="grey90", linewidth=0.5))
}
