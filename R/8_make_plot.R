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
    time_units = "Time"
  )
  
  # Take input and update defaults
  viz_args = modifyList(default_args, args)
  
  # Create plot
  ggplot(data = study, aes(y=factor(COH), color=factor(INT))) +
    geom_segment(aes(x=INT_start, xend=INT_end), linewidth=12) +
    scale_color_brewer(name=NULL, 
                       palette="Set1",
                       breaks = 1:length(viz_args$INT_names),
                       labels = viz_args$INT_names) +
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
