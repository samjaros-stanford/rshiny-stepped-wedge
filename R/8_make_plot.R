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
                       breaks = 1:max(study$INT),
                       labels = viz_args$INT_names) +
    scale_x_continuous(viz_args$time_units, breaks=scales::breaks_pretty()) +
    scale_y_discrete("Cohort", 
                     breaks = 1:max(study$COH), 
                     labels = viz_args$COH_names,
                     limits = rev) +
    theme_classic() +
    theme(text = element_text(size = 16),
          legend.position = "bottom",
          panel.grid.major.x = element_line(color="grey90", linewidth=0.5))
}


################################################################################
# Testing

study = tribble(~COH, ~INT, ~INT_start, ~INT_end,
                1,    1,    0,          2,
                1,    2,    2,          4,
                1,    3,    4,          10,
                2,    1,    0,          3,
                2,    2,    3,          5,
                2,    3,    5,          10,
                3,    1,    0,          4,
                3,    2,    4,          6,
                3,    3,    6,          10,
                4,    1,    0,          5,
                4,    2,    5,          7,
                4,    3,    7,          10)
make_plot(study)