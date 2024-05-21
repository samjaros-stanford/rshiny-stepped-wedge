################################################################################
# Make plot
#
# Create stepwise wedge study plot given study specification in long format
#
make_plot <- function(study, ...){
  ggplot(data = study, aes(y=factor(COH), color=factor(INT))) +
    geom_segment(aes(x=INT_start, xend=INT_end)) +
    scale_color_brewer(name=NULL, palette="Set1") +
    ylab("Cohort") +
    xlab("Time") +
    theme_classic() +
    theme(legend.position = "bottom")
}


################################################################################
# Testing

# study = tribble(~COH, ~INT, ~INT_start, ~INT_end,
#                 1,    1,    0,          2,
#                 1,    2,    2,          4,
#                 1,    3,    4,          10,
#                 2,    1,    0,          3,
#                 2,    2,    3,          5,
#                 2,    3,    5,          10,
#                 3,    1,    0,          4,
#                 3,    2,    4,          6,
#                 3,    3,    6,          10,
#                 4,    1,    0,          5,
#                 4,    2,    5,          7,
#                 4,    3,    7,          10)
# make_plot(study)