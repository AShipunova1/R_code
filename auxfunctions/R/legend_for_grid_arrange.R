legend_for_grid_arrange <-
function(legend_plot) {
  # legend_plot <-
  #   ggplot(data = legend_data, aes(x1, y1, colour = ll)) +
  #   geom_text(dat = legend_data,
  #             aes(label = ll),
  #             hjust = 0) +
  #   scale_color_manual(
  #     name = 'Lines',
  #     breaks = c('Mean', 'Num of weeks'),
  #     values = my_colors
  #   )
  #
  # legend_plot

  # Obtain the legend from a 'legend_plot' using the 'get_legend' function from the 'cowplot' package.
  my_legend <-
    cowplot::get_legend(legend_plot)

  return(my_legend)
}
