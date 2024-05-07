save_plot_to_file <-
function(file_full_name,
           plot_name) {
    ggplot2::ggsave(
      file_full_name,
      plot_name,
      width = 30,
      height = 20,
      units = "cm"
    )
  }
