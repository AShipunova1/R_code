# This code reads in report counts and creates a bar chart by year
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

#general set up:
#load required packages
library(ggplot2)
library(tidyr) #analysis of dataframes
library(dplyr) #analysis of dataframes
library(reshape2) #to melt data for ggplot
library(readxl)  # reading in .xlsx
library(gridExtra) # to plot multiple ggplots side by side
library(RColorBrewer)
library(stringi)

#set input and output directory - where do you keep the data and analysis folder on your computer?
csv_file_path <-
  file.path(my_paths$inputs,
            "annual_plots",
            "annual_metricstracking_numberofreports.csv")

Output <-
  file.path(my_paths$outputs, "annual_plots")

#read in data
cnts_csv <- read_csv(csv_file_path)
# str(cnts_csv)

#restructure for plotting
longFormat_GOM <-
  cnts_csv %>%
  # get GOM rows
  filter(stringi::stri_startswith_fixed(`Permit Group`, "GOM")) %>%
  pivot_longer(cols = c(Logbooks,
                        Declarations),
               names_to = "Type",
               values_to = "Counts") %>%
  select(-`No Fishing Reports`)

# leave a year only
longFormat_GOM$year <-
  gsub("^.+ (\\d+)", "\\1", longFormat_GOM$`Permit Group`)

longFormat_GOM %<>%
  select(-`Permit Group`)

# The same for SA
longFormat_SA <-
  cnts_csv %>%
  filter(stringi::stri_startswith_fixed(`Permit Group`, "SA")) %>%
  pivot_longer(cols = c(Logbooks,
                        `No Fishing Reports`),
               names_to = "Type",
               values_to = "Counts") %>%
  select(-Declarations)

longFormat_SA$year <-
  gsub("^.+ (\\d+)", "\\1", longFormat_SA$`Permit Group`)

longFormat_SA %<>%
  select(-`Permit Group`)

#Create a custom color scale
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

my_colors <- c("Logbooks" = "blue", "No Fishing Reports" = "red", )

colors_GOM <- length(unique(longFormat_GOM$Type))  #2
myColors <- brewer.pal(colors_GOM, "Accent")  #I need 2 unique colors
names(myColors) <-
  unique(longFormat_GOM$Type)  #define names of the 8 colors based on Type
colScale <-
  scale_colour_manual(name = "Type", values = myColors) #create variable to store colors for plot

# lock in factor level order (use unique when levels are present more than once in a varaible - like in year here)
longFormat_GOM$year <-
  factor(longFormat_GOM$year, levels = unique(longFormat_GOM$year))

longFormat_SA$year <-
  factor(longFormat_SA$year, levels = unique(longFormat_SA$year))

#Now Plot - call ggplot
plot_bars <- function(my_data, title) {
  my_plot <-
  ggplot(my_data, aes(x = year,
                      y = Counts,
                      fill = Type,
                      )
         ) +
    # scale_fill_brewer(palette = "Accent") +
    #use geom_col bc stat = identity is default (vs geom_bar)
    geom_col(position = 'dodge') +
    # add counts on top of each bar
    geom_text(aes(label = Counts), 
              position = position_dodge(width = 0.9),
              vjust = -0.25
              ) +
    # colScale +
    # colScale +  #call color variable - defines color by unique species
    #Plot Formatting:
    #remove background grey color and grid, add horizontal grid lines from y axis labels
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(size = 0.5, colour = "gray"),
      panel.background = element_blank(),
      axis.line = element_line(color = "black")
    ) +
    #legend position and formatting
    theme(
      legend.background = element_rect(fill = "transparent"),
      legend.title = element_blank(),
      #no legend background color or title
      legend.box.background = element_rect(fill = "transparent", colour = NA),
      #no fill color
      legend.key = element_rect(fill = "transparent"),
      #no fill color
      legend.spacing = unit(-1, "lines")
    ) +
    #plot title
    ggtitle(paste(title, "by year", sep = " "))  +
    theme(plot.title = element_text(hjust = 0.5, vjust = 2.12))
  
  #save plot to tiff
  ggsave(
    file.path(Output, paste0(title, ".tiff")),
    units = "in",
    width = 7,
    height = 4,
    dpi = 600,
    compression = 'lzw'
  )
  
  return(my_plot)
}
# str(longFormat_GOM)
gulf_pl <- plot_bars(longFormat_GOM, "Gulf")
sa_pl <- plot_bars(longFormat_SA, "South Atlantic")
