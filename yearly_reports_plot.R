# This code reads in report counts and creates a bar chart by year
## ---- general set up ----
# load required packages and usefule functions
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# library(reshape2) #to melt data for ggplot
# library(gridExtra) # to plot multiple ggplots side by side
# library(RColorBrewer)
library(stringi)

#set input and output directory - where do you keep the data and analysis folder on your computer?
csv_file_path <-
  file.path(my_paths$inputs,
            "annual_plots",
            "annual_metricstracking_numberofreports.csv")

Output <-
  file.path(my_paths$outputs, "annual_plots")

## ---- read in data ----
cnts_csv <- read_csv(csv_file_path)
# str(cnts_csv)

## ---- restructure for plotting ----
longFormat_GOM <-
  cnts_csv %>%
  # get GOM rows
  filter(stringi::stri_startswith_fixed(`Permit Group`, "GOM")) %>%
  pivot_longer(
    cols = c(Logbooks,
             Declarations),
    names_to = "Type",
    values_to = "Counts"
  ) %>%
  select(-`No Fishing Reports`)

# leave year only in Permit Group colomn and rename it
longFormat_GOM$year <-
  gsub("^.+ (\\d+)", "\\1", longFormat_GOM$`Permit Group`)

# remove the column
longFormat_GOM %<>%
  select(-`Permit Group`)

# The same for SA
longFormat_SA <-
  cnts_csv %>%
  filter(stringi::stri_startswith_fixed(`Permit Group`, "SA")) %>%
  pivot_longer(
    cols = c(Logbooks,
             `No Fishing Reports`),
    names_to = "Type",
    values_to = "Counts"
  ) %>%
  select(-Declarations)

longFormat_SA$year <-
  gsub("^.+ (\\d+)", "\\1", longFormat_SA$`Permit Group`)

longFormat_SA %<>%
  select(-`Permit Group`)

## ---- Create a custom color scale and keep the same color for Logbooks in both plots ----

## ---- choose a palette ----

cbPalette <-
  c(
    "#999999",
             "#E69F00",
             "#56B4E9",
             "#009E73",
             "#F0E442",
             "#0072B2",
             "#D55E00",
             "#CC79A7"
  )

# The palette with black:
cbbPalette <-
  c(
    "#000000",
             "#E69F00",
             "#56B4E9",
             "#009E73",
             "#F0E442",
             "#0072B2",
             "#D55E00",
             "#CC79A7"
  )

## ---- choose colors ----

my_colors0 <-
  c(
    "Logbooks" = cbPalette[3],
    "No Fishing Reports" = cbPalette[7],
    "Declarations" = cbPalette[8]
  )

# or
my_colors1 <- rainbow(length(cnts_csv))
names(my_colors1) <- names(cnts_csv)

# or
set1_palette <- RColorBrewer::brewer.pal(9, "Set1")
# set1_palette
# Uncomment to see colors
# "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628" "#F781BF" "#999999"

my_colors2 <-
  c(
    "Logbooks" = set1_palette[3],
    "No Fishing Reports" = set1_palette[1],
    "Declarations" = set1_palette[8]
  )

my_colors <- my_colors2
# str(my_colors)
# should be "Named chr"

## ---- lock in factor level order (use unique when levels are present more than once in a varaible - like in year here) ----
longFormat_GOM$year <-
  factor(longFormat_GOM$year, levels = unique(longFormat_GOM$year))

longFormat_SA$year <-
  factor(longFormat_SA$year, levels = unique(longFormat_SA$year))

# ---- theme with transparent background ----
theme_transparent <- theme(
  legend.background = element_rect(fill = "transparent"),
  legend.title = element_blank(),
  #no legend background color or title
  legend.box.background = element_rect(fill = "transparent", colour = NA),
  #no fill color
  legend.key = element_rect(fill = "transparent"),
  #no fill color
  legend.spacing = unit(-1, "lines")
)

# To keep the same order of types across all plots
# 1) Desired order
type_order <- c("Logbooks",
                "Declarations",
                "No Fishing Reports")

## ---- Now Plot - call ggplot ----
plot_bars <- function(my_data, title) {
  # To keep the same order of types across all plots
  # 2) get current types
  my_type_order <- intersect(my_data$Type, type_order)
  # 3) add column with our order as factor and then use this column in aes/fill
  my_data %<>%
  mutate(type_reordered = factor(Type, levels = my_type_order
                                 )
         )
  
  my_plot <-
    ggplot(my_data, aes(x = year,
                        y = Counts,
                        fill = type_reordered)) +
    #call color variable - defines color by report type
    scale_fill_manual(values = my_colors) +
    #use geom_col bc stat = identity is default (vs geom_bar)
    geom_col(position = 'dodge') +
    # add counts on top of each bar
    geom_text(aes(label = Counts),
              position = position_dodge(width = 0.9),
              vjust = -0.25) +
    #Plot Formatting:
    #remove background grey color and grid, add horizontal grid lines from y axis labels
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(size = 0.5, colour = "gray"),
      panel.background = element_blank(),
      axis.line = element_line(color = "black")
    ) +
    #legend position and formatting
    theme_transparent +
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

## ---- compare logbooks number by year between GOM and SA ----

Format_logbooks <-
  cnts_csv %>%
  select(`Permit Group`, Logbooks)
# %>%
  # mutate(Year = gsub("^(.+) (\\d+)", "\\2", `Permit Group`
  #                              )
  #        ) %>%
  # mutate(`Permit Group` = gsub("^(.+) (\\d+)", "\\1", `Permit Group`
  #                              )
  #        ) %>%
  # pivot_longer(
  #   cols = c(Logbooks),
  #   names_to = "Permit Group",
  #   values_to = "Counts"
  # ) %>%
  # select(-`No Fishing Reports`)


logb_pl <- ggplot(Format_logbooks,
                  aes(x = `Permit Group`,
                      y = Logbooks
                      ),
                  fill = "green") +
  geom_col(position = 'dodge') +
  # add counts on top of each bar
  geom_text(aes(label = Logbooks),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  theme_transparent +
  theme(
    axis.text.x = element_text(angle = 45,
                               vjust = -0.01)
  )
  
  
logb_pl
