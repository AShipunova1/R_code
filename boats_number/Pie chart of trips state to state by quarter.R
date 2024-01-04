# This code creates donut plots for Council PPT

  #(1) Get file from Anna: Vessels starting a trip in one county or state and ending in another
  #(2) plot donuts of counts of trips from one state to another, by quarter

#libraries needed
library(lubridate)
library(tidyverse) #(analysis and visualization of DFs; tidyr, ggplot, tibble)
library(reshape2) #to melt data for ggplot
library(ggrepel)
library(scales)  #for commas
library(readxl)

#create path to your working and output directory - where do you keep the data on your computer?
Path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Compliance related analyses/Donut Chart/"
Inputs <- "Inputs/"
Outputs <- "Outputs/"

#read in Anna's output data file, from her analysis (removed rows with endport = NC)
  myDF <- read_excel(paste0(Path, Inputs, "Vessels starting a trip in one county or state and ending in another.xlsx"),
                     sheet = "states by home")  
  
  #create labels for plotting, of sum of count by quarter
  labels <- myDF %>% group_by(Quarter) %>% summarise(quarterly.sums = sum(Count))
  
  #left join labels to myDF by Quarter
  myDF_forPlotting <- left_join(myDF, labels, by = "Quarter")
        
#plot set up (donut)------------------------------------------------------------------
    
    #Donut hole size
    hsize <- 2.5
    
    #create x for donut plot
    df <- myDF_forPlotting %>% 
      mutate(x = hsize)
    
    # Get the positions of the labels
    df2 <- df %>%
      mutate(csum = rev(cumsum(rev(Count))),
             pos = Count/2 + lead(csum, 1),
             pos = if_else(is.na(pos), Count/2, pos))


#plot separate ggplot figures in for-loop    
 #create for-loop to loop through quarters
    
    #create list to save plots to
    plot_list = list()
    
    for(i in 1:length(labels$Quarter)) {
      
    plot_df <- df2 %>% filter(Quarter == labels$Quarter[i])
        
    #plot 
    p <- ggplot(plot_df, aes(x = "", y = Count, fill = as.factor(State_to_State_Label))) +
      geom_col(col = "black") + #adds black border
      coord_polar("y", start=0) +  #turns bar into pie chart
      
      #add data labels, and use position_stack(vjust= 0,5) to get labels in the correct position for pie charts
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 5) +
      
      #Add Figure Title, change Legend title 
      labs(title = paste0(unique(plot_df$Quarter),": Total State to Different State Movements = ", labels$quarterly.sums[i]), 
           fill = "State to State Movement\n") +
      
      #increase size of title text
      theme(plot.title = element_text(size=19)) +
      
      
      #plot theme to get rid of grey
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank()) 
 
    plot_list[[i]] = p
    
    }
    
    # Save plots to tiff. Makes a separate file for each plot.
    for (i in 1:length(labels$Quarter)) {
      file_name = paste("VesselMovement_StateToState", labels$Quarter[i], ".tiff", sep="")
      tiff(file_name)
      print(plot_list[[i]])
      dev.off()
    }
 
