library(magrittr)
library(stringi)
library(readr)   # Reading in the dataset
library(ggplot2) # Viewing the dataset
library(forcats) # Sorting factors
library(RColorBrewer) # Plot colours
library(dplyr, warn.conflicts=FALSE)   # Manipulating the dataframes
library(purrr, warn.conflicts=FALSE)   # Manipulating dataframe metadata
library(zoo, warn.conflicts=FALSE)     # Filling in  NA values
library(reshape2) # Reformmating dataframes 
NWNS <- read.csv("/Volumes/G-DRIVE mobile/SJiMB/SJiMB/Excellsheets/NWNS.csv")
NWNS.shapes<- read.csv("/Volumes/G-DRIVE mobile/SJiMB/SJiMB/Excellsheets/NWNS.shapes.code.csv")
NWNS%>% 
  # Get just the variables we need for the base of the plot
  dplyr::select(Subject_ID, EndTime, Group) %>%
  # Remove duplicate rows
  dplyr::distinct() %>%
  # Pipe into ggplot
  ggplot(aes(Subject_ID, EndTime)) + # Base axis
  geom_bar(stat="identity", aes(fill=factor(Group))) + # Bar plot. Colour by Group
  geom_point(data=NWNS.shapes, # Use SHH.shapes to add reponse points
             aes(Subject_ID, time,shape=ResponseType), size= 5) +
  coord_flip() + labs(fill="Group", colour="Symbol Key", shape="Symbol Key",  # Add labels
                      x="Subject ID ", y="Months since diagnosis",title="Swimmer Plot") +
  theme(plot.title = element_text(hjust = 0.5), # Put title in the middle of plot 
        plot.caption = element_text(size=7, hjust=0)) # Make caption size smaller