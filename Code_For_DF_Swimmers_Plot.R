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
SHH <- read.csv("/Volumes/G-DRIVE mobile/SJiMB/SJiMB/Excellsheets/SJCY07_SHH_DF.csv")
SHH$Subject_ID <- as.character(SHH$Subject_ID)
SHH$Group <- as.factor(SHH$Group)
SHH$isContinued <- as.logical(SHH$isContinued)
SHH.shapes <- SHH %>%
# Get just the subject and response time columns
dplyr::select(Subject_ID, ResponseType, StartTime) %>%
# Melt the data frame, so one row per response value.
reshape2::melt(id.vars=c("Subject_ID", "ResponseType"), value.name="time")%>% 
# Remove na values
dplyr::filter(!is.na(time)) %>%
# Remove response variable column
dplyr::select(-variable) %>%
# Add 'start' to the end of the response type
dplyr::mutate(ResponseType=paste(ResponseType, "start", sep=" "))
# Add the end time for each 
SHH.shapes %<>%
dplyr::bind_rows(SHH %>%
dplyr::select(Subject_ID, EndTime, Response_End_Time, isContinued) %>%
# Place endtime as response endtime if not continuing and responseEndTime is NA
dplyr::mutate(Response_End_Time=dplyr::if_else(!isContinued & is.na(Response_End_Time), EndTime, Response_End_Time)) %>%
  dplyr::select(-EndTime, -isContinued) %>%
# Remove other existing NA Response_End_Times
dplyr::filter(!is.na(Response_End_Time)) %>%
dplyr::mutate(ResponseType="NED") %>%
dplyr::rename(time=Response_End_Time))
# Add on the arrow sets
SHH.shapes %<>% 
  dplyr::bind_rows(SHH %>%
                        dplyr::select(Subject_ID, EndTime, isContinued) %>%
                         dplyr::filter(isContinued) %>%
                         dplyr::select(-isContinued) %>%
                         dplyr::mutate(ResponseType="Continued Response") %>%
                         dplyr::mutate(EndTime=EndTime+0.25) %>%
                         dplyr::rename(time=EndTime))
responseLevels = c("Continued Response", "Response Ends")

# Convert responseType to factor and set the levels
SHH.shapes %<>% 
  dplyr::mutate(ResponseType = factor(ResponseType, levels=responseLevels)) %>%
  # Order by response type
  dplyr::arrange(desc(ResponseType))

unicode = list(triangle=sprintf('\u25B2'),
              arrow=sprintf('\u2794'))

SHH %>% 
  # Get just the variables we need for the base of the plot
  dplyr::select(Subject_ID, EndTime, Group) %>%
  # Remove duplicate rows
  dplyr::distinct() %>%
  # Pipe into ggplot
  ggplot(aes(Subject_ID, EndTime)) + # Base axis
  geom_bar(stat="identity", aes(fill=factor(Group))) + # Bar plot. Colour by Group
  geom_point(data=SHH.shapes, # Use SHH.shapes to add reponse points
             aes(Subject_ID, time,shape=ResponseType), size= 3)
  coord_flip() +labs(fill="Group", colour="Symbol Key", shape="Symbol Key",  # Add labels
       x="Subject ID ", y="Months since diagnosis",
       title="Swimmer Plot") +
  theme(plot.title = element_text(hjust = 0.5), # Put title in the middle of plot
        plot.caption = element_text(size=7, hjust=0)) # Make caption size smaller