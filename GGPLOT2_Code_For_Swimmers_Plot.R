SHH %>% 
# Get just the variables we need for the base of the plot
dplyr::select(Subject_ID, EndTime, Group) %>%
# Remove duplicate rows
dplyr::distinct() %>%
# Pipe into ggplot
ggplot(aes(Subject_ID, EndTime)) + # Base axis
geom_bar(stat="identity", aes(fill=factor(Group))) + # Bar plot. Colour by Group
geom_point(data=SHH.shapes, # Use SHH.shapes to add reponse points
aes(Subject_ID, time,shape=ResponseType), size= 2) +
coord_flip() + labs(fill="Group", colour="Symbol Key", shape="Symbol Key",  # Add labels
x="Subject ID ", y="Months since diagnosis",title="Swimmer Plot") +
theme(plot.title = element_text(hjust = 0.5), # Put title in the middle of plot 
      plot.caption = element_text(size=7, hjust=0)) # Make caption size smaller