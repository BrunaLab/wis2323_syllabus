data <- read_csv("./class_materials/syllabus/assignments.csv") %>%
# data <- read_csv("./assignments.csv") %>%
  mutate(semester="Assignments") %>% 
  rename(tasks=Assignment) %>% 
  mutate(tasks=as.factor(tasks),
         semester=as.factor(semester)
  ) %>% 
  arrange(Points) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(tasks=factor(tasks, levels=tasks)) %>% 
  mutate(perc=round(Points/sum(Points)*100),2) %>% 
  mutate(barlabel=paste(tasks,Points,sep="\n")) %>% 
  mutate(barlabel=paste(barlabel,"pts",sep=" ")) %>% 
  mutate(barlabel=paste(barlabel," (", perc,"%)", sep="")) %>% 
  mutate(barlabel=paste(barlabel,"\n", Requirements,sep=""))

total_pts<-sum(data$Points)

library(RColorBrewer)
library("viridis")   
# Stacked

color_count<-nrow(data)
my.cols<-brewer.pal(color_count+4,"YlGn")
my.cols<-my.cols[color_count:(color_count+4)]


ggplot(data, 
       aes(fill=tasks, label = barlabel, y=Points, x=tasks)) + 
  # scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  # scale_colour_brewer(palette = "YlGn", direction = - 1) + 
  scale_fill_manual(values = my.cols)+
  # scale_fill_brewer(palette = "YlGn", direction=-1)+
  # labs(x = NULL, y = "Points",title = paste("Assignments (",total_pts," points total)", sep="")) +
  # scale_fill_viridis(discrete = T, option="D") +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3.5, 
            position = position_stack(vjust = 0.5),
            fontface="bold", 
            color="white")+
  scale_y_continuous(limits = c(0,501), expand = c(0, 0))+
  coord_flip()+
  theme_classic()+
  # theme(plot.margin = unit(c(0,4,0,0), "pt")) +
  theme(legend.position="none") +
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.title = element_text(size= 15))+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.title.x = element_text(face = "bold"))+
  theme(axis.title.x = element_text(size = 12))+
  theme(plot.title = element_text(size = 22))+
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("./class_materials/syllabus/icons/hw.png", width = 8, height = 4.5, units="in")