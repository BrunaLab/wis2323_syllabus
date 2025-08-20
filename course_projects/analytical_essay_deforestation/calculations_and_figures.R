library(tidyverse)
library(cowplot)
library(here)


# brazil ------------------------------------------------------------------

here()
brazil_2000<- read.csv(here("class_materials","course_projects","analytical_essay_deforestation","brazil","treecover_extent_2000_in_primary_forests_2001_tropics_only__ha.csv")) 
brazil_loss<- read.csv(here("class_materials","course_projects","analytical_essay_deforestation","brazil","treecover_loss_in_primary_forests_2001_tropics_only__ha.csv")) %>% 
  rename(year=umd_tree_cover_loss__year,
         loss=umd_tree_cover_loss__ha) %>% 
  mutate('tree_cover_start' = ifelse(year=="2001",brazil_2000[1,2] ,NA)) %>% 
  relocate(tree_cover_start, .after="iso") %>% 
  mutate(tree_cover_final = tree_cover_start-loss) %>% 
  relocate(tree_cover_final, .after='loss')  

for (i in seq(2002,2022)) {
    brazil_loss$tree_cover_start[brazil_loss$year==i]=brazil_loss$tree_cover_final[brazil_loss$year==(i-1)] 
    brazil_loss$tree_cover_final[brazil_loss$year==i]=brazil_loss$tree_cover_start[brazil_loss$year==i]-brazil_loss$loss[brazil_loss$year==i]
} 

brazil_loss<-brazil_loss %>% 
  mutate(annual_perc_loss=loss/tree_cover_start*100) %>% 
  relocate(annual_perc_loss, .after='tree_cover_final')  

brazil_loss

# indonesia ----------------------------------------------------------------



indonesia_2000<- read.csv(here("class_materials","course_projects","analytical_essay_deforestation","indonesia","treecover_extent_2000_in_primary_forests_2001_tropics_only__ha.csv")) 
indonesia_loss<- read.csv(here("class_materials","course_projects","analytical_essay_deforestation","indonesia","treecover_loss_in_primary_forests_2001_tropics_only__ha.csv")) %>% 
  rename(year=umd_tree_cover_loss__year,
         loss=umd_tree_cover_loss__ha) %>% 
  mutate('tree_cover_start' = ifelse(year=="2001",indonesia_2000[1,2] ,NA)) %>% 
  relocate(tree_cover_start, .after="iso") %>% 
  mutate(tree_cover_final = tree_cover_start-loss) %>% 
  relocate(tree_cover_final, .after='loss')  

for (i in seq(2002,2022)) {
  indonesia_loss$tree_cover_start[indonesia_loss$year==i]=indonesia_loss$tree_cover_final[indonesia_loss$year==(i-1)] 
  indonesia_loss$tree_cover_final[indonesia_loss$year==i]=indonesia_loss$tree_cover_start[indonesia_loss$year==i]-indonesia_loss$loss[indonesia_loss$year==i]
} 

indonesia_loss<-indonesia_loss %>% 
  mutate(annual_perc_loss=loss/tree_cover_start*100) %>% 
  relocate(annual_perc_loss, .after='tree_cover_final')  
indonesia_loss


# bind the two datasets ---------------------------------------------------


forest_data<-bind_rows(brazil_loss,indonesia_loss) %>% 
  filter(year>2001)



# rate plot ---------------------------------------------------------------

cols <- c("IDN" = "darkred", "BRA" = "darkblue")
annual_rate_plot<-ggplot(forest_data, 
                         aes(x=year, 
                             y=annual_perc_loss, 
                             # group=iso, 
                             color=iso
                             )) +
  # geom_line(color="navyblue")+
  geom_line()+
  # geom_bar(stat="identity", fill="navyblue")+
  labs(
    x = "Year",
    y = "rate (%)")+
  ggtitle("Annual rate of forest loss")+
  scale_x_continuous(breaks = seq(from = 2002, to = 2022, by = 1))+
  scale_y_continuous(breaks = seq(from = 0, to = 2.1, by = 0.1), expand = c(0, 0))+
  scale_color_manual(values=cols,
                     name="Country",
                     labels = c("IDN" = "Indonesia", 
                                "BRA" = "Brazil"))
annual_rate_plot<-annual_rate_plot + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title = element_text(face="bold"))+
  theme(plot.title = element_text(face="bold"))
# +
#   theme(legend.position="top")
annual_rate_plot


# annual forest loss ------------------------------------------------------





cols <- c("IDN" = "darkred", "BRA" = "darkblue")
annual_loss_plot<-ggplot(forest_data, 
                         aes(x=year, 
                             y=loss, 
                             # group=iso, 
                             color=iso
                         )) +
  # geom_line(color="navyblue")+
  geom_line()+
  # geom_bar(stat="identity", fill="navyblue")+
  labs(
    x = "Year",
    y = "loss (ha)")+
  ggtitle("Annual forest loss")+
  scale_x_continuous(breaks = seq(from = 2002, to = 2022, by = 1))+
  scale_y_continuous(breaks = seq(from = 0, to = 3000000, by = 500000), expand = c(0, 0))+
  scale_color_manual(values=cols,
                     name="Country",
                     labels = c("IDN" = "Indonesia", 
                                "BRA" = "Brazil"))
annual_loss_plot<-annual_loss_plot + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title = element_text(face="bold"))+
  theme(plot.title = element_text(face="bold"))
# +
#   theme(legend.position="top")
annual_loss_plot




annual_loss_plot<-ggplot(forest_data, 
                         aes(x=year, 
                             y=loss, 
                             fill=iso
                         )) +
  geom_bar(stat="identity", width=.75, position = "dodge")+
  labs(
    x = "Year",
    y = "hectares")+
  ggtitle("Annual Forest loss, 2001-2021")+
  scale_x_continuous(breaks = seq(from = 2001, to = 2021, by = 1))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=c("darkred", "darkblue"), 
                    name="Country",
                    breaks=c("IDN", "BRA"),
                    labels=c("Indonesia", "Brazil"))
  
  
annual_loss_plot<-annual_loss_plot + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title = element_text(face="bold"))+
  theme(plot.title = element_text(face="bold"))
# +
#   theme(legend.position="top")
annual_loss_plot



# total forest loss -------------------------------------------------------


total_loss<-forest_data %>% 
  group_by(iso) %>% 
  summarize(total_loss=sum(loss)) %>% 
  mutate(iso=if_else(iso=="BRA","Brazil","Indonesia"))



total_loss_plot<-ggplot(total_loss, 
                         aes(x=iso, 
                             y=total_loss, 
                             fill=iso
                         )) +
  geom_bar(stat="identity", width=.75, position = "dodge")+
  labs(
    x = "Country",
    y = "hectares")+
  ggtitle("Total Forest loss, 2001-2021")+
  # scale_x_continuous(breaks = seq(from = 2001, to = 2021, by = 1))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=c("darkred", "darkblue"), 
                    name="Country",
                    # breaks=c("IDN", "BRA"),
                    # labels=c("Indonesia", "Brazil")
                    )


total_loss_plot<-total_loss_plot + 
  theme_classic()+
  theme(axis.text.x = element_text(face="bold"))+
  theme(legend.title = element_text(face="bold"))+
  theme(plot.title = element_text(face="bold"))+
  theme(legend.position="none")
total_loss_plot



# Step 1: Call the pdf command to start the plot
pdf(file = "./class_materials/projects_and_code/analytical_essay_deforestation/forest_loss_plots.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
plot_grid(total_loss_plot,
          annual_rate_plot,
          annual_loss_plot,
          ncol=1)


# Step 3: Run dev.off() to create the file!
dev.off()



