library(tidyverse)
library(readxl)
library(here)
library(viridis)

peru <- read_csv(here("class_sessions", "/12_2_forest_loss", "in-class-activity-forest-loss","Primary Forest loss in Peru","treecover_extent_2000_in_primary_forests_2001_tropics_only__ha.csv"),
) %>% select(-area__ha) %>% rename (cover=umd_tree_cover_extent_2000__ha) %>% mutate(year=2000)
rwanda <- read_csv(here("class_sessions", "/12_2_forest_loss", "in-class-activity-forest-loss","Primary Forest loss in Rwanda","treecover_extent_2000_in_primary_forests_2001_tropics_only__ha.csv"),
)%>% select(-area__ha) %>% rename (cover=umd_tree_cover_extent_2000__ha) %>% mutate(year=2000)

cover_2000<-bind_rows(peru,rwanda)

peru_loss <- read_csv(here("class_sessions", "/12_2_forest_loss", "in-class-activity-forest-loss","Primary Forest loss in Peru","treecover_loss_in_primary_forests_2001_tropics_only__ha.csv"),
) %>% select(iso, year=umd_tree_cover_loss__year, loss=umd_tree_cover_loss__ha)
rwanda_loss <- read_csv(here("class_sessions", "/12_2_forest_loss", "in-class-activity-forest-loss","Primary Forest loss in Rwanda","treecover_loss_in_primary_forests_2001_tropics_only__ha.csv"),
)%>% select(iso, year=umd_tree_cover_loss__year, loss=umd_tree_cover_loss__ha)

loss<-bind_rows(peru_loss,rwanda_loss)


data<-full_join(loss,cover_2000) %>% 
  arrange(iso,year) %>% 
  rename(cover_ha=cover,loss_ha=loss)  %>%
  replace_na(list(loss=0,loss_ha=0)) %>% 
  rename(cover_end_ha=cover_ha) %>% 
  mutate(cover_start_ha=lag(cover_end_ha)) %>% 
  relocate(cover_start_ha,.before=loss_ha) %>% 
  replace_na(list(cover_start_ha=0,cover_end_ha=0)) %>% 
  mutate(cover_end_ha=cover_start_ha-cover_end_ha) %>% 
  filter(!year==2000)


tabulator <- function(data) {
  
  for (i in seq_along(data$year)) {
    data$cover_end_ha[i]<-data$cover_start_ha[i]-data$loss_ha[i]  
    if (i == max(seq_along(data$year))) {
      print("finished")
    } else {
      data$cover_start_ha[i+1]<-data$cover_end_ha[i]  
    }
  }
return(data)
  }
peru<-data %>% filter(iso=="PER")
rwanda<-data %>% filter(iso=="RWA")
peru<-tabulator(peru)
rwanda<-tabulator(rwanda)

data_all<-bind_rows(peru,rwanda)

data_all<- data_all %>% mutate(deforestation_rate=loss_ha/cover_start_ha*100)
data_all<- data_all %>% mutate(
  cover_start_km2=cover_start_ha/100,
  loss_km2=loss_ha/100,
  cover_end_km2=cover_end_ha/100)

data_all %>% 
  filter(year>2001) %>% 
  group_by(iso) %>% 
  summarize(sum(loss_km2))

data_all %>% 
  filter(year>2001) %>% 
  group_by(iso) %>% 
  summarize(sum(loss_ha)/1000000)

def_rate_plot<-ggplot(data_all, aes(x=year, y=deforestation_rate, group=iso,color=iso)) +
  geom_line() +
  labs(x="Year",y= "Rate of Deforestation (%)", color="Country")+
  ggtitle("Annual rate of deforestation") +
  scale_y_continuous(limits = c(0, .3), breaks = seq(0,.3,by=.05))+
  theme_classic()
  def_rate_plot
  
  

  def_annual_plot<-ggplot(data_all, aes(x=year, y=loss_ha)) +
    geom_bar(stat="identity") +
    labs(x="Year",y= "hectares", color="Country")+
    ggtitle("Annual deforestation") +
    # scale_y_continuous(limits = c(0, .3), breaks = seq(0,.3,by=.05))+
    facet_grid(rows = vars(iso),scales="free_y")+
    theme_classic()
  def_annual_plot
  

  