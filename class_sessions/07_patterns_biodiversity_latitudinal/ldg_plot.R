library(tidyverse)
library(ggrepel)
data<-read_csv("./class_materials/class_sessions/07_patterns_biodiversity_latitudinal/ldg_data.csv")

# load ggplot2

data_mod<-data %>% pivot_longer(!c(city,lat), names_to = "category", values_to = "count") %>% 
  drop_na(count)



ggplot(data_mod, aes(x=lat, y=count, color=category, label=city)) + 
  geom_point(size=2) +
  geom_vline(xintercept = 23,color="darkgray", linetype="dashed")+
  geom_vline(xintercept = -23,color="darkgray", linetype="dashed")+
  facet_wrap(~category,ncol = 3) +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = FALSE)+
  coord_flip()+
  geom_text(check_overlap = TRUE)
  geom_text_repel(
    data = data_mod,
    aes(
      x = lat,
      y = count,
      label = city
    ))


ggplot(data_mod, aes(x=lat, y=count, color=category)) + 
  geom_point(size=2) +
  geom_vline(xintercept = 23,color="darkgray", linetype="dashed")+
  geom_vline(xintercept = -23,color="darkgray", linetype="dashed")+
  facet_wrap(~category,ncol = 1, scales = "free") +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = FALSE)+
  coord_flip()+
  # geom_text(check_overlap = TRUE)
  geom_text_repel(
    data = data_mod,
    aes(
      x = lat,
      y = count,
      label = city
    ))




  # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)
