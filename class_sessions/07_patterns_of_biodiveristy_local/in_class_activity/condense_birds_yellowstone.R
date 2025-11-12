library(tidyverse)
library(readxl)
# data from https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.ksn02v757
# Fagre, Danielle; Janousek, William; Dreitz, Victoria (2022), Avian species richness 
# and abundance shows stronger response to bison grazing intensity than to ecosystem 
# productivity, Dryad, Dataset, https://doi.org/10.5061/dryad.ksn02v757
species<-read_csv("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/doi_10.5061_dryad.r2280gbds__v3/readme.csv", skip=3) %>% 
  select("code"="Entry","species"='Entry expanded name')
  
  
  
birds_ystone<-read_csv("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/doi_10.5061_dryad.r2280gbds__v3/data_sample.csv") %>% 
  filter(Year==2012) %>% 
  select("AGCU":"ZOCA") %>% 
  colSums() 
names<-names(birds_ystone) %>% as_tibble() %>% rename(code=value)
birds_ystone<-birds_ystone %>% 
  as_tibble() %>% 
  rename(n=value) %>% 
  bind_cols(names) %>% 
  left_join(species) %>% 
  mutate(species=as.factor(species)) %>% 
  filter(n!=0)


  

  
  




head(birds_ystone,30)
str(birds_ystone)
write_csv(birds_ystone,"./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/birds_ystone.csv")

birds_ystone<-birds_ystone %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(cperc = csum/sum(n))
birds_ystone
birds_ystone_last_half<-birds_ystone %>% slice_tail(n=(nrow(birds_ystone)/2))

p<-ggplot(birds_ystone, aes(x=reorder(species, desc(n)), y=n)) + 
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 70, 10))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

p

hist(birds_ystone$n)
sum(birds_ystone$n)