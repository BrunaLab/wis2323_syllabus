library(tidyverse)
# harvard:hf339-01: https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF339

harvard<-read_csv("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/harvard_forest_trees/hf339-01-hf-tree-2013.csv")
hspp<-read_csv("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/harvard_forest_trees/hf339-02-species.csv")

harvard<-harvard %>% 
  count(species) %>% 
  rename(species.code=species) %>% 
  arrange(desc(n)) %>% 
  left_join(hspp) %>% 
  rename(code=species.code,
         species=species.name) %>% 
  relocate(species, .after="code") %>% 
  mutate(code=toupper(code))
harvard
  


head(harvard,20)
str(harvard)
write_csv(harvard,"./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/harvard_trees.csv")

harvard<-harvard %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(cperc = csum/sum(n))
harvard
harvard_last_half<-harvard %>% slice_tail(n=(nrow(harvard)/2))

p<-ggplot(harvard, aes(x=reorder(species, desc(n)), y=n)) + 
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 100, 20))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

p

hist(harvard$n)
sum(harvard$n)