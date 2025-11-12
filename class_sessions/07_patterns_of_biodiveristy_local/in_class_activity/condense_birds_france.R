library(tidyverse)
library(readxl)
# data from https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.jq2bvq878
# García-Navas, Vicente; Thuiller, Wilfried (2020), Bird abundance data for the 
# period 2002-2014 from the French Breeding Bird Survey (STOC), Dryad, Dataset, 
# https://doi.org/10.5061/dryad.jq2bvq878

birds_fr<-read_xlsx("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/STOC_abundance_bird_data.xlsx", sheet="All_years") %>% 
  filter(YEAR==2010) %>% 
  rename(n=N,
         species=NAME) %>% 
  filter(SITE==701314) %>% 
  arrange(desc(n)) %>% 
  mutate(species=gsub("_"," ",species)) %>% 
  separate(species, remove=FALSE,c("a","b")) %>% 
  mutate(code=paste(str_extract(a, "^.{2}"),str_extract(b, "^.{2}"), sep="")) %>% 
  mutate(code=toupper(code)) %>% 
  select(code,species,n)
birds_fr

  
  




head(birds_fr,30)
str(birds_fr)
write_csv(birds_fr,"./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/birds_fr.csv")

birds_fr<-birds_fr %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(cperc = csum/sum(n))
birds_fr
birds_fr_last_half<-birds_fr %>% slice_tail(n=(nrow(birds_fr)/2))
# p<-ggplot(birds_fr, aes(x=reorder(species, desc(1-cperc)), y=cperc)) + 
p<-ggplot(birds_fr, aes(x=reorder(species, desc(n)), y=n)) + 
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 25, 5))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

p

hist(birds_fr$n)
sum(birds_fr$n)