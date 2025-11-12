library(tidyverse)
library(readxl)
# data from: https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.j14g206
# Borges, Sergio H.; Baccaro, Fabricio; Moreira, Marcelo; Choueri, Erik, L. (2019),
# Data from: Bird assemblages on Amazonian river islands: patterns of species diversity 
# and composition, Dryad, Dataset, https://doi.org/10.5061/dryad.j14g206

birds_am<-read_xlsx("~/Dropbox (UFL)/Teaching/IDS 2935 - Future of Rain Forests/IDS2935_RainForests/class_materials/class_sessions/07_patterns_of_biodiveristy_local/in_class_activity/raw_data/Supplementary_material.xlsx", sheet="Capture_data",skip=1) %>% 
  select(-Seq_alfa,-Seq_tax) %>%
  rename(species=Species) %>% 
  mutate(n = rowSums(across(where(is.numeric)))) %>% 
  select(species,n) %>% 
  arrange(desc(n)) %>% 
  separate(species, remove=FALSE,c("a","b")) %>% 
  mutate(code=paste(str_extract(a, "^.{2}"),str_extract(b, "^.{2}"), sep="")) %>% 
  mutate(code=toupper(code)) %>% 
  select(code,species,n)
birds_am

  

slice(birds_am,24:30)
head(birds_am,13)
str(birds_am)
write_csv(birds_am,"~/Dropbox (UFL)/Teaching/IDS 2935 - Future of Rain Forests/IDS2935_RainForests/class_materials/class_sessions/07_patterns_of_biodiveristy_local/in_class_activity/raw_data/birds_am.csv")

birds_am<-birds_am %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(cperc = csum/sum(n))
birds_am
birds_am_last_half<-birds_am %>% slice_tail(n=(nrow(birds_am)/2))
# p<-ggplot(birds_am, aes(x=reorder(species, desc(1-cperc)), y=cperc)) + 
p<-ggplot(birds_am, aes(x=reorder(species, desc(n)), y=n)) +
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 200, 20))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

p


# How many species of birds did they capture?
birds_am %>% summarize(n_distinct(species))
# How many total birds of all species combined did they capture?
sum(birds_am$n)
# What is the most common species?
birds_am %>% arrange(desc(n)) %>% slice(1) %>% select(species)
# How many individuals does it have?
birds_am %>% arrange(desc(n)) %>% slice(1) %>% select(n)

# Arrange the species in order from _most_ abundant to _least_ 
# abundant. How many species do you need to count until you have 
# counted the first 50% of the trees in the plot? 
birds_am %>% 
  arrange(desc(n)) %>% 
  filter(cperc<0.50) %>% 
  summarize(n_distinct(species))

# Arrange the species in order from least abundant to most 
# abundant. Now select the 50% of species that 
# are the *least* abundant, how many species is this?
#   
birds_am %>% 
  arrange(desc(n)) %>% 
  filter(cperc>0.50) %>% 
  summarize(n_distinct(species))

# How many species are represented by only 1 individual?

birds_am %>% 
  arrange(desc(n)) %>% 
  filter(n==1) %>% 
  summarize(n_distinct(species))

hist(birds_am$n)
sum(birds_am$n)