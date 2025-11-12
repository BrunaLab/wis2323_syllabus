library(tidyverse)
library(here)
# https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/tree/master/tree_main_census/data
# https://forestgeo.si.edu/sites/north-america/smithsonian-conservation-biology-institute

here("class_materials","class_sessions","07_patterns_of_biodiveristy_local","in_class_activity","raw_data","birds_am.csv")
data<-read_csv(here("class_materials","class_sessions","07_patterns_of_biodiveristy_local","in_class_activity","raw_data","birds_am.csv"))
data<-load(here("class_materials","class_sessions","07_patterns_of_biodiveristy_local","in_class_activity","scbi.spptable.rdata"))

# load("./IDS2935_RainForests/class_materials/class_sessions/07_patterns_of_biodiveristy_local/in_class_activity/raw_data/birds_am.csv")
# 
# load("./IDS2935_RainForests/class_materials/class_sessions/07_patterns_of_biodiveristy_local/in_class_activity/raw_data/scbi.spptable.rdata")
# load("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity_local/in_class_activity/raw_data/scbi/scbi.stem1.rdata")
taxonomy<-scbi.spptable %>% 
  select(sp,Genus,Species,Family) %>% 
  mutate(species=paste(Genus, Species, sep= " ")) %>% 
  select(sp,species, family=Family)
names(taxonomy)
names(scbi.stem1)
scbi <- scbi.stem1 %>% 
  left_join(taxonomy) %>% 
  filter(status=="A") %>% 
  group_by(treeID) %>% # TO CORRECT FOR HAVING >1 STEM PER INDIV
  slice(1) %>% # TO CORRECT FOR HAVING >1 STEM PER INDIV
  select(treeID, sp, dbh, species, family) %>% 
  # filter(CensusID==171) %>% 
  # filter(CensusID==6) %>% 
  group_by(treeID) %>% 
  group_by(species) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>0) %>% 
  separate(species, remove=FALSE,c("a","b")) %>% 
  mutate(code=paste(str_extract(a, "^.{2}"),str_extract(b, "^.{2}"), sep="")) %>% 
  mutate(code=toupper(code)) %>% 
  select(code,species,n)


head(scbi,20)
str(scbi.stem1)
write_csv(scbi,"./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/virginia_trees.csv")

scbi<-scbi %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(cperc = csum/sum(n))
scbi


# How many species of trees are in the plot?
scbi %>% summarize(n_distinct(species))
# How many total trees of all species combined are there in the plot?
sum(scbi$n)
# What is the most common species?
scbi %>% arrange(desc(n)) %>% slice(1) %>% select(species)
# How many individuals does it have?
scbi %>% arrange(desc(n)) %>% slice(1) %>% select(n)

# If you arrange the species in order from most abundant to least abundant, 
# how many species do you need to include until you have counted 50% 
# of the trees in the plot? 
scbi %>% 
  arrange(desc(n)) %>% 
  filter(cperc<0.50) %>% 
  summarize(n_distinct(species))

# Arrange the species in order from least abundant to most 
# abundant. Now select the 50% of species that 
# are the *least* abundant, how many species is this?
#   
scbi %>% 
  arrange(desc(n)) %>% 
  filter(cperc>0.50) %>% 
  summarize(n_distinct(species))

# How many species are represented by only 1 individual?

scbi %>% 
  arrange(desc(n)) %>% 
  filter(n==1) %>% 
  summarize(n_distinct(species))



p<-ggplot(scbi, aes(x=reorder(species, desc(n)), y=n)) + 
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 10000, 1000))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

p

hist(scbi$n)
sum(scbi$n)