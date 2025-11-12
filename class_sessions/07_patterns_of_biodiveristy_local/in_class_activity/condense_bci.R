library(tidyverse)
load("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/doi_10.15146_5xcp-0d46__v2/bci.spptable.RData")
load("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/doi_10.15146_5xcp-0d46__v2/bci.tree/bci.tree8.RData")
taxonomy<-bci.spptable %>% 
  select(sp,Genus,Species,Family) %>% 
  mutate(species=paste(Genus, Species, sep= " ")) %>% 
  select(sp,species, family=Family)
names(taxonomy)
names(bci.tree8)
bci_25 <- bci.tree8 %>% 
  left_join(taxonomy) %>% 
  filter(status=="A") %>% 
  select(treeID, gx,sp, dbh, species, family) %>% 
  filter(gx<501) %>% 
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


bci_50 <- bci.tree8 %>% 
  left_join(taxonomy) %>% 
  filter(status=="A") %>% 
  select(treeID, gx,sp, dbh, species, family) %>% 
  # filter(CensusID==171) %>% 
  # filter(CensusID==6) %>% 
  group_by(treeID) %>% 
  group_by(sp,species) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>0) %>% 
  separate(species, remove=FALSE,c("a","b")) %>% 
  mutate(code=paste(str_extract(a, "^.{2}"),str_extract(b, "^.{2}"), sep="")) %>% 
  mutate(code=toupper(code)) %>% 
  select(code,species,n)



bci_50_xy <- bci.tree8 %>% 
  left_join(taxonomy) %>% 
  filter(status=="A") %>% 
  select(treeID, gx,gy,sp, dbh, species, family)
df<-bci_50_xy %>% filter(sp=="anacex") %>% select(gx,gy,dbh)
df<-bci_50_xy %>% filter(sp=="hybapr") %>% select(gx,gy,dbh)
df<-bci_50_xy %>% filter(sp=="swars1") %>% select(gx,gy,dbh)

Chamguava shippii - negatively assoc with slope
df<-bci_50_xy %>% filter(sp=="cha2sc") %>% select(gx,gy,dbh)
Chrysoclamys eclipes - positively associated
df<-bci_50_xy %>% filter(sp=="chr1ec") %>% select(gx,gy,dbh)


Pouteria reticulata - neutral wrt slope

ggplot(df, aes(x = gx, y = gy)) +
  geom_point()





head(bci,20)
str(bci)
write_csv(bci_25,"./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/bci_trees_25.csv")
write_csv(bci_50,"./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/bci_trees_50.csv")

# bci<-bci_25
bci<-bci_50
bci<-bci %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(cperc = csum/sum(n))
bci


# How many species of trees are in the plot?
bci %>% summarize(n_distinct(species))
# How many total trees of all species combined are there in the plot?
sum(bci$n)
# What is the most common species?
bci %>% arrange(desc(n)) %>% slice(1) %>% select(species)
# How many individuals does it have?
bci %>% arrange(desc(n)) %>% slice(1) %>% select(n)

# Arrange the species in order from _most_ abundant to _least_ 
# abundant. How many species do you need to count until you have 
# counted the first 50% of the trees in the plot? 
bci %>% 
  arrange(desc(n)) %>% 
  filter(cperc<0.50) %>% 
  summarize(n_distinct(species))

# Arrange the species in order from least abundant to most 
# abundant. Now select the 50% of species that 
# are the *least* abundant, how many species is this?
#   
  bci %>% 
  arrange(desc(n)) %>% 
  filter(cperc>0.50) %>% 
  summarize(n_distinct(species))

# How many species are represented by only 1 individual?

  bci %>% 
  arrange(desc(n)) %>% 
  filter(n==1) %>% 
  summarize(n_distinct(species))


bci_last_50perc<-bci %>% slice_tail(n=(nrow(bci)/2))

p<-ggplot(bci, aes(x=reorder(species, desc(n)), y=n)) + 
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 16000, 2000))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

p

hist(bci$n)
