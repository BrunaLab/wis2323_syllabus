library(tidyverse)
library(forcats)
bci<-read_csv("./class_sessions/07_patterns_of_biodiveristy_local/bci_trees_50.csv") %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum=cumsum(n)) %>% 
  mutate(code = fct_reorder(code, desc(n)))
  
ggplot(bci, aes(x=code, y=n)) + 
  geom_bar(stat = "identity")+
  theme_classic()
# birds_am %>% group_by(code) %>% tally() %>% filter(n>1)
birds_am<-read_csv("./class_sessions/07_patterns_of_biodiveristy_local/birds_am.csv") %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum=cumsum(n)) %>% 
  mutate(code = fct_reorder(code, desc(n)))

ggplot(birds_am, aes(x=code, y=n)) + 
  geom_bar(stat = "identity")+
  theme_classic()