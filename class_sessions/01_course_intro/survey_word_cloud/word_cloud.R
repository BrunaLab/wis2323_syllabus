

# install the required packages
# install.packages("tm")		 # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator
# install.packages("RColorBrewer") # color palettes

# load the packages
library("tm")
library("tidytext")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tidyverse")
library(stopwords)



# Quick Examples

# Read text file

# tutorial  ---------------------------------------------------------------

# For more info on word-clouds see [this tutorial](https://www.geeksforgeeks.org/generating-word-cloud-in-r-programming/)


# load submissions --------------------------------------------------------



words_2021<-read.table("./class_materials/class_sessions/01_course_intro/survey_word_cloud/words_2021.txt",sep=' ') %>% 
  pivot_longer(where(is.character),
               names_to = "word",) %>% 
  mutate(year=2021) %>% 
  select(year,value) %>% 
  select(-word) %>% 
  rename(word=value) %>% 
  mutate(word=trimws(word)) %>% 
  mutate(words=tolower(words)) 
  

words_2022<-read_csv("./class_materials/class_sessions/01_course_intro/survey_word_cloud/words_2022.csv") %>% 
  mutate(year=2022) %>% 
  relocate(year,.before=1) %>% 
  mutate(words=tolower(words)) %>% 
  select(-word) %>% 
  rename(word=value) %>% 
  mutate(word=trimws(word)) %>% 
  mutate(words=tolower(words)) 

words_2023_1<-read_csv("./class_materials/class_sessions/01_course_intro/survey_word_cloud/words_2023_wk1.csv") %>% 
  mutate(year=2023) %>% 
  relocate(year,.before=1) %>% 
  mutate(words=tolower(words)) %>% 
  rename(word=words) %>% 
  mutate(word=trimws(word)) %>% 
  # mutate(word=tolower(word)) %>% 
  mutate(week="week 1") %>% 
  relocate(week,.after=1)

words_2023_16<-read_csv("./class_materials/class_sessions/01_course_intro/survey_word_cloud/words_2023_wk16.csv") %>% 
  mutate(year=2023) %>% 
  relocate(year,.before=1) %>% 
  mutate(words=tolower(words)) %>% 
  # select(-word) %>% 
  rename(word=words) %>% 
  mutate(word=trimws(word)) %>% 
  # mutate(words=tolower(word)) %>% 
  mutate(week="week 16") %>% 
  relocate(week,.after=1)


# words <- bind_rows(words_2021,words_2022,words_2023) %>% 
words <- bind_rows(words_2023_1,words_2023_16) %>% 
  mutate(word=gsub(".", "", word, fixed = TRUE)) %>% 
  # mutate(word=gsub("\\,", "", word, fixed = TRUE)) %>% 
  mutate(word=gsub("south/central america", "central america, south america", word, fixed = TRUE)) %>% 
  mutate(word=gsub("rain/humidity", "rain, humidity", word, fixed = TRUE)) %>% 
  mutate(word=gsub("unique animals/plants", "unique animals, unique plants", word, fixed = TRUE)) %>% 
  mutate(word=gsub("green/fresh", "green, fresh", word, fixed = TRUE)) %>% 
  separate(word,c(letters[seq(1:8)]),sep=",") %>% 
  pivot_longer(cols = "a":"h",
               names_to = "index",) %>% 
  # mutate(word=gsub('\\', "", word, fixed = TRUE)) %>% 
  rename(word=value) %>% 
  arrange(word) %>% 
  drop_na() %>% 
  mutate(word=as.factor(word)) %>% 
  mutate_all(trimws) %>% 
  mutate(word = case_when(
    word == "amazone" ~ "amazon",
    word == "amazone" ~ "amazon",
    word == "the amazon" ~ "amazon",
    word == "forg" ~ "frogs",
    word == "poison dart frog" ~ "poison dart frogs",
    word == "humid\\" ~ "humid",
    word == "rainy\\" ~ "rain",
    word == "lush\\" ~ "lush",
    word == "lushes" ~ "lush",
    word == "and wet" ~ "wet",
    word == "earth lungs" ~ "lungs of the world",
    word == "and wet" ~ "wet",
    word == "a lot of rain" ~ "rain",
    word == "raindrops" ~ "rain",
    word == "constant rain" ~ "rain",
    word == "high rainfall" ~ "rain",
    word == "rainfall" ~ "rain",
    word == "heavy rains" ~ "rain",
    word == "rainy" ~ "rain",
    word == "animal life" ~ "animals",
    word == "beauty" ~ "beautiful",
    word == "biodiverse" ~ "(bio)diversity",
    word == "forests" ~ "forest",
    word == "diverse" ~ "(bio)diversity",
    word == "diversity" ~ "(bio)diversity",
    word == "biodiversity" ~ "(bio)diversity",
    word == "heat" ~ "hot",
    word == "humidity" ~ "humid",
    word == "lots of noises" ~ "noisy",
    word == "plant life" ~ "plant life",
    word == "monkey" ~ "monkeys",
    word == "monkeys swinging from trees" ~ "monkeys",
    word == "places of high diversity" ~ "(bio)diversity",
    word == "rio- the movie" ~ "the movie 'rio'",
    word == "tall trees and plants" ~ "tall trees",
    word == "bird of paradise" ~ "birds of paradise",
    word == "tree" ~ "trees",
    word == "treefrogs" ~ "tree frogs",
    word == "vital to ecosystems" ~ "vital",
    word == "waterfall" ~ "waterfalls",
    word == "toucan" ~ "toucans",
    TRUE ~ word)) 
  

wordcloud_1 <- words %>% 
  filter(week=="week 1") %>% 
  mutate(word=str_to_title(word)) %>% 
  mutate(word=gsub(" ", "", word, fixed = TRUE)) 

wordcloud_16 <- words %>% 
  filter(week=="week 16") %>% 
  mutate(word=str_to_title(word)) %>% 
  mutate(word=gsub(" ", "", word, fixed = TRUE)) 


wordcloud <- words %>% 
  mutate(word=str_to_title(word)) %>% 
  mutate(word=gsub(" ", "", word, fixed = TRUE)) 
  

# count of unique words
wordcloud %>% summarize(n_distinct(word))

wordcloud_1 %>% summarize(n_distinct(word))
wordcloud_16 %>% summarize(n_distinct(word))

# text<-tolower(wordcloud$word)
text1<-wordcloud_1$word
text16<-wordcloud_16$word
docs1 = Corpus(VectorSource(text1))   
docs16 = Corpus(VectorSource(text16))   




# tibble of unique words
as_tibble(unique(wordcloud$word))

# text<-tolower(wordcloud$word)
text<-wordcloud$word
docs = Corpus(VectorSource(text))   

# Convert the text to lower case
# docs = tm_map(docs, 
#               content_transformer(tolower))
# 

# Remove numbers
# docs = tm_map(docs, removeNumbers)


wordcloud_generator <- function(docs, title) {
  # docs<-docs1
  # Remove white spaces
    docs = tm_map(docs, stripWhitespace)
  
  
  
  dtm = TermDocumentMatrix(docs, control=list(removePunctuation=T, tolower=F, stopwords=T))
  
  m = as.matrix(dtm)
  v = sort(rowSums(m), decreasing = TRUE)
  d = data.frame(word = names(v), freq = v)
  head(d, 10)
  
  
  document_tm_clean <- removeSparseTerms(dtm, 0.8)
  document_tm_clean_mat <- as.matrix(document_tm_clean)
  
  
  
  # dev.new(width = 1000, height = 1000, unit = "px")
  
  set.seed(1234)
  
  
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  
  
  wc_fig<-wordcloud(words = d$word,
                    freq = d$freq,
                    min.freq = 1,
                    max.words = 190, # Set top n words
                    random.order = FALSE, # Words in decreasing freq
                    random.color=FALSE,
                    # rot.per = 0.35,
                    # scale=c(3,1),  # Set min and max scale
                    rot.per = 0, # % of vertical words
                    fixed.asp = T,
                    # use.r.layout=TRUE, # Use C++ collision detection
                    colors = brewer.pal(8, "BrBG"))
  
  
  
  return(wc_fig)
  
  
}


fig1<-wordcloud_generator(docs1, "week 1")
fig16<-wordcloud_generator(docs16, "week 16")
fig
# dev.off()