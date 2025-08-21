library(tidyverse)
library(readxl)
file_path <- "~/Dropbox (UFL)/Teaching/IDS 2935 - Future of Rain Forests/2022_ids2935_content/submissions_2-1_narratives/"
# file_path %>% list.files()
file_names <- file_path %>%
  list.files() %>%
  .[str_detect(., ".xlsx")]

file_names <- paste(file_path,file_names,sep="")
file_names


l <- list.files(path = file_path,  
                       pattern = "*.xlsx", full.names = TRUE) %>% 
  lapply(read_xlsx, col_types="text",trim_ws=TRUE,col_names = c("text","words","score"),skip=1)

df<-  purrr::map_df(l, dplyr::bind_rows,.id = "id")

df<-df %>% 
  mutate(score=as.numeric(score)) %>% 
  mutate(text=gsub(".0","",text)) %>% 
  mutate(words=tolower(words)) %>% 
  mutate(text=tolower(text)) %>% 
  mutate(id=as.factor(id)) %>% 
  mutate(words=gsub("[^[:alnum:][:space:]']", "", words)) %>% 
  mutate(words=gsub("\r\n", " ", words)) %>% 
  mutate(words=gsub("item", "", words)) %>% 
  mutate(text=gsub("item","",text)) %>%
  mutate(text=trimws(text)) %>% 
  mutate(
    text=case_when(
      text == "six" ~ "6",
      text == "five" ~ "5",
      text == "four" ~ "4",
      text == "one" ~ "1",
      text == "three" ~ "3",
      text == "number 1 (paragraph 1)"~"1",
      text == "number 1 (paragraph 3)"~"1",
      text == "number 1 (paragraph 4)"~"1",
      text == "number 4 (paragraph 1)"~"4",
      text == "number 4 (paragraph 2)"~"4",
      text == "number 5 (paragraph 2)"~"5",
      text == "number 6 (paragraph 2)"~"6",
      TRUE ~ as.character(text))) %>% 
  mutate(text=as.factor(text)) 


mean_score<-df %>% 
  filter(id!=34) %>% 
  # filter(id!=22) %>% 
  filter(text!="") %>% 
  drop_na() %>% 
  group_by(text,id) %>% 
  summarize(sum = sum(score, na.rm=TRUE)) %>%
  group_by(text) %>% 
  summarize(avg = mean(sum, na.rm=TRUE)) 
mean_score


mean_n_words<-df %>% 
  # filter(id!=17) %>% 
  # filter(id!=22) %>% 
  drop_na() %>% 
  group_by(text,id) %>% 
  filter(text!="") %>% 
  summarize(count = n()) %>%
  group_by(text) %>% 
  summarize(avg = mean(count, na.rm=TRUE)) 
mean_n_words

library(tidytext)

polo<-c("Now let us resume our subject, and first I will tell you of the
kingdom of FERLEC. This kingdom, you must know, is so much frequented
by the Saracen merchants that they have converted the natives to the
Law of Mahommet---I mean the townspeople only, for the hill-people
live for all the world like beasts, and eat human flesh, as well as
all other kinds of flesh, clean or unclean. And they worship this,
that, and the other thing; for in fact the first thing that they see
on rising in the morning, that they do worship for the rest of the
day.Having told you of the kingdom of Ferlec, I will now tell of another
which is called BASMA. When you quit the kingdom of Ferlec you enter
upon that of Basma. This also is an independent kingdom, and the
people have a language of their own; but they are just like beasts
without laws or religion. They call themselves subjects of the Great
Kaan, but they pay him no tribute; indeed they are so far away that
his men could not go thither. Still all these Islanders declare
themselves to be his subjects, and sometimes they send him curiosities
as presents. There are wild elephants in the country, and numerous
unicorns, which are very nearly as big. They have hair like that of a
buffalo, feet like those of an elephant, and a horn in the middle of
the forehead, which is black and very thick. They do no mischief,
however, with the horn, but with the tongue alone; for this is covered
all over with long and strong prickles [and when savage with any one
they crush him under their knees and then rasp him with their tongue].
The head resembles that of a wild boar, and they carry it ever bent
towards the ground. They delight much to abide in mire and mud. 'Tis a
passing ugly beast to look upon, and is not in the least like that
which our stories tell of as being caught in the lap of a virgin; in
fact, 'tis altogether different from what we fancied. CHAPTER XII. CONCERNING THE ISLAND OF NECUVERAN: When you leave the
Island of Java (the less) and the kingdom of Lambri, you sail north
about 150 miles, and then you come to two Islands, one of which is
called NECUVERAN. In this Island they have no king nor chief, but live
like beasts. And I tell you they go all naked, both men and women, and
do not use the slightest covering of any kind. They are Idolaters.
Their woods are all of noble and valuable kinds of trees; such as Red
Sanders and Indian-nut and Cloves and Brazil and sundry other good
spices. There is nothing else worth relating; so we will go on, and I
will tell you of an Island called Angamanain.CHAPTER XIII. CONCERNING THE ISLAND OF ANGAMANAIN: Angamanain is a
very large Island. The people are without a king and are Idolaters,
and no better than wild beasts. And I assure you all the men of this
Island of Angamanain have heads like dogs, and teeth and eyes
likewise; in fact, in the face they are all just like big mastiff
dogs! They have a quantity of spices; but they are a most cruel
generation, and eat everybody that they can catch, if not of their own
race. They live on flesh and rice and milk, and have fruits different
from any of ours.")

raleigh<-c("The fourth river to the west of Caroli is Casnero: which falleth into
the Orenoque on this side of Amapaia. And that river is greater than
Danubius, or any of Europe: it riseth on the south of Guiana from the
mountains which divide Guiana from Amazons, and I think it to be
navigable many hundred miles. But we had no time, means, nor season of
the year, to search those rivers, for the causes aforesaid, the winter
being come upon us; although the winter and summer as touching cold
and heat differ not, neither do the trees ever sensibly lose their
leaves, but have always fruit either ripe or green, and most of them
both blossoms, leaves, ripe fruit, and green, at one time.There is no country which yieldeth more pleasure to the inhabitants, 
either for those common delights of hunting, hawking, fishing, fowling, 
and the rest, than Guiana doth; it hath so many plains, clear rivers, and 
abundance of pheasants, partridges, quails, rails, cranes, herons, and all other
fowl; deer of all sorts, porks, hares, lions, tigers, leopards, and
divers other sorts of beasts, either for chase or food. It hath a kind
of beast called cama or anta (tapir), as big as an English beef, and
in great plenty. To speak of the several sorts of every kind I fear
would be troublesome to the reader, and therefore I will omit them,
and conclude that both for health, good air, pleasure, and riches, I
am resolved it cannot be equalled by any region either in the east or
west. Moreover the country is so healthful, as of an hundred persons
and more, which lay without shift most sluttishly, and were every day
almost melted with heat in rowing and marching, and suddenly wet again
with great showers, and did eat of all sorts of corrupt fruits, and
made meals of fresh fish without seasoning, of tortugas, of lagartos
or crocodiles, and of all sorts good and bad, without either order or
measure, and besides lodged in the open air every night, we lost not
any one, nor had one ill-disposed to my knowledge; nor found any
calentura or other of those pestilent diseases which dwell in all hot
regions, and so near the equinoctial line.")

cook<-c("From what I have said of the Natives of New Holland^1^ they may appear to
some to be the most wretched People upon Earth; but in reality they
are far more happier than we Europeans, being wholy unacquainted not
only with the Superfluous, but with the necessary Conveniences so much
sought after in Europe; they are happy in not knowing the use of them.
They live in a Tranquility which is not disturbed by the Inequality of
Condition. The earth and Sea of their own accord furnishes them with
all things necessary for Life. They covet not Magnificient Houses,
Household-stuff, etc.; they live in a Warm and fine Climate, and enjoy
every wholesome Air, so that they have very little need of Cloathing;
and this they seem to be fully sencible of, for many to whom we gave
Cloth, etc., left it carelessly upon the Sea beach and in the Woods,
as a thing they had no manner of use for; in short, they seem'd to set
no Value upon anything we gave them, nor would they ever part with
anything of their own for any one Article we could offer them. This,
in my opinion, Argues that they think themselves provided with all the
necessarys of Life, and that they have no Superfluities.The native Australians may be happy in their condition, but they
are without doubt among the lowest of mankind. Confirmed cannibals,
they lose no opportunity of gratifying their love of human flesh.
Mothers will kill and eat their own children, and the women again are
often mercilessly illtreated by their lords and masters. There are no
chiefs, and the land is divided into sections, occupied by families,
who consider everything in their district as their own. Internecine
war exists between the different tribes, which are very small. Their
treachery, which is unsurpassed, is simply an outcome of their savage
ideas, and in their eyes is a form of independence which resents any
intrusion on THEIR land, THEIR wild animals, and THEIR rights
generally. In their untutored state they therefore consider that any
method of getting rid of the invader is proper. Both sexes, as Cook
observed, are absolutely nude, and lead a wandering life, with no
fixed abode, subsisting on roots, fruits, and such living things as
they can catch. Nevertheless, although treated by the coarser order of
colonists as wild beasts to be extirpated, those who have studied them
have formed favourable opinions of their intelligence. The more savage
side of their disposition being, however, so very apparent, it is not
astonishing that, brought into contact with white settlers, who
equally consider that they have a right to settle, the aborigines are
rapidly disappearing.")


text<-polo
# text<-raleigh
# text<-cook

text<-gsub("\n"," ", text)
text<-tolower(text)
text<-gsub("[^[:alnum:][:space:]']", " ", text)
text<-strsplit(text, " ")

text<-unlist(text) %>% 
  as_tibble() %>% 
  rename("word"="value") %>% 
  mutate(n=nchar(word)) %>% 
  filter(n>0) %>% 
  select(-n)


# install.packages("stopwords")

# stopwords<-stopwords()
# > stopwords<-as.data.frame(stopwords)

df<-text
library(stopwords)
words <- df %>% 
  anti_join(get_stopwords())
words

word_counts <- words %>%
  anti_join(stop_words, by = "word") %>%
  count(word, word, sort = TRUE)
word_counts

library(tidytext)
# All three of these lexicons are based on unigrams, i.e., single words. These 
# lexicons contain many English words and the words are assigned scores for 
# positive/negative sentiment, and also possibly emotions like joy, anger, 
# sadness, and so forth. The nrc lexicon categorizes words in a binary fashion 
# (“yes”/“no”) into categories of positive, negative, anger, anticipation, 
# disgust, fear, joy, sadness, surprise, and trust. The bing lexicon categorizes
# words in a binary fashion into positive and negative categories. The AFINN 
# lexicon assigns words with a score that runs between -5 and 5, with negative 
# scores indicating negative sentiment and positive scores 
# indicating positive sentiment.
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc") # Bigger file



# bing_codex --------------------------------------------------------------


# examples of word under different sentiment categories
bing_positive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

bing_negative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")



#  word counts from your selected text in different categories

positive_words <- word_counts %>% 
  inner_join(bing_positive) %>%
  arrange(desc(n))
positive_words

negative_words <- word_counts %>% 
  inner_join(bing_negative) %>%
  arrange(desc(n))
negative_words




# afinn codex -------------------------------------------------------------


afinn_positive <- get_sentiments("afinn") %>% 
  filter(value >0)

afinn_negative <- get_sentiments("afinn") %>% 
  filter(value <0)


positive_words_afinn <- word_counts %>% 
  inner_join(afinn_positive) %>%
  arrange(desc(n))
positive_words_afinn

negative_words_afinn <- word_counts %>% 
  inner_join(afinn_negative) %>%
  arrange(desc(n))
negative_words_afinn


# sentiment word cloud

# install.packages("reshape2")
# install.packages("wordcloud")
library(reshape2)
library(wordcloud)
lw_sentiment <- words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)




# marco polo --------------------------------------------------------------



library(gutenbergr)

lw <- gutenberg_download(10636)
lw2 <- gutenberg_download(12410)
lw<-bind_rows(lw,lw2)
lw$line <- seq(1:nrow(lw))

words <- lw %>%
  unnest_tokens(word, text)

library(stopwords)
words <- words %>% 
  anti_join(get_stopwords())
words

word_counts <- words %>%
  anti_join(stop_words, by = "word") %>%
  count(word, word, sort = TRUE)
word_counts

bing_positive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

bing_negative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

#  word counts from your selected text in different categories
positive_words <- word_counts %>% 
  inner_join(bing_positive) %>%
  arrange(desc(n))
positive_words

negative_words <- word_counts %>% 
  inner_join(bing_negative) %>%
  arrange(desc(n))
negative_words


# sentiment word cloud

# install.packages("reshape2")
# install.packages("wordcloud")
library(reshape2)
library(wordcloud)
lw_sentiment <- words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


