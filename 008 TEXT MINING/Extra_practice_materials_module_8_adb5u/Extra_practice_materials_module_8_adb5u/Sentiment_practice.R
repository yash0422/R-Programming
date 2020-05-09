
x = readLines("Dell_earning_ call.txt")

#There are 3 inbuilt sentiment dictionaries as of now in tidytext with a fourth under development.

require(tidytext)
library(tidytext)
require(tidyr)
require(dplyr)
install.packages('textdata')
library(textdata)

#The tidytext package contains all three sentiment lexicons in the sentiments dataset.
#Bing, Afinn and NRC

#The nrc lexicon categorizes words in a binary fashion ("yes"/"no") into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. 
#The bing lexicon categorizes words in a binary fashion into positive and negative categories. 
#The AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment. All of this information is tabulated in the sentiments dataset, and tidytext provides a function get_sentiments() to get specific sentiment lexicons without the columns that are not used in that lexicon.

textdf = data_frame(text = x)   # convert to data frame

bing = get_sentiments("bing")   # put all of the bing sentiment dict into object 'bing'
bing     # view bing object

#Which docs are most positive and negative in the corpus?
senti.bing = textdf %>%
  mutate(linenumber = row_number()) %>%   # build line num variable
  ungroup() %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, index = linenumber %/% 1, sort = FALSE) %>%
  mutate(method = "bing")    # creates a column with method name

senti.bing

#Now let's see the distribution of positive and negative sentiment within documents across the corpus.

#Note use of the spread() function to combine extra row pertaining to some index (doc) and make an extra column. Do ?tidyr::spread for more info.

bing_df = data.frame(senti.bing %>% spread(sentiment, n, fill = 0))

head(bing_df)

#Can we combine the negative and positive rows, by say, subtracting negative from poisitive score and thereby computing some polarity score for each line? Yes, see code below.

bing_pol = bing_df %>% 
  mutate(polarity = (positive - negative)) %>%   #create variable polarity = pos - neg
  arrange(desc(polarity), index)    # sort by polarity

bing_pol %>%  head()

require(ggplot2)

# plotting running sentiment distribution across the analyst call
ggplot(bing_pol, 
       aes(index, polarity)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Sentiment in dell earning call",
       x = "doc",  
       y = "Sentiment")


bing_word_counts <- textdf %>%
  unnest_tokens(word, text) %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  filter(n > 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")  

require(wordcloud)

# build wordcloud of commonest tokens
textdf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))

#AFINN Sentiment lexicon

# get AFINN first
AFINN <- get_sentiments("afinn")
AFINN

# inner join AFINN words and scores with text tokens from corpus
senti.afinn = textdf %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  inner_join(AFINN) %>%    # returns only intersection of wordlists and all columns
  group_by(index = linenumber %/% 1) %>% 
  mutate(method = "afinn")

senti.afinn

data.frame(senti.afinn) %>% head()

# first construct and split bigrams into word1 and word2
dell_bigrams_separated <- textdf %>%
  unnest_tokens(bigram, text, 
                token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dell_bigrams_separated

# examine the most frequent bigrams whose first word is a sentiment word
senti_bigrams <- dell_bigrams_separated %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%     # word1 is from bigrams and word from AFINN
  ungroup()
senti_bigrams

# what if we want sentiment associated with proper words, not stopwords?
senti_bigrams_filtered = dell_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%     # word1 is from bigrams and word from AFINN
  ungroup()

senti_bigrams_filtered

#Sentiments with NRC

# view nrc dict structure
nrc = get_sentiments("nrc")
nrc

senti.nrc = textdf %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, index = linenumber %/% 1, sort = FALSE) %>%  # %/% gives quotient
  mutate(method = "nrc")

senti.nrc

# make a neat table out of the 8 emotion dimensions
a = data.frame(senti.nrc %>% spread(sentiment, n, fill = 0))
head(a)

dell_anger = textdf %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc) %>%
  filter(sentiment == "joy") %>%
  count(word, sort = TRUE)

dell_anger

