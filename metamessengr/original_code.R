#TOBY IS GETTING FORKED!

### Script to analyze messenger data ###
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("purrr","jsonlite","dplyr","tidytext","tidyr","textdata","stringr",
      "ggplot2","scales","chron","lexicon","ggraph","igraph")

## Set wd
setwd("C:/Users/meowy/OneDrive/Documents/R/Projects/Messenger Analysis/Olivia")

## Writing a function for just selection
selection <- function(inp=NULL) {


  ## This looks for all of the files that start with "message_" and are .json
  ## format. It then reads each file in using fromJSON() and assigns to inp
  inp = lapply(list.files(recursive = T, pattern = "\\.json$"), fromJSON)

  ## This then takes the inp and finds how many files it read and assigns it l
  l = as.numeric(length(map(inp,2)))

  ## s is called as a sequence from 1 to the previous length (l)
  s = seq(from = 1, to = l)

  ## Setting f to NULL
  f = NULL
  ## For i in the number of files loaded, select the timestamp, sender_name, and
  ## content of the message. It then makes f = to that selection if it's the
  ## first file's data, or it row binds the data onto it if it's anything else.
  for (i in s) {
    if("content" %in% names(inp[[i]][[2]])){

    t = select(map(inp,2)[[i]], timestamp_ms, sender_name, content)
    if (i == 1){
      f=t
    } else {
      f = rbind(f, t)
    }
    }
    print(i)
  }

  ## Then f, the smaller dataset, is bound to inp and is the output
  inp = f
}

n = selection()

## Convert n to a data frame
n = as.data.frame(n)

## Flattenn the nested dataframe
n = flatten(n)

## Turn the timestamp into the format that R likes (not miliseconds)
n$timestamp_ms = n$timestamp_ms/1000

## Have it convert the timestamp to POSIXct
n$timestamp_ms <- as.POSIXct(n$timestamp_ms, origin="1970-01-01", tz="America/Los_Angeles")

## Make a new row, "date" and have it read in the POSIXct as date
n$date = as.Date(as.character(n$timestamp_ms))

## Read in the timestamp time as an actual time (this separates the date and time)
n$time = strftime(n$timestamp_ms, format="%H:%M:%S")

## Further time cleaning
n$time = chron(times=n$time)
n$time = as.numeric(n$time)

## New column with sending hour
n$hour = n$time*24

## New column with the sender's name
n$sender = n$sender_name

## new column with the length of each message sent
n$length=nchar(n$content)

## Messenger only allows messages of 640 characters or less. Setting anything
## above that to NA as anything else is bogus
n$length[which(n$length>640)] = NA



############# Sentiment Analysis ############

## Getting just one person's messages
alex = as.data.frame(n$content[which(n$sender == "Alexander James Robertson")])
### Renaming the row name
    ### Do I need this?
  names(alex)[names(alex) == 'n$content[which(n$sender == "Alexander James Robertson")]'] <- "content"

    ### Needed to change to a tibble
  text_df <- as.vector(as.character(alex))
  alex_t=tibble(text=text_df)


### Adding whole dictionary
data("grady_augmented")

### Turning it into a tibble that works
grady_augmented=as_tibble(grady_augmented)
grady_augmented=rename(grady_augmented, word=value)

## Removing some custom stopwords
custom_stop_words <- bind_rows(tibble(word = c("na"),
                                      lexicon = c("custom")),
                               stop_words)

### All words in one column then removing stop words and only keeping words in the dictionary
tidy_text <- alex_t %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  semi_join(grady_augmented)


### Graphing the words
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 900) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## Doing the same thing above for Olivia

oliv = as.data.frame(n$content[which(n$sender == "Olivia Wang")])
names(oliv)[names(oliv) == 'n$content[which(n$sender == "Olivia Wang")]'] <- "content"

## Needed to change to a tibble
text2_df <- as.vector(as.character(oliv))
oliv_t=tibble(text=text2_df)

## All words in one column then removing stop words and only keeping words in the dictionary
tidy_text2 <- oliv_t %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  semi_join(grady_augmented)

### Graphing the words
tidy_text2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## Comparing the word frequencies between texts
#### Creating a new DF with author names attached. Noting the frequency


tidy_test=mutate(tidy_text, author = "Alexander") %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(word) %>%
  mutate("proportion" = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(proportion) %>%
  gather(proportion)

tidy_text2=mutate(tidy_text2, author = "Olivia")

frequency <- bind_cols(mutate(tidy_text, author = "Alexander"),
                       mutate(tidy_text2, author = "Olivia"), by=word) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(author, proportion) %>%
  gather(author, proportion, `Alexander`:`Olivia`)
frequency=na.omit(frequency)

## Plotting
ggplot(frequency, aes(x=proportion, y=proportion)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "Olivia", x = "Alexander")



### Bi-gram analysis
### For me - Note: Change "alex_t" to "oliv_t" for Olivia messages
bigrams <- alex_t %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

## What do they look like?
bigrams %>%
  count(bigram, sort = TRUE)

## Separating and cleaning the words to remove stop words and only keep dictionary words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word) %>%
  filter(word1 %in% grady_augmented$word) %>%
  filter(word2 %in% grady_augmented$word)

## New bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

## Filtering to only bigrams with more than 15 uses
bigram_graph <- bigram_counts %>%
  filter(n > 15) %>%
  graph_from_data_frame()

bigram_graph

## Graphing the bigrams network

#### Idea: Color by the percentage sent by one or the other. Normalize by the total number of that
  #### n-gram sent and then go from there.

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
set.seed(2020)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



## Basic bar plot for number of messages sent by date
p <- ggplot(n, aes(x=date))
p + geom_bar()

## Bar plot with coloring for who sent what
q <- ggplot(n, aes(x=date, fill = sender))
q + geom_bar()

## Looking at the percent sent per day by person
q <- ggplot(n, aes(x=date, fill = sender))
q + geom_bar(position = "fill")

## Boxplot of the length of messages
g = ggplot(n, aes(x=sender, y=length, color=sender))
g + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

## Looking at the percent of words sent per day by person
q <- ggplot(n, aes(x=date, y =length, fill = sender))
q + geom_bar(stat = "identity")
## And using fill
q <- ggplot(n, aes(x=date, y =length, fill = sender))
q + geom_bar(stat = "identity", position="fill")

## Scatter of messages sent by time
h <- ggplot(n, aes(x=date, y=hour, color=sender))
h + geom_point(size=1) +
  scale_y_continuous(limits = c(0, 24), breaks = (0:12)*2)

## Scatter of messages sent by time including length
h <- ggplot(n, aes(x=date, y=hour, color=sender, size = length))
h + geom_point() +
  scale_y_continuous(limits = c(0, 24), breaks = (0:12)*2) +
  scale_size_continuous(range=c(0.5,4))








