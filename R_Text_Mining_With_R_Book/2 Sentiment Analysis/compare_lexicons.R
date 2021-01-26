library(data.table)
library(janeaustenr)
library(syuzhet)
library(textdata)
library(tidytext)
library(RtextminerPkg)
library(RplotterPkg)

# Define the 3 sources of lexicon words from afinn, bing, nrc
afinn_dt <- data.table::setDT(textdata::lexicon_afinn())
str(afinn_dt)

bing_dt <- data.table::setDT(tidytext::sentiments)
str(bing_dt)

nrc_dt <- data.table::setDT(syuzhet::get_sentiment_dictionary("nrc", language = "english"))
str(nrc_dt)

# Download Jane Austen's books and focus on her Pride & Prejudice (pp)
jane_austen_books_dt <- data.table::setDT(janeaustenr::austen_books())
pp_dt <- jane_austen_books_dt[book == "Pride & Prejudice"]
str(pp_dt)
# There are 13030 lines of text

# Add a "linenumber" variable to the rows of pp_dt's text
pp_dt[, linenumber := .I]
str(pp_dt)

# Tokenize the words from pp using stopwords
pp_words_lst <- RtextminerPkg::tokenize_text(
  x = pp_dt,
  input_col = "text",
  output_col = "word",
  stopwords = tidytext::stop_words$word,
  strip_punct = TRUE
)
pp_words_dt <- pp_words_lst$tokens_dt
str(pp_words_dt)
# There are 37246 words in pp with removing stop words

# Add an "index" variable to pp_words_dt for every 80 words
pp_words_dt <- pp_words_dt[, index := (linenumber %/% 80)]
print(pp_words_dt)

# -----------------------------afinn lexicon study----------------------------------------------
# Inner join pp's words with the afinn lexicon words by variable "word"
afinn_pp_dt <- RtextminerPkg::join_words(pp_words_dt, afinn_dt)
str(afinn_pp_dt)
# There are now 6065 common words between pp and the afinn lexicon that have a "value"

# Add a "sentiment" variable to afinn_pp_dt that sums the "value" variable grouped by "index"
afinn_pp_dt <- afinn_pp_dt[, sentiment := sum(value), by = index][order(index)]
str(afinn_pp_dt)
# View afinn_pp_dt ordered by "index" to get a handle on "index" vs "sentiment"
# Note that each unique "index" has a "sentiment" value
unique_indexes <- unique(afinn_pp_dt$index)
# There are 163 unique indexes with "sentiment" values

# Plot the 6065 "index" x "sentiment" values
pp_afinn_plot <- RplotterPkg::create_stick_plot(
  df = afinn_pp_dt,
  aes_x = "index",
  aes_y = "sentiment",
  title = "Afinn Sentiment Values Across Index Locations of Pride & Prejudice",
  rot_y_tic_label = T
)
pp_afinn_plot

# -----------------------------------bing lexicon study---------------------------------------
# Inner join pp's words with the bing lexicon words by variable "word"
bing_pp_dt <- RtextminerPkg::join_words(pp_words_dt, bing_dt)
str(bing_pp_dt)
# Note the "sentiment" variable is binary with character values "positive" and "negative"
# There are 7521 common words between the bing lexicon and pp that have a "sentiment" value

# Group bing_pp_dt by "index" and "sentiment" and get the counts for "positive" and "negative" values
bing_pp_dt <- bing_pp_dt[, N := .N, by = .(index,sentiment)][order(index)]
str(bing_pp_dt)
# View bing_pp_dt where for example at "index" = 0, we have 15 positives and 7 negatives

# Reshape bing_pp_dt to make values of "N" on the same row under columns of "sentiment" values
bing_pp_dt <- data.table::dcast(bing_pp_dt, index ~ sentiment, value.var = "N")
print(bing_pp_dt)
# We are now showing the counts for "positive" and "negative" for each "index" value
# Again, "index" = 0 has 15 positive words and 7 negative words

# Add a "sentiment" variable to bing_pp_dt defined as the difference in occurrence between "positive" and "negative"
bing_pp_dt <- bing_pp_dt[, sentiment := (positive - negative)]
print(bing_pp_dt)

# Plot the 163 "index" values versus "sentiment" for the bing lexicon
pp_bing_plot <- RplotterPkg::create_stick_plot(
  df = bing_pp_dt,
  aes_x = "index",
  aes_y = "sentiment",
  title = "Bing Sentiment Values Across Index Locations of Pride & Prejudice",
  rot_y_tic_label = T
)
pp_bing_plot

# ----------------------------------------nrc lexicon study--------------------------------------
# Inner join pp's words with the nrc lexicon words by variable "word"
nrc_pp_dt <- RtextminerPkg::join_words(pp_words_dt, nrc_dt)
str(nrc_pp_dt)

# Note that there are 10 different values for "sentiment"
unique_nrc_sentiment <- unique(nrc_pp_dt$sentiment)
unique_nrc_sentiment

# Note that among the "sentiment" values, there is 'negative' and 'positive'. Filter out words with just these values
nrc_pp_dt <- nrc_pp_dt[sentiment %in% c("positive", "negative")]
str(nrc_pp_dt)
# We have dropped down from 26814 words to 10333 words

# Group nrc_pp_dt by "index" and "sentiment" and get the counts for "positive" and "negative" values
nrc_pp_dt <- nrc_pp_dt[, N := .N, by = .(index,sentiment)][order(index)]
str(nrc_pp_dt)
# View nrc_pp_dt where for example at "index" = 0, we have 28 positives and 10 negatives

# Reshape nrc_pp_dt to make values of "N" on the same row under columns of "sentiment" values
nrc_pp_dt <- data.table::dcast(nrc_pp_dt, index ~ sentiment, value.var = "N")
print(nrc_pp_dt)
# We are now showing the counts for "positive" and "negative" for each "index" value
# Again, "index" = 0 has 28 positive words and 10 negative words

# Add a "sentiment" variable to nrc_pp_dt defined as the difference in occurrence between "positive" and "negative"
nrc_pp_dt <- nrc_pp_dt[, sentiment := (positive - negative)]
print(nrc_pp_dt)

# Plot the 163 "index" values versus "sentiment" for the nrc lexicon
pp_nrc_plot <- RplotterPkg::create_stick_plot(
  df = nrc_pp_dt,
  aes_x = "index",
  aes_y = "sentiment",
  title = "NRC Sentiment Values Across Index Locations of Pride & Prejudice",
  rot_y_tic_label = T
)
pp_nrc_plot

# For comparison place all three plots on the same page
layout <- list(
  plots = list(pp_afinn_plot, pp_bing_plot, pp_nrc_plot),
  rows = c(1, 2, 3),
  cols = c(1, 1, 1)
)
RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(10),
  row_heights = c(3, 3, 3),
  title = "Lexicon Comparisons: afinn, bing, nrc"
)
