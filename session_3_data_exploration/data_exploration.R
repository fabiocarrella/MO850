library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(ggwordcloud)
library(seededlda)
library(kableExtra)

# import and inspect dataset (#4)
raw_data <- read_csv("session_3_data_exploration/data/timebin_sampled_telegram.csv")
View(raw_data)
head(raw_data)
glimpse(raw_data)
str(raw_data)
summary(raw_data)

# basic cleaning
filtered_data <- raw_data %>% 
  filter(!is.na(text_content))

filtered_data <- filtered_data %>% 
  mutate(text_WC = str_count(text_content, "\\S+")) %>% 
  filter(text_WC > 5)


# introduce Quanteda (#6)
telegram_corpus <- corpus(filtered_data, 
                          docid_field = "message_id", 
                          text_field = "text_content")
telegram_corpus

# tokenization
toks <- tokens(telegram_corpus, 
               remove_punct = T,
               remove_url = T,
               remove_numbers = T,
               remove_symbols = T
               )

#toks <- toks %>% 
#  tokens_select(min_nchar = 3)

print(toks)

toks %>% 
  tokens_wordstem()

toks %>%  
  tokens_replace(pattern = lexicon::hash_lemmas$token, 
                 replacement = lexicon::hash_lemmas$lemma)

toks[1:5]  %>%  
  tokens_ngrams(n = c(2, 3))

# Document-feature matrix (DFM)
dfm <- dfm(toks)
dfm


# raw frequency (#7)
dfm %>% 
  textstat_frequency() %>% 
  head(20) # check tail too

# token normalization (min. freq. vs stopwords) (#8)
trimmed_dfm <- dfm %>% 
  dfm_trim(min_docfreq = 0.001, 
           max_docfreq = 0.90, 
           docfreq_type = "prop")

trimmed_dfm %>% 
  textstat_frequency() %>% 
  head(20)

stop_dfm <- dfm %>% 
  dfm_remove(stopwords("pt")) %>%
  dfm_trim(min_docfreq = 0.001, 
           docfreq_type = "prop")

stop_dfm %>% 
  textstat_frequency() %>% 
  head(20)

# custom stopwords
custom_stops <- c(stopwords("pt"), 
                  stopwords("en"), 
                  stopwords("it"), 
                  "channel_hash", 
                  "t.me")

stop_dfm <- dfm %>% 
  dfm_remove(pattern = custom_stops) %>%
  dfm_trim(min_docfreq = 0.001, 
           docfreq_type = "prop")

stop_dfm %>% 
  textstat_frequency() %>% 
  head(20)

# check stopwords (compare snowball vs. stopwords-iso)
stopwords::stopwords("pt")
stopwords::data_stopwords_stopwordsiso$pt


# more tokenization (#9-11)
stop_dfm %>% 
  dfm_weight(scheme = "boolean")

stop_dfm %>% 
  #dfm_smooth(smoothing = 1) %>% 
  dfm_weight(scheme = "prop")

tfidf_dfm <- stop_dfm %>% 
  dfm_tfidf()

tfidf_dfm %>%
  textstat_frequency(n = 3, 
                     groups = docvars(tfidf_dfm, "channel_id"), 
                     force = TRUE) %>%
  arrange(group, rank)


# group comparison (#12)
stop_dfm %>% 
  textstat_frequency(n = 10, 
                     groups = docvars(stop_dfm, "is_vaccine_related"), 
                     force = TRUE) %>%
  arrange(group, rank)

freq_comp <- stop_dfm %>% 
  textstat_frequency(groups = docvars(stop_dfm, "is_vaccine_related"))

vax_novax <- freq_comp %>% 
  pivot_wider(id_cols = "feature", 
              names_from = "group", 
              values_from = "frequency",
              names_prefix = "count_") %>% 
  mutate(freq_novax = count_0/sum(count_0, na.rm = TRUE),
         freq_vax = count_1/sum(count_1, na.rm = TRUE),
         vax_freq_ratio = freq_vax/freq_novax)
  
table(docvars(stop_dfm, "is_vaccine_related"))

vax_keyness <-  stop_dfm %>%
  textstat_keyness(
    target = docvars(stop_dfm, "is_vaccine_related") == 1 & 
      !is.na(docvars(stop_dfm, "is_vaccine_related")),
    measure = "lr")

head(vax_keyness, 10)
tail(vax_keyness, 10)

vax_keyness %>% 
  quanteda.textplots::textplot_keyness()

vax_keyness  %>%  
  # only words with significant difference to p < .001
  filter(p < .001) %>% 
  arrange(desc(abs(G2))) %>% 
  filter(row_number() <= 50 | row_number() >= n() - 49) %>%
  # plot
  ggplot(aes(label = feature, 
             size = abs(G2), 
             color = G2 > 0,
             angle_group = G2 > 0)) +
  geom_text_wordcloud_area(eccentricity = 1) + 
  scale_size_area(max_size = 30, guide = "none") +
  scale_color_discrete(
    name = "",
    breaks = c(FALSE, TRUE)
  ) +
  theme_void()

# continuous variables
fwd_scores <- docvars(telegram_corpus)$n_forwards

fwd_cor <- stop_dfm %>% 
  convert(to = "data.frame") %>%
  summarise(
    across(
      -doc_id, 
      ~ cor(.x, fwd_scores, 
            method = "spearman", 
            use = "complete.obs")
    )
  ) %>%
  pivot_longer(everything(), 
               names_to = "feature", 
               values_to = "cor")

fwd_cor %>% arrange(desc(cor)) %>% tail(10)


# topic modeling with LDA (#13)
lda_model <- textmodel_lda(stop_dfm, k = 10)

terms(lda_model, 10)

head(lda_model$theta)

# connect docs to topics
topic_labels <- apply(terms(lda_model, 5), 2, paste, collapse = "_")
filtered_data <- docvars(lda_model$data)
filtered_data$topic <- topics(lda_model)
filtered_data$topic_label <- topic_labels[filtered_data$topic]

filtered_data %>%
  select(topic_label, text_content) %>% 
  head(5) %>%
  kable() %>%
  kable_styling() %>%
  column_spec(2, width = "30em")
