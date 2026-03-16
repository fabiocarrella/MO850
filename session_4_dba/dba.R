library(tidyverse)
library(glmmTMB)
library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(quanteda.dictionaries)
library(rdflib)

# import dictionaries
nrc_pt <- read_tsv("session_4_dba/data/NRC-Emotion-Lexicon/OneFilePerLanguage/Portuguese-NRC-EmoLex.txt") %>%
  rename(word = `Portuguese Word`)

nrc_emotions <- c(
  "anger", "anticipation", "disgust", "fear", "joy",
  "negative", "positive", "sadness", "surprise", "trust"
)

nrc_pt_dict <- dictionary(
  lapply(setNames(nrc_emotions, nrc_emotions), function(e) {
    nrc_pt$word[nrc_pt[[e]] == 1]
  })
)

hash_nrc <- read_tsv("session_4_dba/data/NRC-Hashtag-Emotion-Lexicon-v0.2/NRC-Hashtag-Emotion-Lexicon-v0.2.txt",
  col_names = c("emotion", "word", "score")
)

oplex <- lexiconPT::oplexicon_v3.0

liwc <- read_csv("session_4_dba/data/LIWC2015 Dictionary - Brazilian Portuguese.csv") %>%
  select(-c(
    `function`, affect, anx:sad, family:male, insight:differ, see:feel, body:ingest,
    affiliation:risk, swear:filler, ppron, pronoun, article, auxverb
  )) %>%
  pivot_longer(-DicTerm, names_to = "category", values_to = "present") %>%
  filter(present == "X") %>%
  group_by(category) %>%
  summarise(terms = list(unique(DicTerm)), .groups = "drop") %>%
  deframe() %>%
  dictionary()

# import data
raw_data <- read_csv("session_4_dba/data/fakeWhatsApp.BR_2018_content_only.csv") %>%
  mutate(
    doc_id = row_number(),
    misinformation = as.factor(ifelse(misinformation == 1,
      "fake",
      "real"
    ))
  )

# quanteda pipeline
corpus <- corpus(raw_data,
  docid_field = "doc_id",
  text_field = "text"
)

toks <- tokens(corpus,
  remove_punct = T,
  remove_symbols = T,
  remove_url = T,
  remove_numbers = T
)

dfm <- dfm(toks)

# dictionary lookup
dfm %>%
  dfm_lookup(nrc_pt_dict)

# merging frequencies with docs
nrc_df <- dfm %>%
  dfm_lookup(nrc_pt_dict) %>%
  convert("data.frame") %>%
  right_join(
    corpus %>%
      convert("data.frame")
  ) %>%
  mutate(wc = ntoken(dfm)) %>%
  filter(wc > 0)

# pivoting the dataframe for plotting
nrc_long <- nrc_df %>%
  pivot_longer(
    cols = all_of(nrc_emotions),
    names_to = "emotion",
    values_to = "count"
  ) %>%
  mutate(count_norm = count / wc)


# differences in joy across real/fake messages
nrc_long %>%
  filter(emotion == "joy") %>%
  group_by(emotion, misinformation) %>%
  summarise(mean_norm = mean(count_norm, na.rm = TRUE)) %>% # check "count"
  ggplot(aes(x = emotion, y = mean_norm, fill = misinformation)) +
  geom_col(position = "dodge") +
  theme_bw() +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "", y = "Mean joy %", fill = "")


# checking correlation with shares
ggplot(
  data = nrc_df,
  aes(
    shares, joy,
    #         color = misinformation
  )
) +
  geom_jitter() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +
  # facet_wrap(~ misinformation) +
  theme_bw() +
  theme(
    text = element_text(size = 18),
    legend.position = "bottom"
  )

cor(nrc_df$shares, nrc_df$joy)


# issues with dictionaries
  m1 <- glmmTMB(joy ~ misinformation,
  offset = log(wc),
  data = nrc_df,
  family = nbinom2()
)
summary(m1)

m2 <- glmmTMB(shares ~ joy + wc,
  data = nrc_df,
  family = nbinom2()
)
summary(m2)

set.seed(30)

sample(nrc_pt_dict$joy, 20)


# polarity
pos_neg_nrc <- nrc_pt_dict[c("positive", "negative")]
polarity(pos_neg_nrc) <- list(pos = "positive", neg = "negative")

sentiment_df <- textstat_polarity(dfm, pos_neg_nrc) %>%
  rename(nrc_polarity = sentiment) %>%
  right_join(nrc_df)


# change dictionary
pos_neg_oplex <- dictionary(list(
  positive = oplex$term[oplex$polarity == 1],
  negative = oplex$term[oplex$polarity == -1]
))
polarity(pos_neg_oplex) <- list(pos = "positive", neg = "negative")

sentiment_df <- textstat_polarity(dfm, pos_neg_nrc) %>%
  rename(polarity_nrc = sentiment) %>%
  right_join(nrc_df) %>%
  left_join(
    textstat_polarity(dfm, pos_neg_oplex) %>%
      rename(polarity_oplex = sentiment)
  )
cor(sentiment_df$polarity_nrc, sentiment_df$polarity_oplex)


## lexical norms
fear_dict_hashtag <- dictionary(
  list(joy = hash_nrc$word[hash_nrc$emotion == "fear"])
)

valence(fear_dict_hashtag) <- list(
  joy = hash_nrc$score[hash_nrc$emotion == "fear"]
)

valence_df <- data_corpus_inaugural %>%
  textstat_valence(
    dictionary = fear_dict_hashtag,
    normalization = "all"
  ) %>%
  bind_cols(docvars(data_corpus_inaugural)) %>%
  mutate(text = as.character(data_corpus_inaugural)) %>%
  arrange(desc(sentiment)) %>%
  select(sentiment, President, Year, Party, text)

valence_df %>%
  filter(Party %in% c("Republican", "Democratic")) %>%
  ggplot(aes(x = Party, y = sentiment, fill = Party)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  theme_bw() +
  theme(
    text = element_text(size = 18),
    legend.position = "none"
  ) +
  labs(x = "", y = "Fear valence")

m3 <- lm(sentiment ~ Party, valence_df %>%
  filter(Party %in% c("Democratic", "Republican")))
summary(m3)


# LIWC
output_liwc <- liwcalike(corpus, dictionary = liwc)

df_liwc <- raw_data %>%
  mutate(raw_data, doc_id = as.character(doc_id)) %>%
  left_join(output_liwc, by = c("doc_id" = "docname")) %>%
  select(-c(Period:OtherP, WC, Dic)) %>%
  rename(punct = AllPunc)


liwc_cog_feats <- c("cogproc", "social", "drives")
liwc_gram_feats <- c("i", "we", "they", "you", "negate")
liwc_cult_feats <- c("leisure", "money", "work", "relig", "home")

df_liwc %>%
  # select(misinformation, c("posemo", "negemo")) %>%
  select(misinformation, liwc_cult_feats) %>%
  group_by(misinformation) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(-misinformation, names_to = "feature", values_to = "mean") %>%
  ggplot(aes(x = reorder(feature, mean), y = mean, fill = misinformation)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  labs(x = "", y = "Mean %", fill = "")


variables <- colnames(df_liwc[9:48])

models <- map_dfr(variables, function(var) {
  formula <- as.formula(paste0("`", var, "` ~ misinformation"))

  lm(formula, data = df_liwc) %>%
    broom::tidy() %>%
    dplyr::filter(term == "misinformationreal") %>%
    dplyr::mutate(variable = var)
})

models <- models %>%
  mutate(
    p_adjusted = p.adjust(p.value, method = "holm"),
    significant = p_adjusted < 0.05
  ) %>%
  arrange(desc(estimate))

ggplot(
  models %>%
    filter(significant == TRUE) %>%
    filter(rank(estimate) <= 10 | rank(-estimate) <= 10),
  aes(x = reorder(variable, estimate), y = estimate)
) +
  geom_point(size = 3) +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * std.error,
    ymax = estimate + 1.96 * std.error
  ), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "LIWC features",
    y = "Beta coefficient",
    title = "Coefficients predicting shares by LIWC features"
  ) +
  theme(text = element_text(size = 18))


## let's use data
fake.br <- readRDS("session_4_dba/data/fakebr.RDS")

fake_corpus <- corpus(fake.br)

fake_dfm <- tokens(fake_corpus,
  remove_punct = T,
  remove_symbols = T,
  remove_numbers = T,
  remove_url = T
) %>%
  dfm()

fake_pmi <- fake_dfm %>%
  dfm_trim(min_docfreq = 0.01, docfreq_type = "prop") %>%
  textstat_keyness(
    docvars(fake_dfm, "news_type") == "fake",
    measure = "pmi"
  ) %>%
  filter(pmi > 1.2)

fake_dict <- dictionary(
  list(fake = fake_pmi$feature)
)

fake_df <- dfm %>%
  dfm_lookup(fake_dict) %>%
  convert("data.frame") %>%
  right_join(
    corpus %>%
      convert("data.frame")
  ) %>%
  mutate(wc = ntoken(dfm))

ggplot(fake_df, aes(
  x = misinformation,
  y = fake
)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  labs(x = "", y = "")

m4 <- glmmTMB(fake ~ misinformation + wc,
  offset = log(wc),
  family = nbinom2,
  data = fake_df
)
summary(m4)


## create a dictionary
seed_words <- c("")

our_dict <- dictionary(list(var = seed_words))

dfm %>%
  dfm_lookup(our_dict)

our_dict_df <- dfm %>%
  dfm_lookup(our_dict) %>%
  convert("data.frame") %>%
  right_join(
    corpus %>%
      convert("data.frame")
  ) %>%
  mutate(
    wc = ntoken(dfm),
    var_norm = var / wc
  )

our_dict_df %>%
  mutate(mentioned = as.integer(var_norm > 0)) %>%
  group_by(misinformation) %>%
  summarise(pct = mean(mentioned, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = misinformation, y = pct, fill = misinformation)) +
  geom_col(width = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "none") +
  labs(x = "", y = "% of articles mentioning keywords")


# expand keywords
# download from portulanclarin
rdf <- rdf_parse("session_4_dba/data/wnet/mwnpt-rdf.xml", format = "ntriples")

# first get all entries and their written forms
query_entries <- "
SELECT ?entry ?word
WHERE {
  ?entry <http://www.w3.org/ns/lemon/ontolex#canonicalForm> ?form .
  ?form <http://www.w3.org/ns/lemon/ontolex#writtenRep> ?word .
}"
wn_entries <- rdf_query(rdf, query_entries)

# get sense -> synset mapping
query_senses <- "
SELECT ?entry ?synset
WHERE {
  ?entry <http://www.w3.org/ns/lemon/ontolex#sense> ?sense .
  ?sense <http://www.w3.org/ns/lemon/ontolex#reference> ?synset .
}"
sense_synset <- rdf_query(rdf, query_senses)


# find synonyms for all seed words
expanded_words <- sense_synset %>%
  inner_join(wn_entries, by = "entry") %>%
  filter(word %in% seed_words) %>% 
  inner_join(sense_synset, by = "synset") %>%
  inner_join(wn_entries, by = c("entry.y" = "entry")) %>%
  filter(!word.y %in% seed_words) %>%
  select(seed = word.x, synonym = word.y) %>%
  distinct()

# update dictionary
full_dict_words <- c(seed_words, expanded_words$synonym) %>% unique()

our_dict <- dictionary(list(var = full_dict_words))

our_dict_df <- dfm %>%
  dfm_lookup(our_dict) %>%
  convert("data.frame") %>%
  right_join(
    corpus %>%
      convert("data.frame")
  ) %>%
  mutate(
    wc = ntoken(dfm),
    var_norm = var / wc
  )

# plot
our_dict_df %>%
  mutate(mentioned = as.integer(var_norm > 0)) %>%
  group_by(misinformation) %>%
  summarise(pct = mean(mentioned, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = misinformation, y = pct, fill = misinformation)) +
  geom_col(width = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "none") +
  labs(x = "", y = "% of articles mentioning keywords")
