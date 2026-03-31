library(quanteda)
library(quanteda.dictionaries)
library(tidyverse)


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

data <- read_csv("session_6_ddr/data/utlc_movies.csv")

corpus <- corpus(data, text_field = "review_text_processed", docid_field = "original_index")

output_liwc <- liwcalike(corpus, dictionary = liwc)

df_liwc <- data %>%
  mutate(data, doc_id = as.character(original_index)) %>%
  left_join(output_liwc, by = c("doc_id" = "docname")) %>%
  select(c(1:8, posemo, negemo)) %>%
  mutate(liwc_score = posemo - negemo)

write_excel_csv(df_liwc, "utlc_movies_liwc_scores.csv")