library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(quanteda.dictionaries)
library(dplyr)
library(readxl)
library(stringr)
library(effectsize)

# data from https://figshare.com/articles/dataset/AH_AITD_Arslan_s_Human_and_AI_Text_Database/29144348?file=54804839

data <- read_xlsx("session_11_llms/data/Dataset.xlsx")
data$WC <- str_count(data$text, "\\S+")

set.seed(42)
short_data <- data %>% 
  group_by(label_name) %>% 
  sample_n(5) %>% 
  mutate(short_text = str_trunc(text, width = 500)) %>% 
  ungroup() %>% 
  arrange(desc(sr.no))

corp <- corpus(short_data$short_text)
toks <- tokens(corp)
dfm <- dfm(toks)

# readability
read <- textstat_readability(corp, measure = "FOG")

# NRC
nrc <- textstat_polarity(corp, dictionary = data_dictionary_NRC)

# moral foundations
#mfd <- liwcalike(corp, dictionary = data_dictionary_MFD)

results <- data.frame(
  text = short_data$short_text,
  label = short_data$label_name,
  fog = read$FOG,
  emotion = nrc$sentiment
  #morality = rowSums(mfd)
)
results <- results %>% select(-label)
results[1,]


## full analysis
full_corp <- corpus(data$text)
full_toks <- tokens(full_corp)
full_dfm <- dfm(full_toks)

# readability
full_read <- textstat_readability(full_corp, measure = "FOG")

# NRC
full_nrc <- textstat_polarity(full_corp, dictionary = data_dictionary_NRC)

full_results <- data.frame(
  text = data$text,
  label = data$label_name,
  fog = full_read$FOG,
  emotion = full_nrc$sentiment
  #morality = rowSums(mfd)
)

t.test(fog ~ label, data = full_results)
cohens_d(fog ~ label, data = full_results)
t.test(emotion ~ label, data = full_results)
cohens_d(emotion ~ label, data = full_results)
