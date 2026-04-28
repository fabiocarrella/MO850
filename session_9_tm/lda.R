library(tidyverse)
library(quanteda) 
library(topicmodels)  
library(seededlda)      
library(tidytext)
library(ldatuning)
library(lda)
set.seed(42)

brpoli_speeches <- BrPoliCorpus::download_InauguralSpeeches()
essays <- read_csv("session_9_tm/data/extended_essay-br.csv") %>% 
  mutate(doc_id = row_number())

essays_sample <- essays |>
  mutate(stratum = ntile(score, 5)) |>   
  group_by(stratum) |>
  slice_sample(n = 100) |>                
  ungroup()


## Quanteda pipeline
corpus <- corpus(brpoli_speeches, text_field = "text", docid_field = "doc_id")

toks <- tokens(corpus,
               remove_numbers = T,
               remove_punct = T,
               remove_symbols = T
               )

custom_stops <- c(stopwords("pt"), "é", "ser")

toks_nostop <- tokens_remove(toks,
                             pattern=stopwords(
                               language = "pt",
                               source = "stopwords-iso"
                             ))

dfm <- dfm(toks_nostop, tolower = T) %>% 
  dfm_trim(min_termfreq = 5, 
           termfreq_type = "count")

dfm %>% quanteda.textstats::textstat_frequency() %>% head(10)

dfm <- dfm_subset(dfm, ntoken(dfm) > 0)


## Fitting an LDA model
K = 10

lda_model <- LDA(dfm,
                 k=K,
                 control = list(seed=42)) # seed is important

ntopics <- K 
nterms  <- 10  

lda_top_terms <- tidy(lda_model, matrix = "beta")  |>
  group_by(topic) |>
  slice_max(beta, n = nterms) |>       
  ungroup() |>
  arrange(topic, -beta) |>
  mutate(
    term  = paste0(term, " (", round(beta, 3), ")"),
    topic = factor(paste("topic", topic),
                   levels = paste("topic", 1:ntopics)),
    top   = factor(paste("top", rep(1:nterms, ntopics)),
                   levels = paste("top", 1:nterms))
  ) |>
  select(-beta) |>
  tidyr::spread(topic, term)

tidy(lda_model, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  ungroup() |>
  mutate(
    term  = reorder_within(term, beta, topic),
    topic = factor(paste("Topic", topic), levels = paste("Topic", 1:ntopics))
  ) |>
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = "β (term-topic probability)") +
  theme_bw() +
  theme(text = element_text(size = 17)) 


## Seeded LDA
dict <- dictionary(list(
  militar        = c("militar", "forças armadas", "guerra", "conflito"), 
  liberdade      = c("liberdade", "direito", "livre"),                 
  nacao          = c("nação", "país", "cidadão", "brasil"),            
  lei            = c("lei", "justiça", "tribunal", "prisão"),            
  tratado        = c("tratado", "acordo", "negociaçao"),                  
  indigena       = c("indígena", "povos indígenas", "território"),     
  trabalho       = c("trabalho", "emprego", "condiçao"),                 
  dinheiro       = c("banco", "moeda", "real", "dinheiro"),            
  financas       = c("dívida", "investimento", "finançia"),                    
  prosperidade   = c("prosperidade", "crescimento", "desenvolvimento"),           
  industria      = c("indústria", "produção", "manufatur"),            
  constituicao   = c("constituiçao", "estado", "poder"),                 
  agricultura    = c("agricultur", "campo", "terra"),                  
  governo        = c("governo", "administraçao", "serviço público")      
))

slda_model <- textmodel_seededlda(
  dfm,
  dictionary    = dict,
  residual      = TRUE,   
)

terms(slda_model)

slda_topic_df <- data.frame(
  Date      = slda_model$data$Term_Begins,      
  President = slda_model$data$Political_Name, 
  Topic     = topics(slda_model)  
) |>
  dplyr::mutate(
    Date = stringr::str_remove_all(Date, "-.*"),      
    Date = stringr::str_replace_all(Date, ".$", "0")
  ) |>
  dplyr::mutate_if(is.character, factor)

# visualize proportions over time
slda_topic_df |>
  filter(!is.na(Date)) |>
  group_by(Date, Topic) |>
  summarise(freq = n(), .groups = "drop") |>
  ggplot(aes(x = Date, y = freq, fill = Topic)) +
  geom_bar(stat = "identity",  
           position = "fill", 
           colour = "white",
           linewidth = 0.2) +
  theme_bw() +
  labs(
    title    = "Topic Proportions in presidential speeches by decade",
     x        = "Decade",
    y        = "Proportion of paragraphs"
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    labels = scales::percent_format() 
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    text = element_text(size = 18)
  )

## Data-driven LDA
ideal_k <- FindTopicsNumber(
  dfm,
  topics  = seq(from = 4, to = 20, by = 1),  
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method  = "Gibbs",                           
  control = list(seed = 42),                   
  verbose = TRUE                              
)

FindTopicsNumber_plot(ideal_k)

new_k <- 16

topicModel <- LDA(
  dfm,
  k = new_k,
  method  = "Gibbs",
  control = list(iter = 500, verbose = 25, seed = 42)
)

tidy(topicModel, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  ungroup() |>
  mutate(
    term  = reorder_within(term, beta, topic),
    topic = factor(paste("Topic", topic), levels = paste("Topic", 1:new_k))
  ) |>
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = "β (term-topic probability)") +
  theme_bw() +
  theme(text = element_text(size = 17)) 

# plot document-level distribution
exampleIds <- c("34_bolsonaro", "29_lula", "25_collor", "15_kubitschek", "11_getulio")

topic_labels <- tidy(topicModel, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 3) |>
  summarise(topic_label = paste(term, collapse = "_"))

tidy(topicModel, matrix = "gamma") |>
  filter(document %in% exampleIds) |>
  left_join(topic_labels, by = "topic") |>
  mutate(
    topic_label = factor(topic_label, levels = topic_labels$topic_label),
    document    = paste("Doc", document)
  ) |>
  ggplot(aes(x = topic_label, y = gamma)) +
  geom_col(aes(fill = topic_label)) +
  coord_flip() +
  facet_wrap(~ document, ncol = length(exampleIds)) +
  labs(x = NULL, y = "Topic proportion (γ)") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "none")

# check the alpha parameter (lower: more concentrated distribution)
alpha_value <- attr(topicModel, "alpha")
cat("Alpha (automatically estimated):", round(alpha_value, 4), "\n")

topicModel2 <- LDA(
  dfm,
  new_k,
  method  = "Gibbs",
  control = list(
    iter    = 500,
    verbose = 25,
    alpha   = 5,
    seed = 42
  )
)

tidy(topicModel2, matrix = "gamma") |>
  filter(document %in% exampleIds) |>
  left_join(topic_labels, by = "topic") |>
  mutate(
    topic_label = factor(topic_label, levels = topic_labels$topic_label),
    document    = paste("Doc", document)
  ) |>
  ggplot(aes(x = topic_label, y = gamma)) +
  geom_col(aes(fill = topic_label)) +
  coord_flip() +
  facet_wrap(~ document, ncol = length(exampleIds)) +
  labs(x = NULL, y = "Topic proportion (γ)") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "none")

# check pervasive topic (mean prob across docs)
tmResult <- posterior(topicModel)
theta <- tmResult$topics
topicNames <- apply(topicmodels::terms(topicModel, 5), 2, paste, collapse = "_")
topicProportions <- colSums(theta) / nrow(dfm)
names(topicProportions) <- topicNames

soP <- sort(topicProportions, decreasing = TRUE)
cat("Topics ranked by mean corpus proportion:\n")
paste(round(soP, 4), ":", names(soP))

# check dominant topic (primary topic assignments)
countsOfPrimaryTopics <- rep(0, new_k)
names(countsOfPrimaryTopics) <- topicNames

for (i in 1:nrow(dfm)) {                                       
  topicsPerDoc <- theta[i, ]                                  
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1]  
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}

so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
cat("Topics ranked by number of documents where they are dominant:\n")
paste(so, ":", names(so))

slda_topic_df |>
  group_by(Date, Topic) |>
  summarise(freq = n(), .groups = "drop") |>
  ggplot(aes(x = Date, y = freq, fill = Topic)) +
  geom_bar(stat = "identity",  
           position = "fill", 
           colour = "white",
           linewidth = 0.2) +
  theme_bw() +
  labs(
    title    = "Topic Proportions in presidential speeches by decade",
    x        = "Decade",
    y        = "Proportion of paragraphs"
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    labels = scales::percent_format() 
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    text = element_text(size = 18)
  )

# plot topics over time
brpoli_speeches$decade <- paste0(substr(brpoli_speeches$Term_Begins, 1, 3), "0")

topic_proportion_per_decade <- stats::aggregate(
  theta,
  by   = list(decade = brpoli_speeches$decade),
  FUN  = mean
) %>% 
  filter(decade != "NA0")

colnames(topic_proportion_per_decade)[2:(new_k + 1)] <- topicNames

reshape2::melt(topic_proportion_per_decade, id.vars = "decade") |>
  ggplot(aes(x = decade, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(
    title    = "Topic proportions in presidential speeches by decade",
    subtitle = paste0("Mean topic proportions per decade (K = ", new_k, " topics)"),
    y        = "Proportion",
    x        = "Decade",
    fill     = "Topic"
  ) +
  scale_fill_manual(
    values = rev(colorRampPalette(RColorBrewer::brewer.pal(8, "RdBu"))(new_k))
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 18)
  )


## Supervised LDA
essays_corpus <- corpus(essays_sample, text_field = "essay", docid_field = "doc_id")
essays_dfm <- essays_corpus %>%
  tokens(remove_punct = T, remove_symbols = T, remove_numbers = T) %>% 
  tokens_remove(pattern = stopwords(language = "pt", source = "stopwords-iso")) %>% 
  dfm(tolower = T) %>% 
  dfm_trim(min_termfreq = 5, termfreq_type = "count")
essays_dfm <- dfm_subset(essays_dfm, ntoken(essays_dfm) > 0)

essay_k <- FindTopicsNumber(
  essays_dfm,
  topics  = seq(from = 20, to = 50, by = 2),  
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),  
  method  = "Gibbs",                           
  control = list(seed = 42),                   
  verbose = TRUE                               
)
FindTopicsNumber_plot(essay_k)

essayTopicModel <- LDA(
  essays_dfm,
  k = 32,
  method  = "Gibbs",
  control = list(iter = 500, seed = 42)
)

essays_theta <- posterior(essayTopicModel)$topics 

essays_results <- as.data.frame(essays_theta) |>
  setNames(paste0("topic_", 1:32)) |>
  mutate(doc_id = as.integer(docnames(essays_dfm))) |>
  left_join(essays_sample |> select(doc_id, score), by = "doc_id")

mod <- lm(score ~ ., data = essays_results)
summary(mod)


broom::tidy(mod, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  filter(p.value < 0.05) |>
  mutate(
    across(c(estimate, conf.low, conf.high), ~ . + coef(mod)[1])
  ) %>% 
  left_join(                                        
    data.frame(
      term  = paste0("topic_", 1:32),
      label = apply(topicmodels::terms(essayTopicModel, 5), 2, paste, collapse = ", ")
    )
  ) |>
  filter(!is.na(label)) |>
  mutate(label = factor(label, levels = label[order(estimate)])) |>
  ggplot(aes(x = label, y = estimate, colour = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  scale_colour_gradient2(low = "red", mid = "grey", high = "blue") +
  labs(x = NULL, y = "Association with score") +
  theme_bw() +
  theme(legend.position = "none")
