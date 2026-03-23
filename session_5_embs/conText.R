library(conText)
library(quanteda)
library(ggplot2)
library(dplyr)

toks <- tokens(cr_sample_corpus)

#extract contexts around target word
immig_toks <- tokens_context(
  x       = toks,
  pattern = "immigr*",
  window  = 6L
)

feats <- featnames(dfm(immig_toks))

# get nearest neighbors separately for each party
set.seed(42)
immig_nns <- get_nns(
  x                = immig_toks,
  N                = 10,                              # top 10 neighbors per group
  groups           = docvars(immig_toks, "party"),  
  candidates       = feats,
  pre_trained      = cr_glove_subset,
  transform        = TRUE,
  transform_matrix = cr_transform,
  bootstrap        = TRUE,                           
  as_list          = FALSE                           
)

print(immig_nns)

# plot 
ggplot(immig_nns, aes(x = reorder(feature, value), y = value, fill = target)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ target, scales = "free_y") +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title    = 'Nearest neighbors of "immigration" by party',
    subtitle = "Words most semantically associated in each group's context",
    x        = NULL,
    y        = "cosine similarity"
  ) +
  theme_minimal(base_size = 13)

# nns_ratio (which words discriminate the groups?) 
immig_nns_ratio <- get_nns_ratio(
  x                = immig_toks,      
  N                = 10,
  groups           = docvars(immig_toks, "party"),
  numerator        = "R",
  candidates       = feats,
  pre_trained      = cr_glove_subset,
  transform        = TRUE,
  transform_matrix = cr_transform,
  bootstrap        = TRUE,
  num_bootstraps   = 100,
  permute          = TRUE,
  num_permutations = 100,
  verbose          = FALSE
)

plot_nns_ratio(immig_nns_ratio, alpha = 0.01, horizontal = F)
