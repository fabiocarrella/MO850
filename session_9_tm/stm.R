library(tidyverse)
library(quanteda)
library(stm)
library(tidytext)
set.seed(42)

#https://burtmonroe.github.io/TextAsDataCourse/Tutorials/IntroSTM.nb.html

# ----------------------------------------------------------------
# DATA & PREPROCESSING
# ----------------------------------------------------------------
poliblogsFull <- read_csv("session_9_tm/data/poliblogs2008.csv") |>
  mutate(rating = as.factor(rating))

# use the built-in poliblog5k sample that ships with stm
data(poliblog5k)

# attach short preview text for later use
poliblog5k.meta <- poliblog5k.meta |>
  mutate(
    fulltext  = poliblogsFull$documents[as.integer(rownames(poliblog5k.meta))],
    shorttext = substr(fulltext, 1, 200)
  )

# build DFM via quanteda, then convert to stm format
poli.dfm <- corpus(poliblog5k.meta, text_field = "fulltext") |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) |>
  tokens_remove(stopwords("en")) |>
  dfm(tolower = TRUE) |>
  dfm_wordstem() |>
  dfm_trim(min_docfreq = 51, docfreq_type = "count")

stm_input <- convert(poli.dfm, to = "stm")

# ----------------------------------------------------------------
# 1. STM WITHOUT METADATA (baseline)
# ----------------------------------------------------------------
fit_nometa <- stm(
  documents  = stm_input$documents,
  vocab      = stm_input$vocab,
  K          = 20,
  max.em.its = 75,
  data       = poliblog5k.meta,
  init.type  = "Spectral",
  verbose    = FALSE
)

# topic labels: four different scoring methods
labelTopics(fit_nometa, n = 5)

# corpus-wide topic prevalence
plot(fit_nometa, type = "summary", main = "Topic prevalence (no metadata)")

# top documents for a topic of interest
findThoughts(
  fit_nometa,
  texts  = poliblog5k.meta$shorttext,
  topics = 12,
  n      = 3
)$docs[[1]] |>
  plotQuote(main = "Top documents — Topic 12", width = 100, text.cex = 1.5)

# word cloud for the same topic
cloud(fit_nometa, topic = 12, scale = c(3, 1))

# ----------------------------------------------------------------
# 2. STM WITH PREVALENCE + CONTENT COVARIATES
# ----------------------------------------------------------------
# prevalence: how much a topic appears ~ political leaning + time
# content:    vocabulary used within a topic ~ political leaning
fit_meta <- stm(
  documents  = stm_input$documents,
  vocab      = stm_input$vocab,
  K          = 20,
  prevalence = ~ rating + s(day),   # s() = smooth spline over time
  content    = ~ rating,
  max.em.its = 75,
  data       = poliblog5k.meta,
  init.type  = "Spectral",
  verbose    = FALSE
)

# how does vocabulary for a topic differ by political leaning?
plot(fit_meta, type = "perspectives", topics = 5,
     main = "Topic 5: Liberal vs Conservative vocabulary")

plot(fit_meta, 
     type   = "perspectives", 
     topics = 7)

# estimated effect of covariates on topic prevalence
effects_meta <- estimateEffect(
  formula = 1:20 ~ rating + s(day),
  stmobj  = fit_meta,
  meta    = poliblog5k.meta
)

# prevalence difference: liberal vs conservative blogs
plot(effects_meta,
     covariate = "rating",
     topics    = c(5, 11, 12), #social/economy/iraq
     model     = fit_meta,
     method    = "difference",
     cov.value1 = "Liberal",
     cov.value2 = "Conservative",
     xlab      = "← More Conservative    More Liberal →",
     main      = "Topic prevalence by political leaning")


# ----------------------------------------------------------------
# 3. PREVALENCE OVER TIME
# ----------------------------------------------------------------
effects_time <- estimateEffect(
  formula = 1:20 ~ s(day),
  stmobj  = fit_nometa,
  meta    = poliblog5k.meta,
  uncertainty = "Global"
)

# pick a topic and plot its trajectory through 2008
plot(effects_time,
     covariate = "day",
     topics    = 12,
     method    = "continuous",
     model     = fit_nometa,
     xaxt      = "n",
     xlab      = "2008",
     ylab      = "Expected topic proportion",
     main      = "Topic 12 prevalence over time")

# add readable month labels on x-axis
month_seq   <- seq(as.Date("2008-01-01"), as.Date("2008-12-01"), by = "month")
axis(1,
     at     = as.numeric(month_seq) - min(as.numeric(month_seq)),
     labels = months(month_seq),
     las    = 2)

# ----------------------------------------------------------------
# 4. INTERACTION: does the time trend differ by political leaning?
# ----------------------------------------------------------------
fit_interaction <- stm(
  documents  = stm_input$documents,
  vocab      = stm_input$vocab,
  K          = 20,
  prevalence = ~ rating * s(day),
  max.em.its = 75,
  data       = poliblog5k.meta,
  init.type  = "Spectral",
  verbose    = FALSE
)

effects_int <- estimateEffect(
  formula = c(7) ~ rating * day,
  stmobj  = fit_interaction,
  meta    = poliblog5k.meta,
  uncertainty = "None"
)

# overlay liberal and conservative trajectories for topic 7
plot(effects_int, covariate = "day", model = fit_interaction,
     method = "continuous", topics = 7,
     moderator = "rating", moderator.value = "Liberal",
     linecol = "steelblue", ylim = c(0, .12),
     xlab = "Days (2008)", main = "Topic 7 — Liberal vs Conservative over time",
     printlegend = FALSE)

plot(effects_int, covariate = "day", model = fit_interaction,
     method = "continuous", topics = 7,
     moderator = "rating", moderator.value = "Conservative",
     linecol = "firebrick", add = TRUE, printlegend = FALSE)

legend("topright", legend = c("Liberal", "Conservative"),
       col = c("steelblue", "firebrick"), lwd = 2)
