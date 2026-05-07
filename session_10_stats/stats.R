library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest) 
library(effectsize)
library(performance)

# 1. LOAD & PREPARE DATA

df <- arrow::read_parquet("session_10_stats/data/brpolicorpus_honesty_ddr.parquet")   # or read.csv / read_parquet etc.

# --- Parse date ---
df <- df |>
  mutate(date = dmy(Data))   # "28/12/2000" -> Date

# --- Clean party & assign ideology ---
left   <- c("PT", "PSB", "PDT", "PSOL", "PCDOB", 
            "REDE", "PV", "PSOL", "PMN")

right  <- c("PSDB", "DEM", "PFL", "PP", "PSL", "PL",
            "PPB",          # PP predecessor
            "PRONA",        # far right nationalist
            "PRTB",         # right nationalist
            "PSDC",         # christian conservative
            "PRP",          # right nationalist
            "PATRI", "PATRIOTA")  # same party, name variants

centro <- c("PMDB", "MDB", "PSD", "PTB", "PR", "PROS",
            "SD", "SDD",    # SDD = SD rebranded
            "REPUBLICANOS", "PRB",  # PRB became Republicanos
            "PODE", "PODEMOS",
            "CIDADANIA", "PPS",  # PPS became Cidadania
            "PSC",          # centre-right evangelical
            "PV",           # sometimes classified centre-green
            "NOVO",         # liberal-right, often centre-right
            "UNIÃO",        # União Brasil, centre-right
            "PHS",          # centre-right
            "SOLIDARIEDADE","AVANTE",
            "PTdoB", "PTC", "PTN",
            "PMB", "PEN", "PMN", "PMR",
            "PST", "REDE")

valid <- c(left, right, centro)

party_lookup <- tibble(Partido = unique(df$Partido)) |>
  mutate(
    partido_clean = case_when(
      # only keep strings that look like party acronyms (2-15 uppercase letters, maybe a space)
      str_detect(str_trim(Partido), "^[A-Za-zÀ-ú]{2,15}$") &
        str_to_upper(str_trim(Partido)) %in% str_to_upper(valid) ~ str_to_upper(str_trim(Partido)),
      TRUE ~ NA_character_
    )
  )

df <- df |>
  left_join(party_lookup, by = "Partido") |>
  mutate(
    ideology = case_when(
      partido_clean %in% str_to_upper(left)   ~ "Left",
      partido_clean %in% str_to_upper(right)  ~ "Right",
      partido_clean %in% str_to_upper(centro) ~ "Centre",
      TRUE ~ NA_character_
    ),
    ideology = factor(ideology, levels = c("Left", "Centre", "Right"))
  )

# Keep only speeches with a known ideology & valid DDR score
df_clean <- df |>
  filter(!is.na(ideology), !is.na(ddr_score))

cat("Speeches retained:", nrow(df_clean), "\n")
cat("Unique speakers:  ", n_distinct(df_clean$Orador), "\n")


# 2. DESCRIPTIVE STATISTICS

# --- Overall summary ---
summary(df_clean$ddr_score)

# --- By ideology group ---
df_clean |>
  group_by(ideology) |>
  summarise(
    n      = n(),
    mean   = mean(ddr_score),
    sd     = sd(ddr_score),
    median = median(ddr_score),
    iqr    = IQR(ddr_score),
    se     = sd / sqrt(n)
  )


# 3. VISUALISATION

# --- Distribution of DDR scores ---
ggplot(df_clean, aes(x = ddr_score)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Honesty Framing (DDR)",
       x = "DDR Score", y = "Count") +
  theme_minimal()

# --- By ideology: violin + boxplot ---
ggplot(df_clean, aes(x = ideology, y = ddr_score, fill = ideology)) +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(title = "Honesty Framing by Ideology",
       x = NULL, y = "DDR Score") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Time trend: raw points + smooth ---
ggplot(df_clean, aes(x = date, y = ddr_score, color = ideology)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Honesty Framing Over Time by Ideology",
       x = "Date", y = "DDR Score", color = "Ideology") +
  theme_minimal()

# --- Q-Q plot: check normality ---
qqnorm(sample(df_clean$ddr_score, 5000))
qqline(sample(df_clean$ddr_score, 5000), col = "red")


# 4. NORMALITY CHECK

shapiro.test(sample(df_clean$ddr_score, 5000))
# Note: with large n, Shapiro-Wilk is very sensitive
# Trust the Q-Q plot more; t-tests are robust to mild non-normality

# 5. T-TEST: Left vs. Right

df_lr <- df_clean |>
  filter(ideology %in% c("Left", "Right")) |>
  mutate(ideology = droplevels(ideology))

t.test(ddr_score ~ ideology, data = df_lr)

# Effect size
cohens_d(ddr_score ~ ideology, data = df_lr)

# Non-parametric alternative (if normality is a concern)
wilcox.test(ddr_score ~ ideology, data = df_lr)


# 6. ANOVA: Left vs. Centre vs. Right

aov_model <- aov(ddr_score ~ ideology, data = df_clean)
summary(aov_model)

# Effect size
eta_squared(aov_model)

# Post-hoc: which pairs differ?
TukeyHSD(aov_model)

# Non-parametric alternative
kruskal.test(ddr_score ~ ideology, data = df_clean)


# 7. LINEAR REGRESSION: Time trend

# Center date (days since median date — helps interpret intercept)
median_date <- median(df_clean$date)
df_clean <- df_clean |>
  mutate(date_c = as.numeric(date - median_date) / 365)  # in years

# Simple regression: does honesty framing change over time?
m1 <- lm(ddr_score ~ date_c, data = df_clean)
summary(m1)

# Add ideology as a predictor
m2 <- lm(ddr_score ~ date_c + ideology, data = df_clean)
summary(m2)

m2_red <- lm(ddr_score ~ date_c + ideology, data = df_clean %>% sample_n(1000))
plot(m2_red)
check_model(m2_red)

# Model comparison
AIC(m1, m2)
compare_performance(m1, m2, rank = T)


# 8. INTERACTION: Time * Ideology

# Does the time trend differ by ideology?
m3 <- lm(ddr_score ~ date_c * ideology, data = df_clean)
summary(m3)

# Interpretation:
#   date_c                    = trend for the reference group (Left)
#   ideologyRight             = Right vs Left at the median date
#   date_c:ideologyRight      = DIFFERENCE in trend: Right vs Left

# Visualise the interaction
ggplot(df_clean, aes(x = date_c, y = ddr_score, color = ideology)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Interaction: Time × Ideology",
       x = "Years (centered)", y = "DDR Score") +
  theme_minimal()

# Compare models: does interaction improve fit?
anova(m2, m3)
AIC(m2, m3)
compare_performance(m1, m2, m3, rank = T)

# Check assumptions
#check_model(m3)   # from 'performance' — plots linearity, normality, homoscedasticity


# 9. MIXED-EFFECTS MODEL: Accounting for repeated speakers

# Problem: same speakers appear many times -> observations are not independent
# Solution: add a random intercept per speaker

m_lmer <- lmer(
  ddr_score ~ date_c * ideology + (1 | Apelido),
  data = df_clean
)

summary(m_lmer)

interactions::interact_plot(m_lmer, date_c, ideology, colors = c("red", "green", "blue"))
# Fixed effects: same interpretation as lm()
# Random effects: how much variance is explained by speaker differences?

# R-squared: marginal (fixed only) and conditional (fixed + random)
r2(m_lmer)   # from 'performance'

# Is the random effect warranted? Compare to plain lm
compare_performance(m3, m_lmer, rank=T)

# Visualise fixed effects
# (lmerTest makes fixef() and confint() easy to work with)
confint(m_lmer, method = "Wald")
