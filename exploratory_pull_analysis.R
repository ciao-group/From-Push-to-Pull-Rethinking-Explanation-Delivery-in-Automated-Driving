library(lme4)
library(lmerTest)
library(emmeans)
library(readr)
library(dplyr)

data_dir <- "DATA"

clicks <- read_csv(
  file.path(data_dir, "clicks.csv"),
  show_col_types = FALSE
)

clicks$event_id <- interaction(clicks$route, clicks$zone, drop = TRUE)

# model
 
click_model <- glmer(
  click ~ perceivability +
          safety_relevance +
         (1|participant_id) +
         (1|event_id),
  family = binomial,
  data = clicks
)

summary(click_model)

# emmeans

emm_perceivability <- emmeans(
  click_model,
  ~ perceivability,
  type = "response"
)

emm_safety_relevance <- emmeans(
  click_model,
  ~ safety_relevance,
  type = "response"
)

summar8.11y(
  contrast(emm_perceivability, method = "revpairwise"),
  infer = c(TRUE, TRUE)
)

summary(
  contrast(emm_safety_relevance, method = "revpairwise"),
  infer = c(TRUE, TRUE)
)
