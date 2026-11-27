library(lme4)
library(lmerTest)
library(readr)
library(dplyr)
library(forcats)

data_dir <- "../DATA"

blocks <- read_csv(
  file.path(data_dir, "blocks.csv"),
  show_col_types = FALSE
)

demo <- read_csv(
  file.path(data_dir, "demo.csv"),
  show_col_types = FALSE
)

dependent_vars <- c(
  "trust",
  "understanding",
  "nasa"
)

df <- blocks %>%
  filter(block != "trial") %>%
  left_join(
    demo,
    by = "participant_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    participant_id = factor(participant_id),
    route = factor(route),
    
    condition = recode(
      condition,
      "Nelo" = "Baseline",
      "Coda" = "Proactive",
      "Lumo" = "Optional"
    ),
    
    condition = factor(
      condition,
      levels = c(
        "Baseline",
        "Proactive",
        "Optional"
      )
    ),
    
    block = factor(
      as.integer(block),
      levels = c(1, 2, 3)
    )
  )

order_models <- list()

for (dv in dependent_vars) {
  
  analysis_data <- df %>%
    filter(
      !is.na(.data[[dv]]),
      !is.na(condition),
      !is.na(block),
      !is.na(participant_id)
    )
  
  model_condition <- lmer(
    formula = as.formula(
      paste0(
        dv,
        " ~ condition + (1 | participant_id)"
      )
    ),
    data = analysis_data,
    REML = FALSE
  )
  
  model_block <- lmer(
    formula = as.formula(
      paste0(
        dv,
        " ~ condition + block + (1 | participant_id)"
      )
    ),
    data = analysis_data,
    REML = FALSE
  )
  
  model_interaction <- lmer(
    formula = as.formula(
      paste0(
        dv,
        " ~ condition * block + (1 | participant_id)"
      )
    ),
    data = analysis_data,
    REML = FALSE
  )
  
  comparison_block <- anova(
    model_condition,
    model_block
  )
  
  comparison_interaction <- anova(
    model_block,
    model_interaction
  )
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(toupper(dv), "\n")
  cat(strrep("=", 70), "\n", sep = "")
  
  cat("\nCondition vs. Condition + Block\n")
  print(comparison_block)
  
  cat("\nCondition + Block vs. Condition × Block\n")
  print(comparison_interaction)
  
  cat("\nPrimary model: Condition + Block\n")
  print(summary(model_block))
  
  cat("\nInteraction model: Condition × Block\n")
  print(summary(model_interaction))
  
  order_models[[dv]] <- list(
    condition = model_condition,
    block = model_block,
    interaction = model_interaction,
    comparison_block = comparison_block,
    comparison_interaction = comparison_interaction
  )
}

order_models$trust$comparison_block
order_models$trust$comparison_interaction

# summary(order_models$trust$block)
# summary(order_models$trust$interaction)
