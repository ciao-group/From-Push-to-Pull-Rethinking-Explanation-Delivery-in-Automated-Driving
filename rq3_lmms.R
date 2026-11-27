library(lme4)
library(lmerTest)
library(emmeans)
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(showtext)
library(parameters)

font_add("Avenir")
showtext_auto()

data_dir <- "../DATA"
plot_dir <- "plots"


demo <- read_csv(
  file.path(data_dir, "demo.csv"),
  show_col_types = FALSE
)

blocks <- read_csv(
  file.path(data_dir, "blocks.csv"),
  show_col_types = FALSE
)

frequency_map <- c(
  "never" = 0,
  "rarely" = 1,
  "Several times a week" = 2,
  "Daily" = 3
)

continuous_predictors <- c(
  "bfi_extraversion",
  "bfi_agreeableness",
  "bfi_conscientiousness",
  "bfi_neuroticism",
  "bfi_openness",
  "nfc_score",
  "tri_score",
  "age",
  "driving_frequency_num",
  "automation_familiarity"
)

demo <- demo %>%
  mutate(
    date = as.Date(date),
    age = as.integer(format(date, "%Y")) - birth_year,
    driving_frequency_num = unname(
      frequency_map[driving_frequency]
    )
  ) %>%
  mutate(
    across(
      all_of(continuous_predictors),
      ~ as.numeric(scale(.x)),
      .names = "z_{.col}"
    )
  )

df <- blocks %>%
  left_join(
    demo,
    by = "participant_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    participant_id = factor(participant_id),
    condition = factor(condition),
    condition = fct_relevel(
      condition,
      "Coda",
      "Lumo",
      "Nelo"
    )
  )

condition_labels <- c(
  "Coda" = "Push",
  "Lumo" = "Pull",
  "Nelo" = "No Explanation"
)

condition_colors <- c(
  "Push" = "#1192e8",
  "Pull" = "#6929c4",
  "No Explanation" = "#005d5d"
)

condition_linetypes <- c(
  "Push" = "solid",
  "Pull" = "solid",
  "No Explanation" = "longdash"
)

emm_options(lmer.df = "satterthwaite")

planned_contrast_weights <- list(
  "Pull > Baseline" = c(
    Coda = 0,
    Lumo = 1,
    Nelo = -1
  ),
  "Push > Baseline" = c(
    Coda = 1,
    Lumo = 0,
    Nelo = -1
  ),
  "Pull > Push" = c(
    Coda = -1,
    Lumo = 1,
    Nelo = 0
  )
)

run_planned_contrasts <- function(
    model,
    at = NULL,
    side = ">",
    adjustment = "holm"
) {
  if (is.null(at)) {
    estimated_means <- emmeans(
      model,
      ~ condition
    )
  } else {
    estimated_means <- emmeans(
      model,
      ~ condition,
      at = at
    )
  }
  
  contrasts <- contrast(
    estimated_means,
    method = planned_contrast_weights
  )
  
  as.data.frame(
    test(
      contrasts,
      side = side,
      adjust = adjustment
    )
  )
}


plot_interaction <- function(
    model,
    moderator,
    x_labels,
    x_title,
    y_title,
    file_name
) {
  moderator_values <- c(-1, 1)
  
  estimated_means <- emmeans(
    model,
    specs = as.formula(
      paste("~ condition *", moderator)
    ),
    at = setNames(
      list(moderator_values),
      moderator
    )
  )
  
  plot_data <- as.data.frame(estimated_means) %>%
    mutate(
      condition = factor(
        condition,
        levels = names(condition_labels),
        labels = unname(condition_labels)
      )
    )
  
  plot_object <- ggplot(
    plot_data,
    aes(
      x = .data[[moderator]],
      y = emmean,
      group = condition,
      colour = condition,
      linetype = condition
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(
      aes(
        ymin = lower.CL,
        ymax = upper.CL
      ),
      width = 0.05,
      linewidth = 0.6
    ) +
    scale_x_continuous(
      breaks = moderator_values,
      labels = x_labels,
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    scale_colour_manual(values = condition_colors) +
    scale_linetype_manual(values = condition_linetypes) +
    labs(
      x = x_title,
      y = y_title,
      colour = NULL,
      linetype = NULL
    ) +
    theme_classic(
      base_size = 20,
      base_family = "Avenir"
    ) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20),
      
      axis.title.x = element_text(
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        margin = margin(r = 12)
      ),
      
      plot.margin = margin(
        t = 15,
        r = 50,
        b = 15,
        l = 15,
        unit = "pt"
      )
    )
  
  ggsave(
    filename = file.path(plot_dir, file_name),
    plot = plot_object,
    width = 6.2,
    height = 4.8,
    units = "in",
    device = "pdf"
  )
  
  plot_object
}

# Trust

model_trust_full_ml <- lmer(
  trust ~
    condition * z_bfi_openness +
    z_bfi_extraversion +
    z_bfi_agreeableness +
    z_bfi_conscientiousness +
    z_bfi_neuroticism +
    z_nfc_score +
    z_tri_score +
    z_age +
    z_driving_frequency_num +
    z_automation_familiarity +
    (1 | participant_id),
  data = df,
  REML = FALSE
)

model_trust_reduced_ml <- lmer(
  trust ~
    condition * z_bfi_openness +
    z_bfi_conscientiousness +
    z_tri_score +
    (1 | participant_id),
  data = df,
  REML = FALSE
)

anova(
  model_trust_reduced_ml,
  model_trust_full_ml
)

model_trust <- update(
  model_trust_reduced_ml,
  REML = TRUE
)

summary(model_trust)
model_parameters(model_trust, ci = 0.95, ci_method = "satterthwaite", digits = "signif4")

trust_pairwise <- run_planned_contrasts(
  model_trust,
  at = list(z_bfi_openness = 0)
)

print(trust_pairwise)


# System understanding

model_understanding_full_ml <- lmer(
  understanding ~
    condition * z_age +
    z_bfi_extraversion +
    z_bfi_agreeableness +
    z_bfi_conscientiousness +
    z_bfi_neuroticism +
    z_bfi_openness +
    z_nfc_score +
    z_tri_score +
    z_driving_frequency_num +
    z_automation_familiarity +
    (1 | participant_id),
  data = df,
  REML = FALSE
)

model_understanding_reduced_ml <- lmer(
  understanding ~
    condition * z_age +
    z_driving_frequency_num +
    (1 | participant_id),
  data = df,
  REML = FALSE
)

anova(
  model_understanding_reduced_ml,
  model_understanding_full_ml
)

model_understanding <- update(
  model_understanding_reduced_ml,
  REML = TRUE
)


summary(model_understanding)
model_parameters(model_understanding, ci = 0.95, ci_method = "satterthwaite", digits = "signif4")

understanding_pairwise <- run_planned_contrasts(
  model_understanding,
  at = list(z_age = 0)
)

print(understanding_pairwise)


# Mental workload

model_nasa_full_ml <- lmer(
  nasa ~
    condition * z_bfi_openness +
    z_age +
    z_bfi_extraversion +
    z_bfi_agreeableness +
    z_bfi_conscientiousness +
    z_bfi_neuroticism +
    z_nfc_score +
    z_tri_score +
    z_driving_frequency_num +
    z_automation_familiarity +
    (1 | participant_id),
  data = df,
  REML = FALSE
)

model_nasa_reduced_ml <- lmer(
  nasa ~
    condition * z_bfi_openness +
    z_age +
    z_tri_score +
    (1 | participant_id),
  data = df,
  REML = FALSE
)

anova(
  model_nasa_reduced_ml,
  model_nasa_full_ml
)

model_nasa <- update(
  model_nasa_reduced_ml,
  REML = TRUE
)

summary(model_nasa)
model_parameters(model_nasa, ci = 0.95, ci_method = "satterthwaite", digits = "signif4")

nasa_pairwise <- run_planned_contrasts(
  model_nasa
)

print(nasa_pairwise)


# Interaction plots

plot_trust <- plot_interaction(
  model = model_trust,
  moderator = "z_bfi_openness",
  x_labels = c(
    "Lower Openness\n(−1 SD)",
    "Higher Openness\n(+1 SD)"
  ),
  x_title = NULL,
  y_title = "Estimated Trust",
  file_name = "interaction_trust_openness.pdf"
)

plot_understanding <- plot_interaction(
  model = model_understanding,
  moderator = "z_age",
  x_labels = c(
    "Younger Age\n(−1 SD)",
    "Older Age\n(+1 SD)"
  ),
  x_title = NULL,
  y_title = "Estimated System Understanding",
  file_name = "interaction_understanding_age.pdf"
)

print(plot_trust)
print(plot_understanding)
