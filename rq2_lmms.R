library(lmerTest)
library(emmeans)
library(readr)

emm_options(lmer.df = "satterthwaite")

df <- read_csv("../DATA/blocks.csv")
df$participant_id <- factor(df$participant_id)
df$condition <- factor(df$condition, levels = c("Coda", "Lumo", "Nelo"))

run_planned_contrasts_with_CI <- function(model, side) {
  op <- c(">" = ">", "<" = "<", "two-sided" = "vs.")[[side]]
  weights <- list(
    c(Coda = 0, Lumo = 1, Nelo = -1),
    c(Coda = 1, Lumo = 0, Nelo = -1),
    c(Coda = -1, Lumo = 1, Nelo = 0)
  )
  names(weights) <- paste(
    c("Pull", "Push", "Pull"), op, c("Baseline", "Baseline", "Push")
  )
  
  contrasts <- contrast(emmeans(model, ~condition), method = weights)
  results <- as.data.frame(test(contrasts, side = side, adjust = "holm"))
  intervals <- as.data.frame(confint(contrasts))
  
  ci_columns <- grep("CL$", names(intervals), value = TRUE)
  results[ci_columns] <- intervals[ci_columns]
  results
}

sides <- c(trust = ">", nasa = "<", understanding = ">")

for (outcome in names(sides)) {
  model <- lmer(
    reformulate(
      c("condition", "factor(block)", "route", "(1 | participant_id)"),
      response = outcome
    ),
    data = df,
    REML = TRUE
  )
  print(summary(model))
  print(run_planned_contrasts_with_CI(model, sides[[outcome]]))
}