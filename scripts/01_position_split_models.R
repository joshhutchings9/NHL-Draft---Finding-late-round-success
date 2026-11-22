# ============================================================
# NHL Late-Round Draft Success — Analysis by Position Group
# Forwards (C, LW, RW, W, F, C/LW, C/RW) vs. Defensemen (D)
# Josh Hutchings | University of Mississippi
# ============================================================

# --- 0. Packages --------------------------------------------
packages <- c("readxl", "dplyr", "ggplot2", "pROC", "broom",
              "car", "patchwork")
installed <- rownames(installed.packages())
for (p in packages) {
  if (!p %in% installed) install.packages(p, repos = "https://cloud.r-project.org")
}
invisible(lapply(packages, library, character.only = TRUE))

dir.create("output", showWarnings = FALSE)

# --- 1. Load & clean ----------------------------------------
df_raw <- read_excel("data/OMG_Final_Data.xlsx", sheet = "Sheet1")

df <- df_raw %>%
  rename(
    Position       = Pos,
    Height         = `Height (inches)`,
    Weight         = `Weight (lbs)`,
    Draft_Hit_Raw  = `DRAFT_HIT?`,
    PPG_ADJ        = PRE_PPG_ADJ,
    POINT_PROG_ADJ = PRE_POINT_PROG_ADJ
  ) %>%
  mutate(
    DRAFT_HIT = ifelse(Draft_Hit_Raw == "Yes", 1, 0),
    # Group positions: D = defensemen, F = all forwards
    POS_GROUP = case_when(
      Position == "D"              ~ "Defensemen",
      Position %in% c("C", "LW", "RW", "W", "F", "C/LW", "C/RW") ~ "Forwards",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(DRAFT_HIT), !is.na(POS_GROUP))

model_vars <- c("Height", "Weight", "PPG_ADJ", "POINT_PROG_ADJ")

df_model <- df %>%
  select(all_of(model_vars), DRAFT_HIT, POS_GROUP) %>%
  filter(complete.cases(.)) %>%
  filter(PPG_ADJ > 0)   # remove zero-stat players (complete separation)

cat("\n=== Sample Breakdown by Position Group ===\n")
df_model %>%
  group_by(POS_GROUP) %>%
  summarise(
    N        = n(),
    Hits     = sum(DRAFT_HIT),
    Hit_Rate = paste0(round(mean(DRAFT_HIT) * 100, 1), "%")
  ) %>%
  print()

# --- 2. Helper functions ------------------------------------

run_model <- function(data, group_name) {
  cat("\n\n============================================================\n")
  cat("  GROUP:", group_name, "\n")
  cat("============================================================\n")

  formula <- DRAFT_HIT ~ Height + Weight + PPG_ADJ + POINT_PROG_ADJ
  model   <- glm(formula, data = data, family = binomial(link = "logit"))

  cat("\n--- Logistic Regression Summary ---\n")
  print(summary(model))

  or_table <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))

  cat("\n--- Odds Ratios (95% CI) ---\n")
  print(or_table)

  null_model    <- glm(DRAFT_HIT ~ 1, data = data, family = binomial)
  mcfadden_r2   <- as.numeric(1 - (logLik(model) / logLik(null_model)))
  n             <- nrow(data)
  nagelkerke_r2 <- (1 - exp((deviance(model) - deviance(null_model)) / n)) /
                   (1 - exp(-deviance(null_model) / n))

  data$pred_prob <- predict(model, type = "response")

  hl_groups <- data %>%
    mutate(decile = ntile(pred_prob, 10)) %>%
    group_by(decile) %>%
    summarise(obs_hits = sum(DRAFT_HIT), exp_hits = sum(pred_prob),
              obs_miss = n() - obs_hits,  exp_miss = n() - exp_hits)
  hl_stat <- sum((hl_groups$obs_hits - hl_groups$exp_hits)^2 / hl_groups$exp_hits +
                 (hl_groups$obs_miss - hl_groups$exp_miss)^2 / hl_groups$exp_miss)
  hl_p    <- pchisq(hl_stat, df = 8, lower.tail = FALSE)

  roc_obj <- roc(data$DRAFT_HIT, data$pred_prob, quiet = TRUE)

  cat("\n--- Model Fit ---\n")
  cat("N:", n, "| Hits:", sum(data$DRAFT_HIT),
      sprintf("(%.1f%%)\n", mean(data$DRAFT_HIT) * 100))
  cat("McFadden R²    :", round(mcfadden_r2, 4), "\n")
  cat("Nagelkerke R²  :", round(nagelkerke_r2, 4), "\n")
  cat("AUC            :", round(auc(roc_obj), 4), "\n")
  cat("Hosmer-Lemeshow: Chi-sq =", round(hl_stat, 3),
      "| p =", round(hl_p, 4), "\n")

  cat("\n--- VIF ---\n")
  print(round(vif(model), 3))

  cat("\n--- Results Table ---\n")
  results_tbl <- or_table %>%
    filter(term != "(Intercept)") %>%
    transmute(
      Variable = case_when(
        term == "Height"         ~ "Height (in)",
        term == "Weight"         ~ "Weight (lbs)",
        term == "PPG_ADJ"        ~ "Adj. PPG",
        term == "POINT_PROG_ADJ" ~ "Adj. Point Progression",
        TRUE                     ~ term
      ),
      `B (log-OR)` = round(log(estimate), 4),
      OR            = round(estimate, 4),
      `95% CI Low`  = round(conf.low, 4),
      `95% CI High` = round(conf.high, 4),
      `p-value`     = round(p.value, 4),
      Sig           = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ ".",
        TRUE            ~ ""
      )
    )
  print(results_tbl, row.names = FALSE)

  list(model = model, or_table = or_table, roc_obj = roc_obj,
       data = data, nagelkerke = nagelkerke_r2, auc = auc(roc_obj))
}

# --- 3. Run models ------------------------------------------
df_fwd <- df_model %>% filter(POS_GROUP == "Forwards")
df_def <- df_model %>% filter(POS_GROUP == "Defensemen")

res_fwd <- run_model(df_fwd, "FORWARDS")
res_def <- run_model(df_def, "DEFENSEMEN")

# --- 4. Plots -----------------------------------------------
theme_nhl <- theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(size = 10, color = "gray40"),
        axis.title      = element_text(size = 11),
        legend.position = "bottom",
        strip.text      = element_text(face = "bold", size = 12))

fwd_color <- "#3A7FBF"
def_color <- "#2E8B57"
miss_color <- "#E07B54"

# Combine both groups for faceted plots
df_plot <- bind_rows(
  res_fwd$data %>% mutate(Group = "Forwards"),
  res_def$data %>% mutate(Group = "Defensemen")
) %>%
  mutate(Hit_Label = factor(DRAFT_HIT, levels = c(0,1),
                            labels = c("Miss (<180 GP)", "Hit (\u2265180 GP)")))

# 4a. Predictor distribution violin plots (faceted by position group)
vbox_facet <- function(var, title, subtitle = "") {
  ggplot(df_plot, aes(x = Hit_Label, y = .data[[var]], fill = Hit_Label)) +
    geom_violin(alpha = 0.45, trim = TRUE) +
    geom_boxplot(width = 0.18, outlier.size = 0.7, outlier.alpha = 0.35,
                 fill = "white", color = "gray30") +
    scale_fill_manual(values = c("Miss (<180 GP)" = miss_color,
                                 "Hit (\u2265180 GP)"  = fwd_color)) +
    facet_wrap(~Group) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = title, fill = NULL) +
    theme_nhl
}

p_ht   <- vbox_facet("Height",        "Height (inches)")
p_wt   <- vbox_facet("Weight",        "Weight (lbs)")
p_ppg  <- vbox_facet("PPG_ADJ",       "Adj. Points Per Game", "NHLe-normalized PPG")
p_prog <- vbox_facet("POINT_PROG_ADJ","Adj. Point Progression",
                     "Draft-yr PPG / prior-yr PPG (NHLe adj.)")

grid_dist <- (p_ht | p_wt) / (p_ppg | p_prog) +
  plot_annotation(
    title    = "Predictor Distributions by Outcome & Position Group",
    subtitle = "Orange = Miss (<180 NHL GP)  |  Blue = Hit (\u2265180 NHL GP)",
    theme    = theme(plot.title    = element_text(face = "bold", size = 15),
                     plot.subtitle = element_text(size = 11, color = "gray40"))
  )
ggsave("output/plot_01_distributions_by_position.png", grid_dist,
       width = 14, height = 10, dpi = 150)
cat("\nSaved: output/plot_01_distributions_by_position.png\n")

# 4b. Odds ratio forest plots — side by side
make_or_plot <- function(or_table, group_name, color) {
  or_table %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = case_when(
        term == "Height"         ~ "Height (in)",
        term == "Weight"         ~ "Weight (lbs)",
        term == "PPG_ADJ"        ~ "Adj. PPG",
        term == "POINT_PROG_ADJ" ~ "Adj. Point Prog.",
        TRUE                     ~ term
      ),
      sig       = ifelse(p.value < 0.05, "p < 0.05", "p \u2265 0.05"),
      conf.low  = pmax(conf.low,  0.01),
      conf.high = pmin(conf.high, 15)
    ) %>%
    filter(is.finite(estimate)) %>%
    ggplot(aes(x = estimate, y = reorder(term, estimate), color = sig)) +
    geom_vline(xintercept = 1, linetype = "dashed",
               color = "gray50", linewidth = 0.8) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  width = 0.25, linewidth = 0.9, orientation = "y") +
    geom_point(size = 3.5) +
    scale_color_manual(values = c("p < 0.05" = color,
                                  "p \u2265 0.05" = "#999999")) +
    scale_x_log10() +
    labs(title    = paste("Odds Ratios \u2014", group_name),
         subtitle = "Odds of reaching 180 NHL GP  |  Error bars = 95% CI",
         x        = "Odds Ratio (log scale)",
         y        = NULL,
         color    = "Significance") +
    theme_nhl
}

p_or_fwd <- make_or_plot(res_fwd$or_table, "Forwards",   fwd_color)
p_or_def <- make_or_plot(res_def$or_table, "Defensemen", def_color)

grid_or <- (p_or_fwd | p_or_def) +
  plot_annotation(title = "Odds Ratios by Position Group",
                  theme = theme(plot.title = element_text(face = "bold", size = 15)))
ggsave("output/plot_02_odds_ratios_by_position.png", grid_or,
       width = 14, height = 6, dpi = 150)
cat("Saved: output/plot_02_odds_ratios_by_position.png\n")

# 4c. ROC curves — side by side
make_roc_plot <- function(roc_obj, group_name, color) {
  roc_df <- data.frame(fpr = 1 - rev(roc_obj$specificities),
                       tpr = rev(roc_obj$sensitivities))
  ggplot(roc_df, aes(x = fpr, y = tpr)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "gray60", linewidth = 0.8) +
    geom_line(color = color, linewidth = 1.2) +
    annotate("text", x = 0.65, y = 0.12,
             label = paste0("AUC = ", round(auc(roc_obj), 3)),
             size = 5, fontface = "bold", color = color) +
    labs(title    = paste("ROC Curve \u2014", group_name),
         subtitle = "Predicting 180+ NHL GP  |  Dashed = random chance",
         x        = "1 \u2013 Specificity (False Positive Rate)",
         y        = "Sensitivity (True Positive Rate)") +
    theme_nhl
}

p_roc_fwd <- make_roc_plot(res_fwd$roc_obj, "Forwards",   fwd_color)
p_roc_def <- make_roc_plot(res_def$roc_obj, "Defensemen", def_color)

grid_roc <- (p_roc_fwd | p_roc_def) +
  plot_annotation(title = "ROC Curves by Position Group",
                  theme = theme(plot.title = element_text(face = "bold", size = 15)))
ggsave("output/plot_03_roc_by_position.png", grid_roc,
       width = 12, height = 6, dpi = 150)
cat("Saved: output/plot_03_roc_by_position.png\n")

# 4d. Predicted probability density — faceted
df_dens <- bind_rows(
  res_fwd$data %>% mutate(Group = "Forwards"),
  res_def$data %>% mutate(Group = "Defensemen")
)

p_density <- ggplot(df_dens, aes(x = pred_prob, fill = factor(DRAFT_HIT))) +
  geom_density(alpha = 0.55) +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             color = "gray30", linewidth = 0.7) +
  scale_fill_manual(values = c("0" = miss_color, "1" = fwd_color),
                    labels  = c("Miss (<180 GP)", "Hit (\u2265180 GP)")) +
  facet_wrap(~Group) +
  labs(title    = "Predicted Probability Distribution by Position Group",
       subtitle = "Density of model-predicted probabilities by actual outcome",
       x        = "Predicted Probability of Being a Draft Hit",
       y        = "Density", fill = NULL) +
  theme_nhl
ggsave("output/plot_04_prob_density_by_position.png", p_density,
       width = 12, height = 5, dpi = 150)
cat("Saved: output/plot_04_prob_density_by_position.png\n")

# 4e. Scatter plots with logistic regression curve
#     One per predictor, faceted by position group
make_scatter <- function(var, xlab, subtitle = "") {
  df_s <- bind_rows(
    res_fwd$data %>% mutate(Group = "Forwards"),
    res_def$data %>% mutate(Group = "Defensemen")
  )
  ggplot(df_s, aes(x = .data[[var]], y = DRAFT_HIT)) +
    geom_jitter(aes(color = factor(DRAFT_HIT)),
                height = 0.04, alpha = 0.25, size = 0.9) +
    geom_smooth(method  = "glm",
                method.args = list(family = binomial),
                color   = "gray20",
                fill    = "gray70",
                linewidth = 1.1,
                se      = TRUE) +
    scale_color_manual(values = c("0" = miss_color, "1" = fwd_color),
                       labels  = c("Miss", "Hit"),
                       name    = NULL) +
    scale_y_continuous(breaks = c(0, 1),
                       labels = c("Miss (0)", "Hit (1)")) +
    facet_wrap(~Group) +
    labs(title    = paste("Scatter + Logistic Curve \u2014", xlab),
         subtitle = subtitle,
         x        = xlab,
         y        = "Draft Hit (Binary)") +
    theme_nhl
}

p_s_ht   <- make_scatter("Height",        "Height (inches)")
p_s_wt   <- make_scatter("Weight",        "Weight (lbs)")
p_s_ppg  <- make_scatter("PPG_ADJ",       "Adj. Points Per Game",
                         "NHLe-normalized PPG")
p_s_prog <- make_scatter("POINT_PROG_ADJ","Adj. Point Progression",
                         "Draft-yr PPG / prior-yr PPG (NHLe adj.)")

grid_scatter <- (p_s_ht | p_s_wt) / (p_s_ppg | p_s_prog) +
  plot_annotation(
    title    = "Logistic Regression Curves by Predictor & Position Group",
    subtitle = "Points jittered for visibility  |  Shaded band = 95% CI",
    theme    = theme(plot.title    = element_text(face = "bold", size = 15),
                     plot.subtitle = element_text(size = 11, color = "gray40"))
  )
ggsave("output/plot_05_scatter_logistic_curves.png", grid_scatter,
       width = 14, height = 10, dpi = 150)
cat("Saved: output/plot_05_scatter_logistic_curves.png\n")

cat("\nAll done. Five plots saved to output/.\n")
