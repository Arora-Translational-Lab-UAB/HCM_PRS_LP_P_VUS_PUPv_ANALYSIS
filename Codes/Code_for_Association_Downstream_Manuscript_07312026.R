# ==============================================================================
# Step 1: Package Loading and Setup
# ==============================================================================

if("package:plyr" %in% search()) detach("package:plyr", unload=TRUE)
if("package:rcompanion" %in% search()) detach("package:rcompanion", unload=TRUE)

suppressPackageStartupMessages({
  library(parallel)
  library(fmsb)
  library(pROC)
  library(dplyr)
  library(stats)
  library(rlang)
  library(icenReg)
  library(survival)
  library(purrr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(tableone)
})

set.seed(42)
cat("Step 1 Complete: Packages loaded successfully.\n")


# ==============================================================================
# Step 2 & 3 (Revised): Dummy Dataset Generation with All Outcomes and PRS Categories
# ==============================================================================
library(dplyr)
library(tableone)

n <- 50000
cat("Generating dummy dataset with", n, "participants and all outcomes and comorbidities...\n")

set.seed(42)
person_id <- paste0("ID_", 100000 + (1:n))
age <- pmax(18, pmin(90, round(rnorm(n, mean = 52.7, sd = 12.0), 1)))
age2 <- age^2
female <- rbinom(n, size = 1, prob = 0.618)

ancestry_pred <- sample(c("eur", "afr", "amr", "asn"), size = n, replace = TRUE, prob = c(0.60, 0.187, 0.175, 0.037))
ancestry <- ifelse(ancestry_pred %in% c("asn", "mid"), "asn", ancestry_pred)

pc_matrix <- matrix(rnorm(n * 10, mean = 0, sd = 1), nrow = n)
colnames(pc_matrix) <- sprintf("pca_features_%02d", 1:10)

autos_sum_std <- rnorm(n, mean = 0, sd = 1)
eur_auto_sum_std <- rnorm(n, mean = 0, sd = 1)
lp_p_carriers <- rbinom(n, size = 1, prob = 0.003)
vus_carriers  <- rbinom(n, size = 1, prob = 0.040)

# Simulate comorbidities matching manuscript proportions (Table 1)
hypertension <- rbinom(n, size = 1, prob = 0.298)
diabetes     <- rbinom(n, size = 1, prob = 0.216)
obesity      <- rbinom(n, size = 1, prob = 0.301)
ckd          <- rbinom(n, size = 1, prob = 0.099)

# Simulate main phenotypes and adverse outcomes
logit_hcm <- -5.5 + 0.03 * age + 0.55 * autos_sum_std + 2.8 * lp_p_carriers + 0.8 * vus_carriers
hcm <- rbinom(n, size = 1, prob = 1 / (1 + exp(-logit_hcm)))
dcm <- rbinom(n, size = 1, prob = 1 / (1 + exp(-(-4.8 + 0.02 * age - 0.20 * autos_sum_std + 1.5 * lp_p_carriers))))
hf  <- rbinom(n, size = 1, prob = 1 / (1 + exp(-(-3.0 + 0.04 * age + 0.15 * autos_sum_std + 1.2 * lp_p_carriers))))
arry_ccd <- rbinom(n, size = 1, prob = 0.113)
death    <- rbinom(n, size = 1, prob = 0.013)
scd_hospital <- rbinom(n, size = 1, prob = 0.005)
composite <- ifelse((hf + arry_ccd + death + scd_hospital) > 0, 1, 0)

# Assemble base dummy dataset
dummy_data <- data.frame(
  person_id, age, age2, female, ancestry_pred, ancestry,
  autos_sum_std, eur_auto_sum_std,
  lp_p_carriers, vus_carriers, 
  hypertension, diabetes, obesity, ckd,
  hcm, dcm, hf, arry_ccd, death, scd_hospital, composite,
  hcm_age = ifelse(hcm == 1, pmin(85, age + runif(n, 1, 15)), 85),
  stringsAsFactors = FALSE
) %>% cbind(as.data.frame(pc_matrix)) %>%
  mutate(
    hcm_start = ifelse(hcm == 1, 0, age),
    hcm_end = ifelse(hcm == 1, hcm_age, Inf)
  )

# Categorize PRS scores and joint risk classifications for Figure 5a/5b context
dummy_data <- dummy_data %>%
  mutate(
    prs_quintile = cut(
      autos_sum_std,
      breaks = quantile(autos_sum_std, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("Q", 1:5)
    ),
    prs_category = case_when(
      autos_sum_std <= quantile(autos_sum_std, 0.20, na.rm = TRUE) ~ "Low PRS (<20%)",
      autos_sum_std >= quantile(autos_sum_std, 0.80, na.rm = TRUE) ~ "High PRS (>80%)",
      TRUE ~ "Intermediate PRS (20-80%)"
    ),
    prs_category = factor(prs_category, levels = c("Low PRS (<20%)", "Intermediate PRS (20-80%)", "High PRS (>80%)")),
    sarc_hcm_status = ifelse(lp_p_carriers == 1, "Carrier", "Non-Carrier"),
    genetic_risk_category = paste0(sarc_hcm_status, " + ", prs_category),
    
    # Formatter labels matching Table 1 layout
    ancestry_label = case_when(
      ancestry_pred == "eur" ~ "European",
      ancestry_pred == "afr" ~ "African",
      ancestry_pred == "amr" ~ "Admixed",
      ancestry_pred == "asn" ~ "Asian",
      TRUE ~ "Other"
    ),
    ancestry_label = factor(ancestry_label, levels = c("European", "African", "Admixed", "Asian")),
    
    self_reported_race_label = case_when(
      ancestry_pred == "eur" ~ "Non-Hispanic White",
      ancestry_pred == "afr" ~ "Non-Hispanic Black",
      ancestry_pred == "amr" ~ "Hispanic",
      TRUE ~ "Others"
    ),
    self_reported_race_label = factor(self_reported_race_label, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Others")),
    
    sex_label = ifelse(female == 1, "Female", "Male"),
    sex_label = factor(sex_label, levels = c("Male", "Female"))
  )

cat("Dummy dataset created successfully with N =", nrow(dummy_data), "including comorbidities and outcomes.\n")

# ==============================================================================
# Step 4: Baseline Table Generation
# ==============================================================================
catVars <- c(
  "sex_label", 
  "ancestry_label", 
  "self_reported_race_label", 
  "hypertension", 
  "diabetes", 
  "obesity", 
  "ckd", 
  "hcm", 
  "dcm", 
  "hf", 
  "arry_ccd", 
  "death",
  "scd_hospital",
  "composite"
)

contVars <- c("age", "autos_sum_std")
allVars <- c(catVars, contVars)

table1_final <- CreateTableOne(
  vars = allVars,
  data = dummy_data,
  factorVars = catVars,
  strata = "prs_category",
  includeOverall = TRUE,
  test = TRUE
)

table1_matrix <- print(
  table1_final,
  showAllLevels = TRUE,
  contDigits = 1,
  test = TRUE,
  formatOptions = list(big.mark = ","),
  printToggle = FALSE
)

cat("\n=== Table 1: Baseline Characteristics and Comorbidities Stratified by PRS ===\n")
print(table1_matrix)

# ==============================================================================
# Step 5: Code for Association Pipeline: Evaluating AUCs and Adjusted ORs per SD 
# Across Candidate PRS Models (C+T and PRS-CSx parameter sweeps)
# ==============================================================================
library(dplyr)
library(pROC)
library(fmsb)
library(rcompanion)

# ------------------------------------------------------------------------------
# 5.1. Setup and Standardization
# ------------------------------------------------------------------------------

# Ensure scores are standardized within ancestry groups as per manuscript methods
score_cols <- c("autos_sum_std", "eur_auto_sum_std", "phi_1e04_sum_std", "phi_1e08_sum_std") 

dummy_data <- dummy_data %>%
  group_by(ancestry_pred) %>%
  mutate(across(any_of(score_cols), ~ as.numeric(scale(.)), .names = "{.col}_std")) %>%
  ungroup()

score_cols_std <- paste0(score_cols, "_std")

# Define covariates matching the adjustment model (Age, Age^2, Sex, and Top PCs)
covariates <- c('age', 'age2', 'female', paste0("pca_features_", sprintf("%02d", 1:10)))

# Fit Null Baseline Model for Incremental Nagelkerke R2 Comparison
null_formula <- as.formula(paste("hcm ~", paste(covariates, collapse = " + ")))
null_model <- glm(null_formula, data = dummy_data, family = binomial(link = "logit"))
null_r2 <- NagelkerkeR2(null_model)$R2

# ------------------------------------------------------------------------------
# 5.2. Generate/Simulate Candidate Scores (45 Total)
# ------------------------------------------------------------------------------
set.seed(42)
n_obs <- nrow(dummy_data)

pval_options <- c("1", "0.05", "5e-04", "5e-06", "5e-07")
r2_options <- c("0.2", "0.5", "0.8", "0.9")
window_options <- c("250kb", "500kb")

ct_score_names <- c()
for (p in pval_options) {
  for (r in r2_options) {
    for (w in window_options) {
      col_name <- paste0("ct_pval_", p, "_r2_", r, "_win_", w, "_std")
      dummy_data[[col_name]] <- rnorm(n_obs, mean = 0, sd = 1)
      ct_score_names <- c(ct_score_names, col_name)
    }
  }
}

prscsx_phi_options <- c("1e_02", "1e_04", "1e_06", "1e_08", "auto")
prscsx_score_names <- c()
for (phi in prscsx_phi_options) {
  col_name <- paste0("prscsx_phi_", phi, "_std")
  mean_val <- if(phi == "auto") 0.1 else 0.05
  dummy_data[[col_name]] <- rnorm(n_obs, mean = mean_val, sd = 1)
  prscsx_score_names <- c(prscsx_score_names, col_name)
}

all_45_scores <- c(ct_score_names, prscsx_score_names)

# ------------------------------------------------------------------------------
# 5.3. Define Association Evaluation Function
# ------------------------------------------------------------------------------
evaluate_prs_performance <- function(data, scores, covars, baseline_r2) {
  results_list <- lapply(scores, function(score_var) {
    tryCatch({
      adj_formula <- as.formula(paste("hcm ~", score_var, "+", paste(covars, collapse = " + ")))
      model <- glm(adj_formula, data = data, family = binomial(link = "logit"))
      
      full_r2 <- NagelkerkeR2(model)$R2
      delta_r2 <- full_r2 - baseline_r2
      
      preds <- predict(model, type = "response")
      roc_obj <- roc(data$hcm, preds, quiet = TRUE)
      auc_val <- as.numeric(auc(roc_obj))
      auc_ci <- ci.auc(roc_obj)
      
      coef_est <- coef(summary(model))[score_var, "Estimate"]
      se_est <- coef(summary(model))[score_var, "Std. Error"]
      or_val <- exp(coef_est)
      ci_lower <- exp(coef_est - 1.96 * se_est)
      ci_upper <- exp(coef_est + 1.96 * se_est)
      p_val <- summary(model)$coefficients[score_var, "Pr(>|z|)"]
      
      data.frame(
        Score = score_var,
        Adjusted_R2 = round(full_r2, 4),
        Nagelkerke_Delta_R2 = round(delta_r2, 4),
        AUC = round(auc_val, 3),
        AUC_CI = sprintf("%.2f (%.2f - %.2f)", auc_val, auc_ci[1], auc_ci[3]),
        Adjusted_OR_CI = sprintf("%.2f (%.2f - %.2f)", or_val, ci_lower, ci_upper),
        P_Value = p_val,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message(sprintf("Skipping score %s due to error: %s", score_var, e$message))
      return(NULL)
    })
  })
  
  bind_rows(results_list)
}

# ------------------------------------------------------------------------------
# 5.4. Execute Pipeline and Export Results
# ------------------------------------------------------------------------------
candidate_performance_table <- evaluate_prs_performance(dummy_data, all_45_scores, covariates, null_r2)

cat("\n=== All 45 Candidate PRS Performance Summary (Top 10 by AUC) ===\n")
print(head(candidate_performance_table %>% arrange(desc(AUC)), 10))

write.csv(candidate_performance_table, "all_45_candidate_prs_performance_results.csv", row.names = FALSE)



# ==============================================================================
# Step 6: Penetrance, Prevalence across PRS Quintiles, Fold Change, and Group Comparisons
# ==============================================================================

library(dplyr)

# 1. Ensure PRS Quintiles are assigned within the dataset
dummy_data <- dummy_data %>%
  mutate(
    prs_quintile_std = cut(
      autos_sum_std,
      breaks = quantile(autos_sum_std, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("Q", 1:5)
    ),
    # Define predicted deleterious variant carriers for extended analysis
    deleterious_carriers = rbinom(n(), size = 1, prob = 0.011)
  )

# 2. Function to compute prevalence across quintiles and fold change (High Q5 vs Low Q1)
calculate_penetrance_metrics <- function(data, subgroup_name, subgroup_filter_expr) {
  sub_df <- data %>% filter(!!rlang::parse_expr(subgroup_filter_expr))
  
  prev_summary <- sub_df %>%
    group_by(prs_quintile_std) %>%
    summarise(
      N = n(),
      Cases = sum(hcm, na.rm = TRUE),
      Prevalence_Percent = 100 * mean(hcm, na.rm = TRUE),
      .groups = "drop"
    )
  
  q1_prev <- prev_summary$Prevalence_Percent[prev_summary$prs_quintile_std == "Q1"]
  q5_prev <- prev_summary$Prevalence_Percent[prev_summary$prs_quintile_std == "Q5"]
  fold_change <- if(length(q1_prev) > 0 && length(q5_prev) > 0 && q1_prev > 0) round(q5_prev / q1_prev, 1) else NA
  
  cat(sprintf("\n--- Subgroup: %s ---\n", subgroup_name))
  print(prev_summary)
  cat(sprintf("Fold Change (Q5 vs Q1): %.1f×\n", fold_change))
  
  return(prev_summary)
}

# Evaluate across required subgroups
overall_penetrance <- calculate_penetrance_metrics(dummy_data, "Overall Population", "TRUE")
lpp_penetrance     <- calculate_penetrance_metrics(dummy_data, "SARC-HCM-P/LP Carriers", "lp_p_carriers == 1")
vus_penetrance     <- calculate_penetrance_metrics(dummy_data, "SARC VUS Carriers", "vus_carriers == 1")
del_penetrance     <- calculate_penetrance_metrics(dummy_data, "Predicted Deleterious Variant Carriers", "deleterious_carriers == 1")

# 3. Compare Mean PRS values between HCM cases and non-cases using two-sided t-tests
compare_mean_prs <- function(data, subgroup_name, subgroup_filter_expr) {
  sub_df <- data %>% filter(!!rlang::parse_expr(subgroup_filter_expr))
  
  t_res <- t.test(autos_sum_std ~ hcm, data = sub_df, alternative = "two.sided", var.equal = TRUE)
  
  mean_cases <- mean(sub_df$autos_sum_std[sub_df$hcm == 1], na.rm = TRUE)
  mean_controls <- mean(sub_df$autos_sum_std[sub_df$hcm == 0], na.rm = TRUE)
  
  cat(sprintf("\nPRS Mean Comparison (%s):\n", subgroup_name))
  cat(sprintf("  Cases Mean: %.4f | Non-Cases Mean: %.4f\n", mean_cases, mean_controls))
  cat(sprintf("  t-statistic = %.3f | p-value = %.5e\n", t_res$statistic, t_res$p.value))
}

compare_mean_prs(dummy_data, "Overall Population", "TRUE")
compare_mean_prs(dummy_data, "SARC-HCM-P/LP Carriers", "lp_p_carriers == 1")
compare_mean_prs(dummy_data, "SARC VUS Carriers", "vus_carriers == 1")
compare_mean_prs(dummy_data, "Predicted Deleterious Variant Carriers", "deleterious_carriers == 1")


# ==============================================================================
# Step 7: Function and Execution: Multi-Panel Penetrance & Prevalence Plotting
# ==============================================================================

library(dplyr)
library(ggplot2)
library(cowplot)

# --- 1. Custom Theme Definition ---
my_cowplot_theme <- theme_cowplot(font_family = "sans") +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  )

# --- 2. Main Plotting and Saving Function ---
generate_plots_and_save <- function(data, data_name, filename_suffix) {
  
  # --- A. Summarize prevalence with confidence intervals ---
  prev_summary <- data %>%
    group_by(prs_quintile_std) %>%
    summarise(
      n = n(),
      cases = sum(hcm),
      proportion = mean(hcm), 
      prevalence = 100 * proportion, 
      se = sqrt(proportion * (1 - proportion) / n),
      lower = pmax(0, prevalence - 1.96 * se * 100),
      upper = pmin(100, prevalence + 1.96 * se * 100),
      .groups = "drop"
    )
  
  # Calculate fold change (Q5 / Q1)
  q5_prev <- prev_summary$prevalence[prev_summary$prs_quintile_std == "Q5"]
  q1_prev <- prev_summary$prevalence[prev_summary$prs_quintile_std == "Q1"]
  
  if (length(q5_prev) == 0 || length(q1_prev) == 0 || q1_prev == 0) {
    fold_label <- "Fold change: N/A"
    max_y_limit <- 1.1 * max(prev_summary$upper, na.rm = TRUE)
  } else {
    fold_change <- round(q5_prev / q1_prev, 1)
    fold_label <- paste0("Fold change = ", fold_change, "×")
    max_y_limit <- 1.1 * max(prev_summary$upper, na.rm = TRUE)
  }
  
  # --- B. Plot 1: Prevalence Barplot ---
  plot1 <- ggplot(prev_summary, aes(x = prs_quintile_std, y = prevalence)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
    geom_text(aes(label = sprintf("%.2f%%", prevalence)), vjust = -0.5, size = 4) +
    annotate("text", 
             x = ifelse(nrow(prev_summary) > 3, nrow(prev_summary) - 1, 3.5), 
             y = max_y_limit * 0.85, 
             label = fold_label, 
             size = 6, 
             fontface = "bold") +
    labs(x = "PGS quantile", y = "HCM penetrance (%)", title = data_name) +
    scale_y_continuous(limits = c(0, max_y_limit)) +
    my_cowplot_theme
  
  # --- C. Plot 2: PRS Density Plot ---
  plot2 <- ggplot(data, aes(x = autos_sum_std, color = factor(hcm), fill = factor(hcm))) +
    geom_density(alpha = 0.3) +
    labs(
      title = "Distribution of PRS by HCM Status",
      x = "Standardized PRS",
      y = "Density",
      color = "HCM Status",
      fill = "HCM Status"
    ) +
    my_cowplot_theme
  
  # --- D. Combine and Save Plots ---
  combined_plot <- plot_grid(
    plot1, plot2,
    labels = c("A", "B"),
    label_fontfamily = "Arial",
    label_fontface = "bold",
    label_size = 20,
    align = "hv",
    axis = "tblr"
  )
  
  filename <- paste0("combined_plots_with_fold_change_", filename_suffix, "_01212026.svg")
  svg(filename, width = 14, height = 8)
  print(combined_plot)
  dev.off()
  cat(sprintf("Successfully saved plots to: %s\n", filename))
}


# Ensure quintile variable is present in dummy_data
dummy_data$prs_quintile_std <- dummy_data$prs_quintile

# Setup data subsets
final_hcm_prs_scores <- dummy_data
lp_p_2star <- dummy_data %>% filter(lp_p_carriers == 1)
vus_carriers <- dummy_data %>% filter(vus_carriers == 1 & lp_p_carriers == 0)
lp_p_2star_pipeline <- dummy_data %>% filter(lp_p_carriers == 1)

# Run function across all 4 groups
generate_plots_and_save(
  data = final_hcm_prs_scores, 
  data_name = "Overall Population (All Carriers)", 
  filename_suffix = "overall"
)

generate_plots_and_save(
  data = lp_p_2star, 
  data_name = "LP/P Carriers", 
  filename_suffix = "lp_p_carrier"
)

generate_plots_and_save(
  data = vus_carriers, 
  data_name = "VUS Carriers", 
  filename_suffix = "vus_carrier"
)

generate_plots_and_save(
  data = lp_p_2star_pipeline, 
  data_name = "LP/P/PuPv Pipeline Carriers", 
  filename_suffix = "lp_p_pipelines_carrier"
)

cat("Step 7 Complete: Functions and plots executed successfully.\n")


# ==============================================================================
# Step 8: Comprehensive Survival and Interval-Censored Association Analysis Pipeline
# Utilizing icenReg, dplyr, purrr, and parallel processing for multi-outcome modeling
# ==============================================================================

library(dplyr)
library(rlang)
library(purrr)
library(icenReg)
library(doParallel)
library(tidyr)
library(knitr)
library(kableExtra)
library(IRdisplay)

# 1. Define Core Function for Dynamic Interval Phenotype Preparation
prepare_interval_phenotype <- function(data, 
                                       pheno_status_col, 
                                       pheno_age_col, 
                                       output_prefix) {
  
  # Convert strings to symbols for dynamic evaluation
  pheno_status <- sym(pheno_status_col)
  pheno_age <- sym(pheno_age_col)
  
  # Define output column names
  start_name <- paste0(output_prefix, "_start")
  end_name <- paste0(output_prefix, "_end")
  
  data %>%
    mutate(
      # The Start of the Interval
      !!start_name := case_when(
        !!pheno_status == 1 ~ 0,                         # All Cases: Birth to outcome
        !!pheno_status == 0 ~ as.numeric(!!pheno_age)    # Non-cases: Age at last encounter
      ),
      # The End of the Interval
      !!end_name := case_when(
        !!pheno_status == 1 ~ as.numeric(!!pheno_age),   # Cases: Age of outcome
        !!pheno_status == 0 ~ Inf                        # Non-cases: To infinity
      )
    )
}

# 2. Apply Interval Phenotype Preparation across Clinical Outcomes
final_hcm_prs_scores_v2 <- final_hcm_prs_scores_v2 %>%
  prepare_interval_phenotype("hcm", "hcm_age", "hcm") %>%
  prepare_interval_phenotype("hf", "hf_age", "hf") %>%
  prepare_interval_phenotype("dcm", "dcm_age", "dcm") %>%
  prepare_interval_phenotype("arry_ccd1", "arry_ccd_age", "arry_ccd") %>%
  prepare_interval_phenotype("cm", "cm_age", "cm") %>%
  prepare_interval_phenotype("stroke", "stroke_age", "stroke") %>%
  prepare_interval_phenotype("death", "death_age", "death") %>%
  prepare_interval_phenotype("lvad_hf_asa_sep", "lvad_hf_asa_sep_age", "lvad_hf_asa_sep")

# 3. Factor Releveling for Reference Group Control
final_hcm_prs_scores_v2$vus_prs_cat_no_lp_p_v1 <- relevel(
  factor(final_hcm_prs_scores_v2$vus_prs_cat_no_lp_p), 
  ref = "Low PRS + VUS -"
)

# 4. Set Up Parallel Computing Cluster
n_cores <- parallel::detectCores() - 1 
registerDoParallel(cores = n_cores)

# 5. Define Multi-Outcome Interval-Censored Regression Wrapper
get_ic_summary_multi <- function(predictor, outcome_prefix, data) {
  
  start_col <- paste0(outcome_prefix, "_start")
  end_col <- paste0(outcome_prefix, "_end")
  
  pca_vars <- paste0("pca_features_", sprintf("%02d", 1:16))
  required_vars <- c(start_col, end_col, predictor, "female", pca_vars)
  
  data_filtered <- data %>%
    select(all_of(required_vars)) %>%
    filter(if_all(everything(), ~ !is.na(.))) %>%
    mutate(across(all_of(predictor), as.factor))
  
  formula_str <- paste0("cbind(", start_col, ", ", end_col, ") ~ ", predictor, " + female + ", 
                        paste(pca_vars, collapse = " + "))
  
  fit <- tryCatch({
    ic_sp(as.formula(formula_str), 
          model = 'ph', 
          data = data_filtered,
          bs_samples = 10,
          useMCores = TRUE)
  }, error = function(e) return(NULL))
  
  if(is.null(fit)) return(data.frame(Outcome = outcome_prefix, Term = "Model Failed", Model_Source = predictor))

  coef_estimates <- coef(fit)
  se_estimates <- sqrt(diag(vcov(fit)))
  
  data.frame(
    Outcome = outcome_prefix,
    Term = names(coef_estimates),
    Estimate = coef_estimates,
    SE = se_estimates
  ) %>%
    filter(grepl(predictor, Term)) %>%
    mutate(
      Hazard_Ratio = round(exp(Estimate), 2),
      CI_Lower = round(exp(Estimate - 1.96 * SE), 2),
      CI_Upper = round(exp(Estimate + 1.96 * SE), 2),
      Model_Source = predictor
    ) %>%
    select(Outcome, Term, Hazard_Ratio, CI_Lower, CI_Upper, Model_Source)
}

# 6. Execute Grid Search Across Predictors and Outcomes
predictors <- c("lp_p_prs_cat", "lp_p_pipeline_prs_cat", "vus_prs_cat", "vus_prs_cat_no_lp_p_v1")
outcomes <- c("hcm", "hf", "arry_ccd", "dcm", "death")

task_grid <- expand.grid(pred = predictors, out = outcomes, stringsAsFactors = FALSE)

summary_df <- pmap_df(list(task_grid$pred, task_grid$out), 
                     ~get_ic_summary_multi(..1, ..2, final_hcm_prs_scores_v2))

stopImplicitCluster()

# 7. Format and Render Final Summary Results Table
formatted_results <- summary_df %>%
  mutate(
    HR_CI = paste0(sprintf("%.2f", Hazard_Ratio), 
                   " (", sprintf("%.2f", CI_Lower), 
                   "-", sprintf("%.2f", CI_Upper), ")")
  ) %>%
  select(Model_Source, Outcome, Term, HR_CI) %>%
  pivot_wider(
    names_from = Outcome, 
    values_from = HR_CI
  ) %>%
  mutate(Term = gsub("as.factor\\(|\\)|lp_p_prs_cat|vus_prs_cat|lp_p_pipeline_prs_cat", "", Term))

t_html <- formatted_results %>%
  kable(
    caption = "Summary of Lifetime Risk: Hazard Ratios (95% CI) for Cardiomyopathy Outcomes",
    format = "html",
    align = "l"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = F,
    position = "left"
  ) %>%
  pack_rows(index = table(formatted_results$Model_Source))

display_html(as.character(t_html))



# ==============================================================================
# Step 9: Extension Pipeline for Age-Dependent Cumulative Incidence Curves 
# Using Robust Custom Prediction Logic for Interval-Censored Models
# ==============================================================================

library(icenReg)
library(dplyr)

# 1. Comprehensive Manual Cumulative Incidence Plotting Function
plot_incidence_manual_robust <- function(data, hf_start, hf_end, cmp_patho, ylab, filename = "hcm_clean_incidence.svg", max_y = 0.05, y_by = 0.01, y_labels = paste0(seq(0, 5, by = 1), "%")) {
  
  # Strict data cleaning for the selected variables
  data_filtered <- data %>%
    select(all_of(c(hf_start, hf_end, cmp_patho))) %>%
    filter(if_all(everything(), ~ !is.na(.))) %>%
    mutate(across(all_of(cmp_patho), as.factor))
  
  # Fit the Semi-Parametric Proportional Hazards model
  formula_str <- paste0("cbind(", hf_start, ", ", hf_end, ") ~ ", cmp_patho)
  fit <- ic_sp(as.formula(formula_str), data = data_filtered, model = 'ph')
  
  # Set up output SVG device
  svg(filename = filename, width = 10, height = 7)
  par(font.lab = 2, font.axis = 2, lwd = 2, mar = c(5, 5, 4, 2))
  
  # Initialize blank plotting window clipped cleanly to the 18-90 age span
  plot(NULL, xlim = c(18, 90), ylim = c(0, max_y),
       xlab = "Age in years", ylab = ylab, 
       main = "Cumulative Incidence (Corrected)", axes = FALSE)
  
  lvls <- levels(data_filtered[[cmp_patho]])
  line_colors <- c('#377EB8', '#4DAF4A', '#E41A1C', '#984EA3', '#FF7F00', '#A65628')[1:length(lvls)]
  plot_ages <- seq(18, 90, by = 0.5)
  
  # Iteratively compute and plot cumulative incidence curves for each stratum
  for(i in seq_along(lvls)) {
    pred_df <- data.frame(temp = factor(lvls[i], levels = lvls))
    colnames(pred_df) <- cmp_patho
    
    surv_probs <- predict(fit, newdata = pred_df, times = plot_ages, type = "lp")
    incidence <- 1 - as.numeric(surv_probs)
    
    lines(plot_ages, incidence, col = line_colors[i], lwd = 3)
  }
  
  # Draw customized bold axes with a graph break style if plotrix is available
  axis(1, at = seq(20, 90, by = 10), lwd = 2)
  if (requireNamespace("plotrix", quietly = TRUE)) {
    plotrix::axis.break(1, breakpos = 18.5, style = "slash")
  }
  
  axis(2, at = seq(0, max_y, by = y_by), 
       labels = y_labels, 
       lwd = 2, las = 1)
  
  box(lwd = 2)
  legend("topleft", legend = lvls, col = line_colors, 
         lwd = 3, bty = "n", cex = 0.85, text.font = 2)
  
  dev.off()
  cat("Successfully generated and saved clean incidence curve:", filename, "\n")
}

library(icenReg)
library(dplyr)

# 1. Ensure factor labels/levels are prepared for all 4 main groupings
# Category 1: LP/P Carriers (SARC-HCM P/LP)
final_hcm_prs_scores_v2$lp_p_prs_cat_v1 <- factor(
  final_hcm_prs_scores_v2$lp_p_prs_cat,
  levels = c("Low PRS + Lp/P -","Low PRS + Lp/P +","Int PRS + Lp/P -","Int PRS + Lp/P +","High PRS + Lp/P -","High PRS + Lp/P +"),
  labels = c("SARC HCM P/LP Non Carriers + Low PRS (<20%)", "SARC HCM P/LP Carriers + Low PRS (<20%)",
             "SARC HCM P/LP Non Carriers + Intermediate PRS (20-80%)", "SARC HCM P/LP Carriers + Intermediate PRS (20-80%)",
             "SARC HCM P/LP Non Carriers + High PRS (>80%)", "SARC HCM P/LP Carriers + High PRS (>80%)")
)

# Category 2: VUS Carriers (No LP/P)
final_hcm_prs_scores_v2$vus_prs_cat_no_lp_p_v1 <- factor(
  final_hcm_prs_scores_v2$vus_prs_cat_no_lp_p,
  levels = c('Low PRS + VUS -','Low PRS + VUS +','Int PRS + VUS -','Int PRS + VUS +','High PRS + VUS -','High PRS + VUS +'),
  labels = c("SARC HCM VUS Non Carriers + Low PRS (<20%)", "SARC HCM VUS Carriers + Low PRS (<20%)",
             "SARC HCM VUS Non Carriers + Intermediate PRS (20-80%)", "SARC HCM VUS Carriers + Intermediate PRS (20-80%)",
             "SARC HCM VUS Non Carriers + High PRS (>80%)", "SARC HCM VUS Carriers + High PRS (>80%)")
)

# Category 3: LP/P Pipeline Carriers (if applicable in data)
if ("lp_p_pipeline_prs_cat" %in% colnames(final_hcm_prs_scores_v2)) {
  final_hcm_prs_scores_v2$lp_p_pipeline_prs_cat_v1 <- factor(
    final_hcm_prs_scores_v2$lp_p_pipeline_prs_cat
  )
}

# Category 4: Standard VUS Carriers (if applicable in data)
if ("vus_prs_cat" %in% colnames(final_hcm_prs_scores_v2)) {
  final_hcm_prs_scores_v2$vus_prs_cat_v1 <- factor(
    final_hcm_prs_scores_v2$vus_prs_cat
  )
}

# 2. Execution for Category 1: SARC-HCM P/LP
plot_incidence_manual_robust(
  data = final_hcm_prs_scores_v2,
  hf_start = "hcm_start",
  hf_end = "hcm_end",
  cmp_patho = "lp_p_prs_cat_v1",
  ylab = "Cumulative Incidence",
  filename = "hcm_6_cat_lp_p_final_prs_incidence_02052026.svg",
  max_y = 0.20,
  y_by = 0.05,
  y_labels = paste0(seq(0, 20, by = 5), "%")
)

# 3. Execution for Category 2: VUS Carriers (No LP/P)
plot_incidence_manual_robust(
  data = final_hcm_prs_scores_v2,
  hf_start = "hcm_start",
  hf_end = "hcm_end",
  cmp_patho = "vus_prs_cat_no_lp_p_v1",
  ylab = "Cumulative Incidence",
  filename = "hcm_6_cat_vus_prs_cat_no_lp_p_final_prs_incidence_02052026.svg",
  max_y = 0.05,
  y_by = 0.01,
  y_labels = paste0(seq(0, 5, by = 1), "%")
)

# 4. Execution for Category 3: LP/P Pipeline Carriers
if ("lp_p_pipeline_prs_cat_v1" %in% colnames(final_hcm_prs_scores_v2)) {
  plot_incidence_manual_robust(
    data = final_hcm_prs_scores_v2,
    hf_start = "hcm_start",
    hf_end = "hcm_end",
    cmp_patho = "lp_p_pipeline_prs_cat_v1",
    ylab = "Cumulative Incidence",
    filename = "hcm_6_cat_lp_p_pipeline_incidence_02052026.svg",
    max_y = 0.20,
    y_by = 0.05,
    y_labels = paste0(seq(0, 20, by = 5), "%")
  )
}

# 5. Execution for Category 4: Standard VUS Carriers
if ("vus_prs_cat_v1" %in% colnames(final_hcm_prs_scores_v2)) {
  plot_incidence_manual_robust(
    data = final_hcm_prs_scores_v2,
    hf_start = "hcm_start",
    hf_end = "hcm_end",
    cmp_patho = "vus_prs_cat_v1",
    ylab = "Cumulative Incidence",
    filename = "hcm_6_cat_vus_prs_incidence_02052026.svg",
    max_y = 0.05,
    y_by = 0.01,
    y_labels = paste0(seq(0, 5, by = 1), "%")
  )
}

cat("Step 9 Complete: Cumulative incidence survival curves successfully generated for all 4 categories.\n")


# ==============================================================================
# Step 10: Complete Within-Carrier Association and Risk-Sorted Summary Pipeline
# ==============================================================================

library(dplyr)
library(rlang)
library(icenReg)
library(doParallel)
library(purrr)
library(tidyr)
library(knitr)
library(kableExtra)
library(IRdisplay)

# 1. Subset HCM Cases and Construct Composite Event Endpoint
hcm_events <- final_hcm_prs_scores_v2 %>% filter(hcm == 1)

hcm_events$composite <- rowSums(hcm_events[, c("hf", "arry_ccd1", "death", "stroke", "proc", "lvad_hf_asa_sep")] > 0, na.rm = TRUE)
hcm_events$composite <- ifelse(hcm_events$composite > 0, 1, 0)

hcm_events$composite_age <- ifelse(
  hcm_events$composite == 1,
  pmin(hcm_events$death_age, hcm_events$stroke_age, hcm_events$hf_age, hcm_events$arry_ccd_age, hcm_events$icd_proc_age, hcm_events$lvad_hf_asa_sep_age, na.rm = TRUE),
  pmax(hcm_events$death_age, hcm_events$stroke_age, hcm_events$hf_age, hcm_events$arry_ccd_age, hcm_events$icd_proc_age, hcm_events$lvad_hf_asa_sep_age, na.rm = TRUE)
)

# 2. Interval Phenotype Preparation Function for Composite Events
prepare_interval_phenotype <- function(data, pheno_status_col, pheno_age_col, output_prefix) {
  pheno_status <- sym(pheno_status_col)
  pheno_age <- sym(pheno_age_col)
  
  start_name <- paste0(output_prefix, "_start")
  end_name <- paste0(output_prefix, "_end")
  
  data %>%
    mutate(
      !!start_name := case_when(
        !!pheno_status == 1 ~ 0,
        !!pheno_status == 0 ~ as.numeric(!!pheno_age)
      ),
      !!end_name := case_when(
        !!pheno_status == 1 ~ as.numeric(!!pheno_age),
        !!pheno_status == 0 ~ Inf
      )
    )
}

hcm_events <- hcm_events %>%
  prepare_interval_phenotype("composite", "composite_age", "composite")

# 3. Define Within-Carrier Multi-Outcome Wrapper Function
get_ic_summary_multi_within <- function(predictor, outcome_prefix, data) {
  
  start_col <- paste0(outcome_prefix, "_start")
  end_col <- paste0(outcome_prefix, "_end")
  
  pca_vars <- paste0("pca_features_", sprintf("%02d", 1:16))
  required_vars <- c(start_col, end_col, predictor, "female", pca_vars)
  
  data_filtered <- data %>%
    select(all_of(required_vars)) %>%
    filter(if_all(everything(), ~ !is.na(.))) %>%
    mutate(across(all_of(predictor), as.factor))
  
  formula_str <- paste0("cbind(", start_col, ", ", end_col, ") ~ ", predictor, " + female + ", 
                        paste(pca_vars, collapse = " + "))
  
  fit <- tryCatch({
    ic_sp(as.formula(formula_str), 
          model = 'ph', 
          data = data_filtered,
          bs_samples = 10,
          useMCores = TRUE)
  }, error = function(e) return(NULL))
  
  if(is.null(fit)) return(data.frame(Outcome = outcome_prefix, Term = "Model Failed", Model_Source = predictor))

  coef_estimates <- coef(fit)
  se_estimates <- sqrt(diag(vcov(fit)))
  
  data.frame(
    Outcome = outcome_prefix,
    Term = names(coef_estimates),
    Estimate = coef_estimates,
    SE = se_estimates
  ) %>%
    filter(grepl(predictor, Term)) %>%
    mutate(
      Hazard_Ratio = round(exp(Estimate), 2),
      CI_Lower = round(exp(Estimate - 1.96 * SE), 2),
      CI_Upper = round(exp(Estimate + 1.96 * SE), 2),
      Model_Source = predictor
    ) %>%
    select(Outcome, Term, Hazard_Ratio, CI_Lower, CI_Upper, Model_Source)
}

# 4. Execute Parallel Processing across Within-Carrier Task Grid
n_cores <- parallel::detectCores() - 1 
registerDoParallel(cores = n_cores)

predictors <- c("lp_p_prs_cat", "lp_p_pipeline_prs_cat", "vus_prs_cat", "vus_prs_cat_no_lp_p_v1")
outcomes <- c("hf", "arry_ccd", "dcm", "composite")

task_grid <- expand.grid(pred = predictors, out = outcomes, stringsAsFactors = FALSE)

summary_df_hcm_subset <- pmap_df(list(task_grid$pred, task_grid$out), 
                     ~get_ic_summary_multi_within(..1, ..2, hcm_events))

stopImplicitCluster()

# 5. Format and Render Structured HTML Results Table Sorted by Risk Category
formatted_results <- summary_df_hcm_subset %>%
  mutate(
    HR_CI = paste0(sprintf("%.2f", Hazard_Ratio), 
                   " (", sprintf("%.2f", CI_Lower), 
                   "-", sprintf("%.2f", CI_Upper), ")"),
    Term_Clean = gsub("as.factor\\(|\\)|lp_p_prs_cat|vus_prs_cat|lp_p_pipeline_prs_cat|vus_prs_cat_no_lp_p_v1", "", Term),
    Risk_Rank = case_when(
      grepl("Low", Term_Clean) ~ 1,
      grepl("Int", Term_Clean) ~ 2,
      grepl("High", Term_Clean) ~ 3,
      TRUE ~ 4
    )
  ) %>%
  mutate(HR_CI = ifelse(grepl("0.00", HR_CI), "—", HR_CI)) %>%
  arrange(Model_Source, Risk_Rank, Term_Clean) %>%
  select(Model_Source, Term_Clean, Outcome, HR_CI) %>%
  pivot_wider(names_from = Outcome, values_from = HR_CI)

display_df <- formatted_results %>% select(-Model_Source)
actual_cols <- colnames(display_df)
friendly_names <- c("Genetic Category", toupper(actual_cols[-1]))

row_group_counts <- rle(as.character(formatted_results$Model_Source))$lengths
row_group_names <- rle(as.character(formatted_results$Model_Source))$values
names(row_group_counts) <- row_group_names

t_html <- display_df %>%
  kable(
    caption = "Consolidated Within-Carrier Lifetime Risk: Ordered by Risk Category",
    format = "html",
    col.names = friendly_names,
    align = "l"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = F,
    position = "left"
  ) %>%
  pack_rows(index = row_group_counts)

display_html(as.character(t_html))

cat("Step 10 Complete: Associations within HCM events were completed.\n")


# ==============================================================================
# Step 11: Ancestry-Stratified Prevalence, Penetrance, and Density Distribution Pipeline
# ==============================================================================

library(dplyr)
library(ggplot2)
library(cowplot)

# 1. Define Custom Theme for Ancestry Plots
ancestry_theme <- theme_cowplot(font_family = "sans") +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  )

# 2. Pipeline Function to Compute Ancestry-Specific Prevalence and Generate Multi-Panel Visualizations
generate_ancestry_stratified_plots <- function(data, ancestry_code, ancestry_label_name) {
  
  # Filter data for specific ancestry group
  sub_data <- data %>% filter(ancestry_pred == ancestry_code)
  
  if (nrow(sub_data) < 50) {
    cat(sprintf("Skipping ancestry %s due to insufficient sample size (N = %d)\n", ancestry_code, nrow(sub_data)))
    return(NULL)
  }
  
  # A. Calculate Prevalence & Confidence Intervals by PRS Quintile within Ancestry
  prev_summary <- sub_data %>%
    group_by(prs_quintile_std) %>%
    summarise(
      n = n(),
      cases = sum(hcm, na.rm = TRUE),
      proportion = mean(hcm, na.rm = TRUE),
      prevalence = 100 * proportion,
      se = sqrt(proportion * (1 - proportion) / n),
      lower = pmax(0, prevalence - 1.96 * se * 100),
      upper = pmin(100, prevalence + 1.96 * se * 100),
      .groups = "drop"
    )
  
  q5_prev <- prev_summary$prevalence[prev_summary$prs_quintile_std == "Q5"]
  q1_prev <- prev_summary$prevalence[prev_summary$prs_quintile_std == "Q1"]
  
  if (length(q5_prev) == 0 || length(q1_prev) == 0 || q1_prev == 0) {
    fold_label <- "Fold change: N/A"
    max_y_limit <- 1.1 * max(prev_summary$upper, na.rm = TRUE)
  } else {
    fold_change <- round(q5_prev / q1_prev, 1)
    fold_label <- paste0("Fold change = ", fold_change, "×")
    max_y_limit <- 1.1 * max(prev_summary$upper, na.rm = TRUE)
  }
  
  # B. Plot 1: Prevalence Barplot with Error Bars
  plot_bar <- ggplot(prev_summary, aes(x = prs_quintile_std, y = prevalence)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
    geom_text(aes(label = sprintf("%.2f%%", prevalence)), vjust = -0.5, size = 4) +
    annotate("text", x = 3.5, y = max_y_limit * 0.85, label = fold_label, size = 5, fontface = "bold") +
    labs(x = "PGS Quantile", y = "HCM Penetrance (%)", title = paste("Penetrance:", ancestry_label_name)) +
    scale_y_continuous(limits = c(0, max_y_limit)) +
    ancestry_theme
  
  # C. Plot 2: PRS Density Distribution by HCM Status within Ancestry
  plot_density <- ggplot(sub_data, aes(x = autos_sum_std, color = factor(hcm), fill = factor(hcm))) +
    geom_density(alpha = 0.3) +
    labs(
      title = paste("PRS Distribution:", ancestry_label_name),
      x = "Standardized PRS",
      y = "Density",
      color = "HCM Status",
      fill = "HCM Status"
    ) +
    ancestry_theme
  
  # D. Combine into Multi-Panel Figure
  combined_plot <- plot_grid(
    plot_bar, plot_density,
    labels = c("A", "B"),
    label_fontfamily = "Arial",
    label_fontface = "bold",
    label_size = 20,
    align = "hv",
    axis = "tblr"
  )
  
  # Export SVG vector graphic
  filename <- paste0("ancestry_stratified_plots_", ancestry_code, ".svg")
  svg(filename, width = 14, height = 8)
  print(combined_plot)
  dev.off()
  
  cat(sprintf("Successfully generated ancestry plots for %s -> Saved as %s\n", ancestry_label_name, filename))
  return(prev_summary)
}

# 3. Execution Across Available Ancestry Groups (e.g., European, African, Admixed, Asian)
ancestry_mapping <- list(
  list(code = "eur", label = "European Ancestry"),
  list(code = "afr", label = "African Ancestry"),
  list(code = "amr", label = "Admixed Ancestry"),
  list(code = "asn", label = "Asian Ancestry")
)

ancestry_prevalence_results <- list()

for (anc in ancestry_mapping) {
  if (anc$code %in% unique(final_hcm_prs_scores_v2$ancestry_pred)) {
    res <- generate_ancestry_stratified_plots(final_hcm_prs_scores_v2, anc$code, anc$label)
    if (!is.null(res)) {
      ancestry_prevalence_results[[anc$code]] <- res
    }
  }
}

cat("\nStep 11 completed: Ancestry-stratified prevalence and density distribution pipeline complete.\n")

# ==============================================================================
# Step 12: Ancestry-Stratified Association Pipeline: AUCs and Adjusted ORs per SD
# ==============================================================================

library(dplyr)
library(pROC)
library(fmsb)
library(rcompanion)

# 1. Define Ancestry Groups and Covariates
ancestry_groups <- unique(final_hcm_prs_scores_v2$ancestry_pred)
covariates <- c('age', 'age2', 'female', paste0("pca_features_", sprintf("%02d", 1:10)))

# 2. Function to compute Ancestry-Specific Performance Metrics (AUC, OR per SD, R2)
run_ancestry_association_metrics <- function(data, score_cols, anc_groups, covars) {
  
  results_list <- list()
  
  for (anc in anc_groups) {
    cat(sprintf("\n--- Calculating Association Metrics for Ancestry: %s ---\n", toupper(anc)))
    
    anc_data <- data %>% filter(ancestry_pred == anc)
    
    if (nrow(anc_data) < 50) {
      cat(sprintf("Skipping %s due to small sample size (N = %d)\n", anc, nrow(anc_data)))
      next
    }
    
    # Fit Null Baseline Model for Ancestry Subgroup
    null_formula <- as.formula(paste("hcm ~", paste(covars, collapse = " + ")))
    null_model <- tryCatch({
      glm(null_formula, data = anc_data, family = binomial(link = "logit"))
    }, error = function(e) return(NULL))
    
    if (is.null(null_model)) next
    null_r2 <- NagelkerkeR2(null_model)$R2
    
    for (score_var in score_cols) {
      res <- tryCatch({
        adj_formula <- as.formula(paste("hcm ~", score_var, "+", paste(covars, collapse = " + ")))
        model <- glm(adj_formula, data = anc_data, family = binomial(link = "logit"))
        
        full_r2 <- NagelkerkeR2(model)$R2
        delta_r2 <- full_r2 - null_r2
        
        # AUC Computation
        preds <- predict(model, type = "response")
        roc_obj <- roc(anc_data$hcm, preds, quiet = TRUE)
        auc_val <- as.numeric(auc(roc_obj))
        auc_ci <- ci.auc(roc_obj)
        
        # Adjusted Odds Ratio (ORadj) per SD increase with 95% CI
        coef_est <- coef(summary(model))[score_var, "Estimate"]
        se_est <- coef(summary(model))[score_var, "Std. Error"]
        or_val <- exp(coef_est)
        ci_lower <- exp(coef_est - 1.96 * se_est)
        ci_upper <- exp(coef_est + 1.96 * se_est)
        p_val <- summary(model)$coefficients[score_var, "Pr(>|z|)"]
        
        data.frame(
          Ancestry = anc,
          Score = score_var,
          N_Observations = nrow(anc_data),
          Adjusted_R2 = round(full_r2, 4),
          Nagelkerke_Delta_R2 = round(delta_r2, 4),
          AUC = round(auc_val, 3),
          AUC_CI = sprintf("%.2f (%.2f - %.2f)", auc_val, auc_ci[1], auc_ci[3]),
          Adjusted_OR_CI = sprintf("%.2f (%.2f - %.2f)", or_val, ci_lower, ci_upper),
          P_Value = p_val,
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(res)) {
        results_list[[length(results_list) + 1]] <- res
      }
    }
  }
  
  bind_rows(results_list)
}

# 3. Execute Association Pipeline for Standardized Scores within Ancestry Groups
score_cols_to_test <- c("autos_sum_std", "eur_auto_sum_std")
ancestry_association_table <- run_ancestry_association_metrics(final_hcm_prs_scores_v2, score_cols_to_test, ancestry_groups, covariates)

# 4. View and Export Summary Table
cat("\n=== Ancestry-Stratified Association Summary (AUC, OR per SD, and R2) ===\n")
print(ancestry_association_table)

write.csv(ancestry_association_table, "ancestry_stratified_association_results.csv", row.names = FALSE)

cat("\nStep 12 completed: Ancestry-stratified associations pipeline complete.\n")


# ==============================================================================
# Step 13: Complete UK Biobank Validation, Association, and Performance Pipeline
# ==============================================================================

library(dplyr)
library(data.table)
library(pROC)
library(fmsb)
library(rcompanion)

# 1. Load UK Biobank Phenotype and Genetic Principal Components
pheno_file <- "ukb_phenotypes_comprehensive.csv"
ukb_data <- fread(pheno_file, data.table = FALSE)

# 2. Extract and Harmonize Covariates for UK Biobank Validation Cohort
ukb_validation_cohort <- ukb_data %>%
  mutate(
    age = age_at_recruitment,
    age2 = age^2,
    female = ifelse(sex == "Female", 1, 0),
    hcm = ifelse(hcm_case_status == 1, 1, 0)
  )

# Ensure required principal component columns exist
for (i in 1:10) {
  ukb_validation_cohort[[paste0("pca_features_", sprintf("%02d", i))]] <- ukb_validation_cohort[[paste0("PC", i)]]
}

# 3. Standardize Polygenic Scores within UK Biobank Ancestry Subgroups
if ("inferred_ancestry" %in% colnames(ukb_validation_cohort)) {
  ukb_validation_cohort <- ukb_validation_cohort %>%
    rename(ancestry_pred = inferred_ancestry)
}

score_cols_to_validate <- c("autos_sum", "eur_auto_sum")

ukb_validation_cohort <- ukb_validation_cohort %>%
  group_by(ancestry_pred) %>%
  mutate(
    across(all_of(score_cols_to_validate), ~ as.numeric(scale(.)), .names = "{.col}_std")
  ) %>%
  ungroup()

score_cols_std <- paste0(score_cols_to_validate, "_std")

# 4. Define Covariates and Fit Null Baseline Model for Incremental R2 Comparison
covariates_ukb <- c('age', 'age2', 'female', paste0("pca_features_", sprintf("%02d", 1:10)))

null_formula <- as.formula(paste("hcm ~", paste(covariates_ukb, collapse = " + ")))
null_model <- glm(null_formula, data = ukb_validation_cohort, family = binomial(link = "logit"))
null_r2 <- NagelkerkeR2(null_model)$R2

# 5. Define Comprehensive Association and Validation Evaluation Function
evaluate_ukb_prs_performance <- function(data, scores, covars, baseline_r2) {
  results_list <- lapply(scores, function(score_var) {
    tryCatch({
      adj_formula <- as.formula(paste("hcm ~", score_var, "+", paste(covars, collapse = " + ")))
      model <- glm(adj_formula, data = data, family = binomial(link = "logit"))
      
      full_r2 <- NagelkerkeR2(model)$R2
      delta_r2 <- full_r2 - baseline_r2
      
      # Area Under the Curve (AUC)
      preds <- predict(model, type = "response")
      roc_obj <- roc(data$hcm, preds, quiet = TRUE)
      auc_val <- as.numeric(auc(roc_obj))
      auc_ci <- ci.auc(roc_obj)
      
      # Adjusted Odds Ratio (ORadj) per standard deviation increase with 95% CIs
      coef_est <- coef(summary(model))[score_var, "Estimate"]
      se_est <- coef(summary(model))[score_var, "Std. Error"]
      or_val <- exp(coef_est)
      ci_lower <- exp(coef_est - 1.96 * se_est)
      ci_upper <- exp(coef_est + 1.96 * se_est)
      p_val <- summary(model)$coefficients[score_var, "Pr(>|z|)"]
      
      data.frame(
        Score = score_var,
        N_Observations = nrow(data),
        Adjusted_R2 = round(full_r2, 4),
        Nagelkerke_Delta_R2 = round(delta_r2, 4),
        AUC = round(auc_val, 3),
        AUC_CI = sprintf("%.2f (%.2f - %.2f)", auc_val, auc_ci[1], auc_ci[3]),
        Adjusted_OR_CI = sprintf("%.2f (%.2f - %.2f)", or_val, ci_lower, ci_upper),
        P_Value = p_val,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message(sprintf("Skipping score %s due to error: %s", score_var, e$message))
      return(NULL)
    })
  })
  
  bind_rows(results_list)
}

# 6. Execute Validation Association Pipeline Across Cohort
ukb_validation_results <- evaluate_ukb_prs_performance(ukb_validation_cohort, score_cols_std, covariates_ukb, null_r2)

cat("\n=== UK Biobank Validation & Association Performance Summary ===\n")
print(ukb_validation_results)

# Save validation outputs and data frame
write.csv(ukb_validation_results, file = "ukb_validation_performance_results.csv", row.names = FALSE)
saveRDS(ukb_validation_cohort, file = "ukb_validated_hcm_prs_cohort.rds")

cat("\nStep 13 completed: Validation in ukbiobank completed.\n")

# ==============================================================================
# Step 14: PRS Distribution by Rare Variant Carrier Status Pipeline
# ==============================================================================

library(dplyr)
library(ggplot2)
library(cowplot)

# 1. Define Shared Publication Theme
carrier_dist_theme <- theme_cowplot(font_family = "sans") +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  )

# 2. Prepare Carrier Classification Labels in final_hcm_prs_scores_v2
final_hcm_prs_scores_v2 <- final_hcm_prs_scores_v2 %>%
  mutate(
    carrier_status_label = case_when(
      lp_p_carriers == 1 ~ "SARC-HCM LP/P Carrier",
      vus_carriers == 1 & lp_p_carriers == 0 ~ "SARC-HCM VUS Carrier",
      TRUE ~ "Non-Carrier / General Population"
    ),
    carrier_status_label = factor(
      carrier_status_label,
      levels = c("Non-Carrier / General Population", "SARC-HCM VUS Carrier", "SARC-HCM LP/P Carrier")
    )
  )

# 3. Generate Density Distribution Plot Comparing PRS Across Carrier Groups
plot_carrier_prs_density <- ggplot(
  final_hcm_prs_scores_v2 %>% filter(!is.na(carrier_status_label)), 
  aes(x = autos_sum_std, color = carrier_status_label, fill = carrier_status_label)
) +
  geom_density(alpha = 0.25, lwd = 1) +
  labs(
    title = "PRS Distribution Stratified by Rare Variant Carrier Status",
    x = "Standardized Polygenic Risk Score (PRS)",
    y = "Density",
    color = "Carrier Status",
    fill = "Carrier Status"
  ) +
  carrier_dist_theme

# 4. Save Vector Graphic Output
svg("prs_distribution_by_carrier_status.svg", width = 12, height = 7)
print(plot_carrier_prs_density)
dev.off()

cat("Successfully generated and saved carrier-stratified PRS distribution plot: prs_distribution_by_carrier_status.svg\n")

cat("\nStep 14 completed: PRS Distribution by carriers status pipeline completed.\n")


# ==============================================================================
# Step 15: Extended Multi-Category Prevalence & Distribution Pipeline for HCM, DCM, and HF
# Across Specific Genetic/PRS Categories (e.g., LP/P and VUS Subgroups)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(svglite)

# 1. Define Custom Fine-Grained Quantile Breaks & Labels
custom_breaks <- c(-Inf, 0.01, 0.05, 0.10, 0.20, 0.40, 0.60, 0.80, 0.90, 0.95, 0.99, Inf)
bin_labels    <- c("<1%", "1-5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-95%", "95-99%", ">99%")

# 2. Function to Process and Plot Multi-Condition Prevalence Across Fine Quantiles
generate_multicondition_distribution_plot <- function(data, subset_filter_expr, subgroup_name, filename_suffix) {
  
  # Filter cohort for specific genetic subgroup
  sub_data <- data %>% filter(!!rlang::parse_expr(subset_filter_expr))
  
  if (nrow(sub_data) < 50) {
    cat(sprintf("Skipping subgroup %s due to small sample size (N = %d)\n", subgroup_name, nrow(sub_data)))
    return(NULL)
  }
  
  # Data Processing & Prevalence per 1,000 calculation
  plot_data <- sub_data %>%
    filter(!is.na(autos_sum_std)) %>%
    mutate(
      score_percentile = percent_rank(autos_sum_std),
      pgs_quantile = cut(score_percentile, breaks = custom_breaks, labels = bin_labels)
    ) %>%
    pivot_longer(cols = c(hcm, dcm, hf), names_to = "Condition", values_to = "Status") %>%
    group_by(pgs_quantile, Condition) %>%
    summarise(
      n = n(),
      cases = sum(Status == 1, na.rm = TRUE),
      prevalence = (cases / n) * 1000,
      se = sqrt((cases / n) * (1 - (cases / n)) / n) * 1000,
      lower = prevalence - (1.96 * se),
      upper = prevalence + (1.96 * se),
      .groups = "drop"
    ) %>%
    mutate(Condition = toupper(Condition))
  
  # Editorial Color Palette
  journal_colors <- c("HCM" = "#008080", "DCM" = "#2A4B7C", "HF" = "#A63A50")
  
  p <- ggplot(plot_data, aes(x = pgs_quantile, y = prevalence, color = Condition, group = Condition)) +
    geom_line(position = position_dodge(0.4), alpha = 0.4, linewidth = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, lower), ymax = upper), 
      width = 0.15, 
      position = position_dodge(0.4),
      linewidth = 0.5,
      alpha = 0.8
    ) +
    geom_point(position = position_dodge(0.4), size = 2.5) +
    facet_wrap(~Condition, scales = "free_y", ncol = 1) + 
    scale_color_manual(values = journal_colors) +
    labs(
      title = paste("Prevalence Gradient by PGS Quantile:", subgroup_name),
      x = "Polygenic Score (PGS) Quantile",
      y = "Prevalence (cases per 1,000 individuals)"
    ) +
    theme_minimal(base_size = 11, base_family = "sans") + 
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 12), color = "#111111", size = 11),
      axis.title.y = element_text(face = "bold", margin = margin(r = 12), color = "#111111", size = 11),
      axis.text.x  = element_text(angle = 45, hjust = 1, color = "#222222", size = 9),
      axis.text.y  = element_text(color = "#222222", size = 9),
      axis.line    = element_line(color = "#333333", linewidth = 0.4),
      axis.ticks   = element_line(color = "#333333", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "#EDEDED", linewidth = 0.4),
      panel.grid.minor.y = element_blank(),
      strip.background = element_rect(fill = "#F4F4F6", color = "#D1D1D6", linewidth = 0.5),
      strip.text       = element_text(face = "bold", size = 11, color = "#111111", margin = margin(t = 6, b = 6)),
      panel.spacing    = unit(1.4, "lines"),
      legend.position = "none"
    )
  
  # Export High-DPI SVG Vector Graphic
  filename <- paste0("pgs_prevalence_distribution_", filename_suffix, ".svg")
  ggsave(filename = filename, plot = p, device = "svg", width = 6.5, height = 8.5)
  cat(sprintf("Successfully generated distribution plot for %s -> Saved as %s\n", subgroup_name, filename))
}

# 3. Execution Across Targeted Subgroups (Overall, LP/P Carriers, VUS Carriers)
generate_multicondition_distribution_plot(final_hcm_prs_scores_v2, "TRUE", "Overall Population", "overall")
generate_multicondition_distribution_plot(final_hcm_prs_scores_v2, "lp_p_carriers == 1", "SARC-HCM LP/P Carriers", "lp_p_carriers")
generate_multicondition_distribution_plot(final_hcm_prs_scores_v2, "vus_carriers == 1 & lp_p_carriers == 0", "SARC-HCM VUS Carriers", "vus_carriers")

cat("\nStep 15 completed: Extended Multi-Category Prevalence & Distribution Pipeline for HCM, DCM, and HF completed.\n")