library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(mapview)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis/scripts/")

# ============================================================================
# DATA LOADING
# ============================================================================

# Load census data (both percentage and raw counts in one pass)
census_data <- read_csv("../data/processed/census/mpls_tracts_census.csv", show_col_types = FALSE) |>
  mutate(GEOID = as.character(GEOID))

# Extract percentage variables for main analysis
census_data_pct <- census_data |>
  select(c(1:10, 91, contains("pct_"), unemployment_rate))

# Extract raw count variables for LISA cluster analysis
census_data_raw <- census_data |>
  select(GEOID,
         # Race variables
         race_total, race_white, race_black, race_asian,
         # Hispanic variables
         hisp_total, hisp_latino,
         # Education variables (population 25+)
         educ_total, educ_bachelors, educ_masters, educ_professional, educ_doctorate,
         # School enrollment variables
         school_total, school_undergrad, school_grad,
         # Age variables
         starts_with("age_"),
         # Housing variables
         tenure_total, tenure_owner, tenure_renter,
         rent_burden_total, rent_burden_30_34, rent_burden_35_39,
         rent_burden_40_49, rent_burden_50_plus,
         # Language variables
         lang_total, lang_english_only,
         # Income (median)
         med_hh_income)

# Load Frey vote share shapefile
frey_data_sf <- read_sf("../data/shapefiles/tracts/mpls_tract_frey_votes.shp") |>
  st_transform(26915)

# Join all data sources
joined_data_sf <- census_data_pct |>
  left_join(frey_data_sf, by = "GEOID") |>
  st_as_sf()

# Create spatial weights matrix
nb <- poly2nb(joined_data_sf$geometry)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Define predictor variables

predictor_vars <- c(
  "pct_white", "pct_bachelors_plus", "med_hh_income", "pct_higher_ed", "pct_english_only",
  "pct_owner_occupied", "pct_rent_burdened", "pct_built_post2000", "pct_multifamily",
  "pct_moved_2021_later", "pct_same_sex_hh",
  "pct_age_18_34", "pct_age_65_plus", "unemployment_rate"
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Map plotting function
map_plot <- function(data, variable, title = NULL, subtitle = title) {
  ggplot(data, aes(fill = !!sym(variable), geometry = geometry)) +
    geom_sf(color = "white", size = 0.1) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b"
    ) +
    labs(
      title = paste0(title, " by Census Tract"),
      fill = subtitle
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

mapview_map_plot <- function(data, variable, title = NULL) {
  # Create color palette matching map_plot gradient2 scale
  # low="#2166ac", mid="#f7f7f7", high="#b2182b"
  color_palette <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))
  
  # Create tooltip label with title and rounded value (1 decimal place)
  label_values <- paste0(title, ": ", round(data[[variable]], 1))
  
  mapview(
    data,
    zcol = variable,
    layer.name = title,
    legend = TRUE,
    col.regions = color_palette(100),
    label = label_values
  )
}


# Calculate percentages from aggregated counts
calculate_percentages <- function(data) {
  data |>
    mutate(
      pct_white = (race_white_sum / race_total_sum) * 100,
      pct_black = (race_black_sum / race_total_sum) * 100,
      pct_asian = (race_asian_sum / race_total_sum) * 100,
      pct_hispanic = (hisp_latino_sum / hisp_total_sum) * 100,
      pct_bachelors_plus = ((educ_bachelors_sum + educ_masters_sum + educ_professional_sum + educ_doctorate_sum) / educ_total_sum) * 100,
      pct_higher_ed = ((school_undergrad_sum + school_grad_sum) / school_total_sum) * 100,
      pct_age_0_17 = (age_0_17_sum / age_total_sum) * 100,
      pct_age_18_34 = (age_18_34_sum / age_total_sum) * 100,
      pct_age_35_49 = (age_35_49_sum / age_total_sum) * 100,
      pct_age_50_64 = (age_50_64_sum / age_total_sum) * 100,
      pct_age_65_plus = (age_65_plus_sum / age_total_sum) * 100,
      pct_owner_occupied = (tenure_owner_sum / tenure_total_sum) * 100,
      pct_rent_burdened = (rent_burden_30_plus_sum / rent_burden_total_sum) * 100,
      pct_english_only = (lang_english_only_sum / lang_total_sum) * 100
    )
}

# Calculate descriptive statistics and Moran's I for a variable
calculate_var_stats <- function(var_name, data, listw) {
  var_values <- data[[var_name]]
  
  # Calculate descriptive statistics
  mean_val <- mean(var_values, na.rm = TRUE)
  median_val <- median(var_values, na.rm = TRUE)
  min_val <- min(var_values, na.rm = TRUE)
  max_val <- max(var_values, na.rm = TRUE)
  sd_val <- sd(var_values, na.rm = TRUE)
  
  # Calculate Moran's I
  moran_result <- moran.test(var_values, listw)
  moran_i <- moran_result$estimate[1]
  moran_p <- moran_result$p.value
  
  # Return as data frame row
  data.frame(
    Variable = var_name,
    Mean = mean_val,
    Median = median_val,
    Min = min_val,
    Max = max_val,
    SD = sd_val,
    Moran_I = moran_i,
    Moran_I_Pvalue = moran_p,
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# EXPLORATORY ANALYSIS
# ============================================================================

# Plot Frey average vote share map
map_plot(joined_data_sf, "fry_sh_", title = "Frey Average Vote Share", subtitle = "Avg. Share")
mapview_map_plot(joined_data_sf, "fry_sh_", "Frey Average Vote Share")


# Calculate Moran's I for Frey Average Vote Share
moran_test_frey <- moran.test(joined_data_sf$fry_sh_, lw)
print(moran_test_frey)

# ============================================================================
# VARIABLE DESCRIPTIVE STATISTICS AND SPATIAL AUTOCORRELATION
# ============================================================================

# Dependent Variable Statistics
cat("\n")
cat("=", strrep("=", 78), "\n", sep = "")
cat("DEPENDENT VARIABLE: DESCRIPTIVE STATISTICS AND SPATIAL AUTOCORRELATION\n")
cat("=", strrep("=", 78), "\n", sep = "")

dep_var_stats <- calculate_var_stats("fry_sh_", joined_data_sf, lw)
cat("Variable: fry_sh_ (Frey Average Vote Share)\n")
cat("  Mean:   ", sprintf("%.4f", dep_var_stats$Mean), "\n", sep = "")
cat("  Median: ", sprintf("%.4f", dep_var_stats$Median), "\n", sep = "")
cat("  Min:    ", sprintf("%.4f", dep_var_stats$Min), "\n", sep = "")
cat("  Max:    ", sprintf("%.4f", dep_var_stats$Max), "\n", sep = "")
cat("  SD:     ", sprintf("%.4f", dep_var_stats$SD), "\n", sep = "")
cat("  Moran's I: ", sprintf("%.4f", dep_var_stats$Moran_I), " (p-value: ", 
    format.pval(dep_var_stats$Moran_I_Pvalue, digits = 4), ")\n", sep = "")
cat("\n")

# Predictor Variables Statistics
cat("=", strrep("=", 78), "\n", sep = "")
cat("PREDICTOR VARIABLES: DESCRIPTIVE STATISTICS AND SPATIAL AUTOCORRELATION\n")
cat("=", strrep("=", 78), "\n", sep = "")

# Calculate statistics for all predictor variables
predictor_stats_list <- lapply(predictor_vars, function(var) {
  calculate_var_stats(var, joined_data_sf, lw)
})
predictor_stats_table <- do.call(rbind, predictor_stats_list)

# Print the table
print(predictor_stats_table, digits = 4, row.names = FALSE)
cat("\n")

# Save statistics tables to CSV files
write_csv(dep_var_stats, "../outputs/spatial_regression_dependent_var_stats.csv")
write_csv(predictor_stats_table, "../outputs/spatial_regression_predictor_vars_stats.csv")
cat("Statistics tables saved to outputs/ directory:\n")
cat("  - spatial_regression_dependent_var_stats.csv\n")
cat("  - spatial_regression_predictor_vars_stats.csv\n\n")

# ============================================================================
# SPATIAL REGRESSION MODELS
# ============================================================================

# Linear Regression (OLS)
formula_linear <- as.formula(paste("fry_sh_ ~", paste(predictor_vars, collapse = " + ")))
model_linear <- lm(formula_linear, data = joined_data_sf)
print(summary(model_linear))

# Extract and save OLS model summary
linear_summary <- summary(model_linear)

# Extract coefficients
ols_coefficients <- as.data.frame(linear_summary$coefficients)
ols_coefficients$Variable <- rownames(ols_coefficients)
ols_coefficients <- ols_coefficients |>
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) |>
  rename(
    StdError = `Std. Error`,
    TValue = `t value`,
    PValue = `Pr(>|t|)`
  )

# Extract Moran's I for OLS residuals
moran_test_ols_resids <- moran.test(model_linear$residuals, lw)
print(moran_test_ols_resids)

# Extract fit statistics
ols_fit_stats <- data.frame(
  Statistic = c("R-squared", "Adjusted R-squared", "F-statistic", "F p-value", "AIC", "BIC", "Residual SE", "Moran's I", "Moran's I p-value"),
  Value = c(
    linear_summary$r.squared,
    linear_summary$adj.r.squared,
    linear_summary$fstatistic[1],
    pf(linear_summary$fstatistic[1], linear_summary$fstatistic[2], linear_summary$fstatistic[3], lower.tail = FALSE),
    AIC(model_linear),
    BIC(model_linear),
    linear_summary$sigma,
    moran_test_ols_resids$estimate[1],
    format.pval(moran_test_ols_resids$p.value, digits = 4)
  )
)

# Save to CSV
write_csv(ols_coefficients, "../outputs/spatial_regression_ols_coefficients.csv")
write_csv(ols_fit_stats, "../outputs/spatial_regression_ols_fit_stats.csv")
cat("OLS model summaries saved to outputs/ directory:\n")
cat("  - spatial_regression_ols_coefficients.csv\n")
cat("  - spatial_regression_ols_fit_stats.csv\n\n")

joined_data_sf$linear_resids <- model_linear$residuals

# Plot OLS residuals on map and save to file
ols_resid_map <- map_plot(
  joined_data_sf,
  "linear_resids",
  "Linear Regression Residuals\n (Frey Average Vote Share)",
  "Residual"
)

# Save plot to PNG for website output
ggsave(
  filename = "../website-source/resources/plots/ols_residuals.png",
  plot = ols_resid_map,
  width = 7,
  height = 7,
  dpi = 300
)

moran_test_linear_resids <- moran.test(joined_data_sf$linear_resids, lw)
print(moran_test_linear_resids)

# Spatial Error Model (SEM) with Raw Census Variables
model_sem <- errorsarlm(formula_linear, data = joined_data_sf, listw = lw, Durbin = FALSE)
print(summary(model_sem))

# Extract and save SEM model summary
sem_summary <- summary(model_sem)

# Extract coefficients
sem_coefficients <- as.data.frame(sem_summary$Coef)
sem_coefficients$Variable <- rownames(sem_coefficients)
sem_coefficients <- sem_coefficients |>
  select(Variable, Estimate, `Std. Error`, `z value`, `Pr(>|z|)`) |>
  rename(
    StdError = `Std. Error`,
    ZValue = `z value`,
    PValue = `Pr(>|z|)`
  )

joined_data_sf$sem_resids <- model_sem$residuals

# Plot SEM residuals on map and save to file
sem_resid_map <- map_plot(
  joined_data_sf,
  "sem_resids",
  "Spatial Error Model Residuals\n (Frey Average Vote Share)",
  "Residual"
)

# Save plot to PNG for website output
ggsave(
  filename = "../website-source/resources/plots/sem_residuals.png",
  plot = sem_resid_map,
  width = 7,
  height = 7,
  dpi = 300
)

# Extract Moran's I for SEM residuals
moran_test_sem_resids <- moran.test(model_sem$residuals, lw)
print(moran_test_sem_resids)


# Extract fit statistics including lambda
# Calculate pseudo R-squared as correlation between fitted and observed values
pseudo_r2 <- cor(model_sem$y, model_sem$fitted.values, use = "complete.obs")^2

sem_fit_stats <- data.frame(
  Statistic = c("Pseudo R-squared", "AIC", "BIC", "Log-likelihood", "Lambda", "Lambda Std Error", "Lambda z-value", "Lambda p-value", "Moran's I", "Moran's I p-value"),
  Value = c(
    pseudo_r2,
    AIC(model_sem),
    BIC(model_sem),
    as.numeric(logLik(model_sem)),
    sem_summary$lambda,
    sem_summary$lambda.se,
    sem_summary$lambda / sem_summary$lambda.se,
    2 * (1 - pnorm(abs(sem_summary$lambda / sem_summary$lambda.se))),
    moran_test_sem_resids$estimate[1],
    format.pval(moran_test_sem_resids$p.value, digits = 4)
  )
)

# Save to CSV
write_csv(sem_coefficients, "../outputs/spatial_regression_sem_coefficients.csv")
write_csv(sem_fit_stats, "../outputs/spatial_regression_sem_fit_stats.csv")
cat("SEM model summaries saved to outputs/ directory:\n")
cat("  - spatial_regression_sem_coefficients.csv\n")
cat("  - spatial_regression_sem_fit_stats.csv\n\n")

joined_data_sf$sp_resids <- model_sem$residuals
map_plot(joined_data_sf, "sp_resids", "Spatial Error Model Residuals\n (Frey Average Vote Share)", "Residual")

moran_test_sem_resids <- moran.test(joined_data_sf$sp_resids, lw)
print(moran_test_sem_resids)

# ============================================================================
# MODEL COMPARISON: Accuracy Measures
# ============================================================================

# Helper function to extract model metrics
extract_model_metrics <- function(model, moran_test, model_type = "linear") {
  if (model_type == "linear") {
    model_summary <- summary(model)
    r2 <- model_summary$r.squared
    adj_r2 <- model_summary$adj.r.squared
    lambda <- NA
    rho <- NA
  } else if (model_type == "sac") {
    r2 <- cor(model$y, model$fitted.values, use = "complete.obs")^2
    adj_r2 <- NA
    lambda <- model$lambda
    rho <- model$rho
  } else {
    r2 <- cor(model$y, model$fitted.values, use = "complete.obs")^2
    adj_r2 <- NA
    lambda <- model$lambda
    rho <- NA
  }
  
  list(
    r2 = r2,
    adj_r2 = adj_r2,
    aic = AIC(model),
    bic = BIC(model),
    rmse = sqrt(mean(model$residuals^2, na.rm = TRUE)),
    mae = mean(abs(model$residuals), na.rm = TRUE),
    lambda = lambda,
    rho = rho,
    moran_i = moran_test$estimate[1],
    moran_p = moran_test$p.value
  )
}

# Extract metrics for all models
metrics_linear <- extract_model_metrics(model_linear, moran_test_linear_resids, "linear")
metrics_sem <- extract_model_metrics(model_sem, moran_test_sem_resids, "sem")

# Create comparison table
model_comparison <- data.frame(
  Model = c("Linear (OLS)", "Spatial Error Model (SEM)"),
  AIC = c(metrics_linear$aic, metrics_sem$aic),
  BIC = c(metrics_linear$bic, metrics_sem$bic),
  RMSE = c(metrics_linear$rmse, metrics_sem$rmse),
  MAE = c(metrics_linear$mae, metrics_sem$mae),
  Lambda = c(NA, metrics_sem$lambda),
  Moran_I_Residuals = c(metrics_linear$moran_i, metrics_sem$moran_i),
  Moran_I_Pvalue = c(metrics_linear$moran_p, metrics_sem$moran_p)
)

# Print comparison
cat("\n")
cat("=", strrep("=", 78), "\n", sep = "")
cat("MODEL COMPARISON: Accuracy Measures\n")
cat("=", strrep("=", 78), "\n", sep = "")
print(model_comparison, digits = 4, row.names = FALSE)

cat("\nNotes:\n")
cat("  - R_Squared: For linear model, this is standard R-squared.\n")
cat("    For SEM models, this is correlation-based R-squared (squared correlation\n")
cat("    between fitted and actual values).\n")
cat("  - Lambda: Spatial error coefficient (for SEM models).\n")
cat("    Indicates strength of spatial autocorrelation in errors.\n")
cat("  - Moran_I_Residuals: Spatial autocorrelation in residuals.\n")
cat("    Values closer to 0 indicate better spatial error handling.\n")
cat("  - Lower AIC/BIC/RMSE/MAE indicates better model fit.\n")
cat("  - Higher R-squared indicates better model fit.\n")
cat("  - Lower Moran's I in residuals indicates better spatial error correction.\n")
cat("\n")
