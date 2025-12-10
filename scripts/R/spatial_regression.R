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

# ============================================================================
# PRINCIPAL COMPONENTS ANALYSIS (Census Only)
# ============================================================================

# Census variables PCA
pca_census <- census_data_pct |>
  select(-c(1:10)) |>
  prcomp(scale. = TRUE)

factor_loadings_census <- pca_census$rotation |>
  as.data.frame() |>
  mutate(variable = rownames(pca_census$rotation)) |>
  arrange(PC1)

# Join census PCA results to main dataset
# Create census PCA results as regular data frame
census_pca_results <- census_data_pct |>
  select(GEOID) |>
  bind_cols(as.data.frame(pca_census$x)) |>
  rename_with(~ paste0("census_", .x), starts_with("PC"))

# Join census PCA to main dataset
joined_data_pca_sf <- joined_data_sf |>
  left_join(census_pca_results, by = "GEOID") |>
  st_as_sf()

cat("\n=== Using Frey Average Vote Share as Dependent Variable ===\n")
cat("Variable: fry_sh_ (average across 2017, 2021, 2025 final rounds)\n")
cat("Range: [", round(min(joined_data_pca_sf$fry_sh_, na.rm = TRUE), 2), 
    ", ", round(max(joined_data_pca_sf$fry_sh_, na.rm = TRUE), 2), "]\n")
cat("Mean: ", round(mean(joined_data_pca_sf$fry_sh_, na.rm = TRUE), 2), "%\n\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Map plotting function
map_plot <- function(data, variable, title = NULL) {
  ggplot(data, aes(fill = !!sym(variable), geometry = geometry)) +
    geom_sf(color = "white", size = 0.1) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b"
    ) +
    labs(
      title = paste0(title, " by Census Tract"),
      fill = title
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

# ============================================================================
# EXPLORATORY ANALYSIS
# ============================================================================

# Plot Frey average vote share map
map_plot(joined_data_sf, "fry_sh_", "Frey Average Vote Share")
mapview_map_plot(joined_data_sf, "fry_sh_", "Frey Average Vote Share")


# Calculate Moran's I for Frey Average Vote Share
moran_test_frey <- moran.test(joined_data_pca_sf$fry_sh_, lw)
print(moran_test_frey)

# ============================================================================
# SPATIAL REGRESSION MODELS
# ============================================================================

# Define predictor variables

predictor_vars <- c(
  "pct_white", "pct_bachelors_plus", "med_hh_income", "pct_higher_ed", "pct_english_only",
  "pct_owner_occupied", "pct_wfh", "pct_transit", "pct_drive_alone",
  "pct_rent_burdened", "pct_built_post2000", "pct_multifamily",
  "pct_moved_2021_later", "pct_same_sex_hh",
  "pct_age_18_34", "pct_age_65_plus", "unemployment_rate"
)

predictor_vars_census <- paste0("census_PC", 1:10)

# Linear Regression (OLS)
formula_linear <- as.formula(paste("fry_sh_ ~", paste(predictor_vars, collapse = " + ")))
model_linear <- lm(formula_linear, data = joined_data_pca_sf)
print(summary(model_linear))

joined_data_pca_sf$linear_resids <- model_linear$residuals
map_plot(joined_data_pca_sf, "linear_resids", "Linear Regression Residuals (Frey Average Vote Share)")

moran_test_linear_resids <- moran.test(joined_data_pca_sf$linear_resids, lw)
print(moran_test_linear_resids)

# SAR Model with Raw Census Variables
model_sar <- errorsarlm(formula_linear, data = joined_data_pca_sf, listw = lw, Durbin = FALSE)
print(summary(model_sar))

joined_data_pca_sf$sp_resids <- model_sar$residuals
map_plot(joined_data_pca_sf, "sp_resids", "Spatial Autoregressive Residuals (Frey Average Vote Share)")

moran_test_sar_resids <- moran.test(joined_data_pca_sf$sp_resids, lw)
print(moran_test_sar_resids)

# SAR Model with Census PCA
formula_census <- as.formula(paste("fry_sh_ ~", paste(predictor_vars_census, collapse = " + ")))
model_sar_census <- errorsarlm(formula_census, data = joined_data_pca_sf, listw = lw, Durbin = FALSE)
print(summary(model_sar_census))

joined_data_pca_sf$sp_resids_census <- model_sar_census$residuals
map_plot(joined_data_pca_sf, "sp_resids_census", "Spatial Autoregressive Residuals w/ Census PCA (Frey Average Vote Share)")

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
metrics_sar <- extract_model_metrics(model_sar, moran_test_sar_resids, "sar")
metrics_sar_census <- extract_model_metrics(
  model_sar_census,
  moran.test(model_sar_census$residuals, lw),
  "sar"
)

# Create comparison table
model_comparison <- data.frame(
  Model = c("Linear (OLS)", "SAR (Raw Census)", "SAR (PCA)"),
  AIC = c(metrics_linear$aic, metrics_sar$aic, metrics_sar_census$aic),
  BIC = c(metrics_linear$bic, metrics_sar$bic, metrics_sar_census$bic),
  RMSE = c(metrics_linear$rmse, metrics_sar$rmse, metrics_sar_census$rmse),
  MAE = c(metrics_linear$mae, metrics_sar$mae, metrics_sar_census$mae),
  Lambda = c(NA, metrics_sar$lambda, metrics_sar_census$lambda),
  Moran_I_Residuals = c(metrics_linear$moran_i, metrics_sar$moran_i, metrics_sar_census$moran_i),
  Moran_I_Pvalue = c(metrics_linear$moran_p, metrics_sar$moran_p, metrics_sar_census$moran_p)
)

# Print comparison
cat("\n")
cat("=", strrep("=", 78), "\n", sep = "")
cat("MODEL COMPARISON: Accuracy Measures\n")
cat("=", strrep("=", 78), "\n", sep = "")
print(model_comparison, digits = 4, row.names = FALSE)

cat("\nNotes:\n")
cat("  - R_Squared: For linear model, this is standard R-squared.\n")
cat("    For SAR/SAC models, this is correlation-based R-squared (squared correlation\n")
cat("    between fitted and actual values).\n")
cat("  - Lambda: Spatial error coefficient (for SAR and SAC models).\n")
cat("    Indicates strength of spatial autocorrelation in errors.\n")
cat("  - Rho: Spatial lag coefficient (for SAC models only).\n")
cat("    Indicates strength of spatial dependence in the dependent variable.\n")
cat("    SAC models include both spatial error (lambda) and spatial lag (rho).\n")
cat("  - Moran_I_Residuals: Spatial autocorrelation in residuals.\n")
cat("    Values closer to 0 indicate better spatial error handling.\n")
cat("  - Lower AIC/BIC/RMSE/MAE indicates better model fit.\n")
cat("  - Higher R-squared indicates better model fit.\n")
cat("  - Lower Moran's I in residuals indicates better spatial error correction.\n")
cat("\n")
