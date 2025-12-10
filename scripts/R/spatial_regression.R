library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(mapview)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")

# ============================================================================
# DATA LOADING
# ============================================================================

# Load census data (both percentage and raw counts in one pass)
census_data <- read_csv("../data/processed/census/mpls_tracts_census.csv", show_col_types = FALSE) |>
  mutate(GEOID = as.character(GEOID))

# Extract percentage variables for main analysis
census_data_pct <- census_data |>
  select(c(1:10, 91, contains("pct_")))

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

# Load election results
election_results <- read_csv("../data/processed/election_results/tract_election_results_all.csv", show_col_types = FALSE) |>
  mutate(tract_geoid = as.character(tract_geoid))

# Load election shapefile
election_data_sf <- read_sf("../data/shapefiles/tracts/mpls_tract_elections.shp") |>
  st_transform(26915)

# Join all data sources
joined_data_sf <- census_data_pct |>
  left_join(election_data_sf, by = "GEOID") |>
  left_join(election_results, by = c("GEOID" = "tract_geoid")) |>
  st_as_sf()

# Create spatial weights matrix
nb <- poly2nb(joined_data_sf$geometry)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ============================================================================
# PRINCIPAL COMPONENTS ANALYSIS
# ============================================================================

# Election variables PCA
pca_election <- election_results |>
  select(contains("_pct")) |>
  prcomp(scale. = TRUE)

factor_loadings_election <- pca_election$rotation |>
  as.data.frame() |>
  mutate(variable = rownames(pca_election$rotation)) |>
  arrange(PC1)

# Census variables PCA
pca_census <- census_data_pct |>
  select(-c(1:10)) |>
  prcomp(scale. = TRUE)

factor_loadings_census <- pca_census$rotation |>
  as.data.frame() |>
  mutate(variable = rownames(pca_census$rotation)) |>
  arrange(PC1)

# Join PCA results to main dataset
# Create census PCA results as regular data frame
census_pca_results <- census_data_pct |>
  select(GEOID) |>
  bind_cols(as.data.frame(pca_census$x)) |>
  rename_with(~ paste0("census_", .x), starts_with("PC"))

joined_data_pca_sf <- joined_data_sf |>
  left_join(
    election_results |>
      select(tract_geoid) |>
      bind_cols(as.data.frame(pca_election$x)),
    by = c("GEOID" = "tract_geoid")
  ) |>
  left_join(census_pca_results, by = "GEOID") |>
  st_as_sf()

# Rescale election PC1 to 0-100 scale for easier interpretability
pc1_range <- range(joined_data_pca_sf$PC1, na.rm = TRUE)
joined_data_pca_sf <- joined_data_pca_sf |>
  mutate(
    PC1_original = PC1,
    PC1 = ((PC1 - pc1_range[1]) / (pc1_range[2] - pc1_range[1])) * 100
  )

cat("\n=== PC1 Rescaling ===\n")
cat("Original PC1 range: [", round(pc1_range[1], 4), ", ", round(pc1_range[2], 4), "]\n", sep = "")
cat("Rescaled PC1 range: [0, 100]\n")
cat("(PC1_original column preserved for reference)\n\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Map plotting function
pca_map_plot <- function(data, pc, title = NULL) {
  ggplot(data, aes(fill = !!sym(pc), geometry = geometry)) +
    geom_sf(color = "white", size = 0.1) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b"
    ) +
    labs(
      title = paste0(title, " by Census Tract")
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
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

# Plot PC1 map
pca_map_plot(joined_data_pca_sf, "PC1", "Election PC1")

# Calculate Moran's I for Election PC1
moran_test_election <- moran.test(joined_data_pca_sf$PC1, lw)
print(moran_test_election)

# ============================================================================
# LISA CLUSTER ANALYSIS
# ============================================================================

# Calculate Local Moran's I (LISA) for PC1
lisa_pc1 <- localmoran(joined_data_pca_sf$PC1, lw)

# Add LISA results and create cluster classifications
joined_data_pca_sf <- joined_data_pca_sf |>
  mutate(
    lisa_I = lisa_pc1[, 1],
    lisa_p = lisa_pc1[, 5],
    lisa_z = lisa_pc1[, 4],
    pc1_mean = mean(PC1, na.rm = TRUE),
    pc1_lag = lag.listw(lw, PC1),
    pc1_hl = if_else(PC1 >= pc1_mean, "High", "Low"),
    lag_hl = if_else(pc1_lag >= pc1_mean, "High", "Low"),
    significant = lisa_p < 0.05,
    lisa_cluster = case_when(
      !significant ~ "Not Significant",
      pc1_hl == "High" & lag_hl == "High" ~ "High-High",
      pc1_hl == "Low" & lag_hl == "Low" ~ "Low-Low",
      pc1_hl == "High" & lag_hl == "Low" ~ "High-Low",
      pc1_hl == "Low" & lag_hl == "High" ~ "Low-High",
      TRUE ~ "Not Significant"
    ),
    lisa_cluster = factor(
      lisa_cluster,
      levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")
    )
  )

# Print LISA cluster summary
cat("\n=== LISA Cluster Summary for Election PC1 ===\n")
print(table(joined_data_pca_sf$lisa_cluster))
cat("Percent significant:",
    round(100 * sum(joined_data_pca_sf$significant) / nrow(joined_data_pca_sf), 1), "%\n\n")

# Create LISA cluster map
lisa_colors <- c(
  "High-High" = "#d7191c",
  "Low-Low" = "#2b83ba",
  "High-Low" = "#fdae61",
  "Low-High" = "#abdda4",
  "Not Significant" = "#f0f0f0"
)

lisa_map <- ggplot(joined_data_pca_sf) +
  geom_sf(aes(fill = lisa_cluster), color = "white", size = 0.1) +
  scale_fill_manual(name = "LISA Clusters", values = lisa_colors, drop = FALSE) +
  labs(
    title = "LISA Clusters - Election PC1",
    subtitle = paste0(
      "Global Moran's I = ", round(moran_test_election$estimate[1], 3),
      ", p < 0.001"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(lisa_map)

# ============================================================================
# CENSUS CHARACTERISTICS BY LISA CLUSTER
# ============================================================================

# Join raw counts to LISA cluster data
joined_data_with_counts <- joined_data_pca_sf |>
  st_drop_geometry() |>
  select(GEOID, lisa_cluster) |>
  left_join(census_data_raw, by = "GEOID")

# Function to aggregate counts by group
aggregate_counts <- function(data, group_var = "lisa_cluster") {
  data |>
    group_by(across(all_of(group_var))) |>
    summarise(
      n_tracts = n(),
      # Race aggregations
      race_total_sum = sum(race_total, na.rm = TRUE),
      race_white_sum = sum(race_white, na.rm = TRUE),
      race_black_sum = sum(race_black, na.rm = TRUE),
      race_asian_sum = sum(race_asian, na.rm = TRUE),
      # Hispanic aggregations
      hisp_total_sum = sum(hisp_total, na.rm = TRUE),
      hisp_latino_sum = sum(hisp_latino, na.rm = TRUE),
      # Education aggregations
      educ_total_sum = sum(educ_total, na.rm = TRUE),
      educ_bachelors_sum = sum(educ_bachelors, na.rm = TRUE),
      educ_masters_sum = sum(educ_masters, na.rm = TRUE),
      educ_professional_sum = sum(educ_professional, na.rm = TRUE),
      educ_doctorate_sum = sum(educ_doctorate, na.rm = TRUE),
      # School enrollment aggregations
      school_total_sum = sum(school_total, na.rm = TRUE),
      school_undergrad_sum = sum(school_undergrad, na.rm = TRUE),
      school_grad_sum = sum(school_grad, na.rm = TRUE),
      # Age aggregations
      age_total_sum = sum(age_total, na.rm = TRUE),
      age_0_17_sum = sum(
        age_m_under5 + age_m_5_9 + age_m_10_14 + age_m_15_17 +
        age_f_under5 + age_f_5_9 + age_f_10_14 + age_f_15_17,
        na.rm = TRUE
      ),
      age_18_34_sum = sum(
        age_m_18_19 + age_m_20 + age_m_21 + age_m_22_24 + age_m_25_29 + age_m_30_34 +
        age_f_18_19 + age_f_20 + age_f_21 + age_f_22_24 + age_f_25_29 + age_f_30_34,
        na.rm = TRUE
      ),
      age_35_49_sum = sum(
        age_m_35_39 + age_m_40_44 + age_m_45_49 +
        age_f_35_39 + age_f_40_44 + age_f_45_49,
        na.rm = TRUE
      ),
      age_50_64_sum = sum(
        age_m_50_54 + age_m_55_59 + age_m_60_61 + age_m_62_64 +
        age_f_50_54 + age_f_55_59 + age_f_60_61 + age_f_62_64,
        na.rm = TRUE
      ),
      age_65_plus_sum = sum(
        age_m_65_66 + age_m_67_69 + age_m_70_74 + age_m_75_79 + age_m_80_84 + age_m_85_plus +
        age_f_65_66 + age_f_67_69 + age_f_70_74 + age_f_75_79 + age_f_80_84 + age_f_85_plus,
        na.rm = TRUE
      ),
      # Housing aggregations
      tenure_total_sum = sum(tenure_total, na.rm = TRUE),
      tenure_owner_sum = sum(tenure_owner, na.rm = TRUE),
      tenure_renter_sum = sum(tenure_renter, na.rm = TRUE),
      rent_burden_total_sum = sum(rent_burden_total, na.rm = TRUE),
      rent_burden_30_plus_sum = sum(
        rent_burden_30_34 + rent_burden_35_39 + rent_burden_40_49 + rent_burden_50_plus,
        na.rm = TRUE
      ),
      # Language aggregations
      lang_total_sum = sum(lang_total, na.rm = TRUE),
      lang_english_only_sum = sum(lang_english_only, na.rm = TRUE),
      # Income
      median_hh_income_mean = mean(med_hh_income, na.rm = TRUE),
      .groups = "drop"
    ) |>
    calculate_percentages()
}

# Aggregate by LISA cluster
lisa_aggregated_counts <- aggregate_counts(joined_data_with_counts, "lisa_cluster")

# Aggregate for Minneapolis total (create dummy group variable for all data)
mpls_aggregated_counts <- joined_data_with_counts |>
  mutate(lisa_cluster = "Minneapolis Total") |>
  aggregate_counts("lisa_cluster")

# Rename clusters for display
lisa_aggregated_counts <- lisa_aggregated_counts |>
  mutate(
    lisa_cluster = case_when(
      lisa_cluster == "High-High" ~ "Progressive Stronghold",
      lisa_cluster == "Low-Low" ~ "Moderate Stronghold",
      TRUE ~ as.character(lisa_cluster)
    )
  )

# Combine and filter
lisa_summary_filtered <- bind_rows(lisa_aggregated_counts, mpls_aggregated_counts) |>
  filter(lisa_cluster %in% c("Progressive Stronghold", "Moderate Stronghold", "Minneapolis Total"))

# Display comparison table
census_vars_to_compare <- c(
  "pct_white", "pct_black", "pct_asian", "pct_hispanic",
  "pct_bachelors_plus", "pct_higher_ed",
  "pct_age_0_17", "pct_age_18_34", "pct_age_35_49", "pct_age_50_64", "pct_age_65_plus",
  "pct_owner_occupied", "pct_rent_burdened",
  "pct_english_only", "median_hh_income_mean"
)

comparison_table <- lisa_summary_filtered |>
  select(lisa_cluster, n_tracts, all_of(census_vars_to_compare)) |>
  rename(med_hh_income = median_hh_income_mean)

cat("\n=== Census Characteristics by Cluster (Aggregated from Raw Counts) ===\n")
cat("Progressive Stronghold = High-High LISA clusters\n")
cat("Moderate Stronghold = Low-Low LISA clusters\n\n")
print(comparison_table, width = Inf)

# Display raw counts
raw_counts_display <- lisa_summary_filtered |>
  select(lisa_cluster, n_tracts,
         race_total_sum, race_white_sum, race_black_sum, race_asian_sum,
         educ_total_sum, educ_bachelors_sum,
         age_total_sum, age_0_17_sum, age_18_34_sum, age_35_49_sum, age_50_64_sum, age_65_plus_sum,
         tenure_total_sum, tenure_owner_sum, tenure_renter_sum) |>
  rename(
    total_population = race_total_sum,
    white_pop = race_white_sum,
    black_pop = race_black_sum,
    asian_pop = race_asian_sum,
    educ_pop_25plus = educ_total_sum,
    bachelors_plus = educ_bachelors_sum,
    total_age = age_total_sum,
    age_0_17 = age_0_17_sum,
    age_18_34 = age_18_34_sum,
    age_35_49 = age_35_49_sum,
    age_50_64 = age_50_64_sum,
    age_65_plus = age_65_plus_sum,
    total_households = tenure_total_sum,
    owner_households = tenure_owner_sum,
    renter_households = tenure_renter_sum
  )

cat("\n=== Raw Counts by Cluster ===\n")
print(raw_counts_display, width = Inf)

# ============================================================================
# SPATIAL REGRESSION MODELS
# ============================================================================

# Define predictor variables
predictor_vars <- c(
  "pct_white", "pct_bachelors_plus", "pct_higher_ed", "pct_english_only",
  "pct_owner_occupied", "pct_wfh", "pct_transit", "pct_drive_alone",
  "pct_rent_burdened", "pct_built_post2000", "pct_multifamily",
  "pct_moved_pre2000", "pct_moved_2021_later", "pct_same_sex_hh",
  "pct_age_0_17", "pct_age_18_34", "pct_age_35_49", "pct_age_50_64"
)

predictor_vars_census <- paste0("census_PC", 1:10)

# Linear Regression (OLS)
formula_linear <- as.formula(paste("PC1 ~", paste(predictor_vars, collapse = " + ")))
model_linear <- lm(formula_linear, data = joined_data_pca_sf)
print(summary(model_linear))

joined_data_pca_sf$linear_resids <- model_linear$residuals
pca_map_plot(joined_data_pca_sf, "linear_resids", "Linear Regression Residuals (Election PC1)")

moran_test_linear_resids <- moran.test(joined_data_pca_sf$linear_resids, lw)
print(moran_test_linear_resids)

# SAR Model with Raw Census Variables
model_sar <- errorsarlm(formula_linear, data = joined_data_pca_sf, listw = lw, Durbin = FALSE)
print(summary(model_sar))

joined_data_pca_sf$sp_resids <- model_sar$residuals
pca_map_plot(joined_data_pca_sf, "sp_resids", "Spatial Autoregressive Residuals (Election PC1)")

moran_test_sar_resids <- moran.test(joined_data_pca_sf$sp_resids, lw)
print(moran_test_sar_resids)

# SAR Model with Census PCA
formula_census <- as.formula(paste("PC1 ~", paste(predictor_vars_census, collapse = " + ")))
model_sar_census <- errorsarlm(formula_census, data = joined_data_pca_sf, listw = lw, Durbin = FALSE)
print(summary(model_sar_census))

joined_data_pca_sf$sp_resids_census <- model_sar_census$residuals
pca_map_plot(joined_data_pca_sf, "sp_resids_census", "Spatial Autoregressive Residuals w/ Census PCA(Election PC1)")

# Mapview visualization
pca_color_palette <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))
mapview(joined_data_pca_sf, zcol = "PC1", col.regions = pca_color_palette(100))

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
  } else {
    r2 <- cor(model$y, model$fitted.values, use = "complete.obs")^2
    adj_r2 <- NA
    lambda <- model$lambda
  }
  
  list(
    r2 = r2,
    adj_r2 = adj_r2,
    aic = AIC(model),
    bic = BIC(model),
    rmse = sqrt(mean(model$residuals^2, na.rm = TRUE)),
    mae = mean(abs(model$residuals), na.rm = TRUE),
    lambda = lambda,
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
  R_Squared = c(metrics_linear$r2, metrics_sar$r2, metrics_sar_census$r2),
  Adj_R_Squared = c(metrics_linear$adj_r2, NA, NA),
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
cat("    For SAR models, this is correlation-based R-squared (squared correlation\n")
cat("    between fitted and actual values).\n")
cat("  - Lambda: Spatial error coefficient (only for SAR models).\n")
cat("    Indicates strength of spatial autocorrelation in errors.\n")
cat("  - Moran_I_Residuals: Spatial autocorrelation in residuals.\n")
cat("    Values closer to 0 indicate better spatial error handling.\n")
cat("  - Lower AIC/BIC/RMSE/MAE indicates better model fit.\n")
cat("  - Higher R-squared indicates better model fit.\n")
cat("  - Lower Moran's I in residuals indicates better spatial error correction.\n")
cat("\n")
