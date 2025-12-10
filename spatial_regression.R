library(tidyverse)
library(sf)
library(spdep)
library(sp)
library(spatialreg)
library(mapview)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")

# load census data
census_data <- read_csv("mpls_tracts_census.csv") |>
  select(c(1:10, 91, contains("pct_"))) |>
  mutate(GEOID = as.character(GEOID))

# load full election results
election_results <- read_csv("tract_election_results_all.csv") |>
  mutate(tract_geoid = as.character(tract_geoid))

# load mayor election data shapefile
election_data_sf <- read_sf("tract_election_shapefiles/mpls_tract_elections.shp") |>
 st_transform(26915)

# join census data and election data
joined_data_sf <- left_join(census_data, election_data_sf, by = "GEOID") |>
  left_join(election_results, by = c("GEOID" = "tract_geoid"))

# create spatial weights matrix
nb <- spdep::poly2nb(joined_data_sf$geometry)
lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

# generate principal components analysis of election variables
pca_data_prep <- election_results |>
  select(contains("_pct"))

pca_data <- pca_data_prep |>
  prcomp(scale. = TRUE)

# look at factor loadings by ascending order of PC1
factor_loadings <- pca_data$rotation |>
  as.data.frame() |>
  mutate(variable = names(pca_data$rotation)) |>
  arrange(PC1)

# join PCA data to joined_data_sf
# add GEOID to PCA results to ensure proper joining
pca_results_with_id <- election_results |>
  select(tract_geoid) |>
  bind_cols(as.data.frame(pca_data$x))

# use left_join to ensure rows match by GEOID
joined_data_pca_sf <- joined_data_sf |>
  left_join(pca_results_with_id, by = c("GEOID" = "tract_geoid"))


# generate PCA of census variables
pca_census_data_prep <- census_data |> select(-c(1:10))

pca_census_data <- pca_census_data_prep |>
  prcomp(scale. = TRUE)

factor_loadings_census <- pca_census_data$rotation |>
  as.data.frame() |>
  mutate(variable = names(pca_census_data$rotation)) |>
  arrange(PC1)

# join census PCA data to joined_data_sf
# add GEOID to PCA results to ensure proper joining
# rename census PCA columns to have "census_" prefix
pca_results_census_with_id <- joined_data_sf |>
  select(GEOID) |>
  bind_cols(as.data.frame(pca_census_data$x)) |>
  rename_with(~ paste0("census_", .x), starts_with("PC"))

# create combined dataframe with both electoral and census PCA
# ensure it remains an sf object after joining
joined_data_pca_sf <- joined_data_pca_sf |>
  left_join(pca_results_census_with_id, by = c("GEOID" = "GEOID")) |>
  st_as_sf()

# plot PCA by tract
pca_map_plot <- function(data, pc, subtitle = NULL) {
  ggplot(data, aes(fill = !!sym(pc), geometry = geometry)) +
    geom_sf(aes(fill = !!sym(pc)), color = "white", size = 0.1) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b"
    ) +
    labs(
      title = "PCA of Election Variables by Census Tract",
      subtitle = subtitle %||% pc
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

pca_map_plot(joined_data_pca_sf, "PC1", "PC1")

# calculate Moran's I for Election PC1
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
  scale_fill_manual(
    name = "LISA Clusters",
    values = lisa_colors,
    drop = FALSE
  ) +
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
# COMPARE CENSUS CHARACTERISTICS BY LISA CLUSTER
# Using raw counts aggregated by cluster, then calculating percentages
# ============================================================================

# Load raw count variables from census data
census_data_raw <- read_csv("mpls_tracts_census.csv") |>
  mutate(GEOID = as.character(GEOID)) |>
  select(GEOID, 
         # Race variables
         race_total, race_white, race_black, race_asian, 
         # Hispanic variables
         hisp_total, hisp_latino,
         # Education variables (population 25+)
         educ_total, educ_bachelors, educ_masters, educ_professional, educ_doctorate,
         # Age variables (total population) - select all age columns
         starts_with("age_"),
         # Housing variables
         tenure_total, tenure_owner, tenure_renter,
         rent_burden_total, rent_burden_30_34, rent_burden_35_39, 
         rent_burden_40_49, rent_burden_50_plus,
         # Language variables
         lang_total, lang_english_only,
         # Income (median)
         med_hh_income)

# Join raw counts to LISA cluster data
joined_data_with_counts <- joined_data_pca_sf |>
  st_drop_geometry() |>
  select(GEOID, lisa_cluster) |>
  left_join(census_data_raw, by = "GEOID")

# Aggregate raw counts by LISA cluster
lisa_aggregated_counts <- joined_data_with_counts |>
  group_by(lisa_cluster) |>
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
    # Education aggregations (population 25+)
    educ_total_sum = sum(educ_total, na.rm = TRUE),
    educ_bachelors_sum = sum(educ_bachelors, na.rm = TRUE),
    educ_masters_sum = sum(educ_masters, na.rm = TRUE),
    educ_professional_sum = sum(educ_professional, na.rm = TRUE),
    educ_doctorate_sum = sum(educ_doctorate, na.rm = TRUE),
    # Age aggregations (calculate age groups from raw counts)
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
    # Income (weighted median - using mean of medians as approximation)
    median_hh_income_mean = mean(med_hh_income, na.rm = TRUE)
  ) |>
  # Calculate percentages from aggregated counts
  mutate(
    pct_white = (race_white_sum / race_total_sum) * 100,
    pct_black = (race_black_sum / race_total_sum) * 100,
    pct_asian = (race_asian_sum / race_total_sum) * 100,
    pct_hispanic = (hisp_latino_sum / hisp_total_sum) * 100,
    pct_bachelors_plus = ((educ_bachelors_sum + educ_masters_sum + educ_professional_sum + educ_doctorate_sum) / educ_total_sum) * 100,
    pct_higher_ed = ((educ_masters_sum + educ_professional_sum + educ_doctorate_sum) / educ_total_sum) * 100,
    pct_age_0_17 = (age_0_17_sum / age_total_sum) * 100,
    pct_age_18_34 = (age_18_34_sum / age_total_sum) * 100,
    pct_age_35_49 = (age_35_49_sum / age_total_sum) * 100,
    pct_age_50_64 = (age_50_64_sum / age_total_sum) * 100,
    pct_age_65_plus = (age_65_plus_sum / age_total_sum) * 100,
    pct_owner_occupied = (tenure_owner_sum / tenure_total_sum) * 100,
    pct_rent_burdened = (rent_burden_30_plus_sum / rent_burden_total_sum) * 100,
    pct_english_only = (lang_english_only_sum / lang_total_sum) * 100
  )

# Calculate Minneapolis-wide statistics using same method
mpls_aggregated_counts <- joined_data_with_counts |>
  summarise(
    lisa_cluster = "Minneapolis Total",
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
    median_hh_income_mean = mean(med_hh_income, na.rm = TRUE)
  ) |>
  # Calculate percentages from aggregated counts
  mutate(
    pct_white = (race_white_sum / race_total_sum) * 100,
    pct_black = (race_black_sum / race_total_sum) * 100,
    pct_asian = (race_asian_sum / race_total_sum) * 100,
    pct_hispanic = (hisp_latino_sum / hisp_total_sum) * 100,
    pct_bachelors_plus = ((educ_bachelors_sum + educ_masters_sum + educ_professional_sum + educ_doctorate_sum) / educ_total_sum) * 100,
    pct_higher_ed = ((educ_masters_sum + educ_professional_sum + educ_doctorate_sum) / educ_total_sum) * 100,
    pct_age_0_17 = (age_0_17_sum / age_total_sum) * 100,
    pct_age_18_34 = (age_18_34_sum / age_total_sum) * 100,
    pct_age_35_49 = (age_35_49_sum / age_total_sum) * 100,
    pct_age_50_64 = (age_50_64_sum / age_total_sum) * 100,
    pct_age_65_plus = (age_65_plus_sum / age_total_sum) * 100,
    pct_owner_occupied = (tenure_owner_sum / tenure_total_sum) * 100,
    pct_rent_burdened = (rent_burden_30_plus_sum / rent_burden_total_sum) * 100,
    pct_english_only = (lang_english_only_sum / lang_total_sum) * 100
  )

# Rename clusters: High-High -> Progressive Stronghold, Low-Low -> Moderate Stronghold
lisa_aggregated_counts <- lisa_aggregated_counts |>
  mutate(
    lisa_cluster = case_when(
      lisa_cluster == "High-High" ~ "Progressive Stronghold",
      lisa_cluster == "Low-Low" ~ "Moderate Stronghold",
      TRUE ~ as.character(lisa_cluster)
    )
  )

# Combine cluster summaries with city-wide summary
lisa_summary_combined <- bind_rows(
  lisa_aggregated_counts,
  mpls_aggregated_counts
)

# Filter to only show Progressive Stronghold, Moderate Stronghold, and Minneapolis Total
lisa_summary_filtered <- lisa_summary_combined |>
  filter(lisa_cluster %in% c("Progressive Stronghold", "Moderate Stronghold", "Minneapolis Total"))

# Select key variables for display
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

# Print raw counts for transparency
cat("\n=== Raw Counts by Cluster ===\n")
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

print(raw_counts_display, width = Inf)

# ============================================================================
# END LISA CLUSTER ANALYSIS
# ============================================================================


# perform regular linear regression
predictor_vars <- c("pct_white", "pct_bachelors_plus", "pct_higher_ed", "pct_english_only", 
"pct_owner_occupied", "pct_wfh", "pct_transit", "pct_drive_alone", "pct_rent_burdened", "pct_built_post2000",
"pct_multifamily", "pct_moved_pre2000", "pct_moved_2021_later", "pct_same_sex_hh", 
"pct_age_0_17", "pct_age_18_34", "pct_age_35_49", "pct_age_50_64")

formula_str_linear <- paste("PC1 ~", paste(predictor_vars, collapse = " + "))

subset_result_linear <- lm(as.formula(formula_str_linear), data = joined_data_pca_sf)

# print summary of model
print(summary(subset_result_linear))

# plot linear regression residuals by tract
joined_data_pca_sf$linear_resids <- subset_result_linear$residuals
pca_map_plot(joined_data_pca_sf, "linear_resids", "Linear Regression Residuals (PC1)")

# calculate Moran's I for linear regression residuals
moran_test_linear_resids <- moran.test(joined_data_pca_sf$linear_resids, lw)
print(moran_test_linear_resids)

# perform CAR (spatial error) model
subset_result <- spatialreg::errorsarlm(
  as.formula(formula_str_linear),
  data = joined_data_pca_sf,
  listw = lw,
  Durbin = FALSE
)

# print summary of model
print(summary(subset_result))

# plot spatial residuals
joined_data_pca_sf$sp_resids <- subset_result$residuals
pca_map_plot(joined_data_pca_sf, "sp_resids", "Spatial Residuals (PC1)")

# calculate Moran's I for spatial residuals
moran_test_residuals <- moran.test(joined_data_pca_sf$sp_resids, lw)
print(moran_test_residuals)

# use census PCA as predictor in spatial error model
predictor_vars_census <- c("census_PC1", "census_PC2", "census_PC3", "census_PC4", "census_PC5", "census_PC6", "census_PC7", "census_PC8", "census_PC9", "census_PC10")

formula_str_census <- paste("PC1 ~", paste(predictor_vars_census, collapse = " + "))

subset_result_census <- spatialreg::errorsarlm(
  as.formula(formula_str_census),
  data = joined_data_pca_sf,
  listw = lw,
  Durbin = FALSE
)

# print summary of model
print(summary(subset_result_census))

# plot spatial residuals by census PCA
joined_data_pca_sf$sp_resids_census <- subset_result_census$residuals
pca_map_plot(joined_data_pca_sf, "sp_resids_census", "Census PCA (PC1)")

# plot PCA by tract using mapview
# ensure object is sf class for mapview
if (!inherits(joined_data_pca_sf, "sf")) {
  joined_data_pca_sf <- st_as_sf(joined_data_pca_sf)
}
# create color palette matching pca_map_plot (blue-white-red diverging)
pca_color_palette <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))
mapview(joined_data_pca_sf, zcol = "PC1", col.regions = pca_color_palette(100))
