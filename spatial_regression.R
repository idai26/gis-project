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

# perform CAR (spatial error) model
predictor_vars <- c("pct_white", "pct_bachelors_plus", "pct_higher_ed", "pct_english_only", 
"pct_owner_occupied", "pct_wfh", "pct_transit", "pct_drive_alone", "pct_rent_burdened", "pct_built_post2000",
"pct_multifamily", "pct_moved_pre2000", "pct_moved_2021_later", "pct_same_sex_hh", 
"pct_age_0_17", "pct_age_18_34", "pct_age_35_49", "pct_age_50_64")

formula_str_full <- paste("PC1 ~", paste(predictor_vars, collapse = " + "))

subset_result <- spatialreg::errorsarlm(
  as.formula(formula_str_full),
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
