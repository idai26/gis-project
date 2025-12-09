library(tidyverse)
library(sf)
library(spdep)
library(sp)
library(spatialreg)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")

# load census data
census_data <- read_csv("mpls_tracts_census.csv") |>
  select(c(1:10, 91, 163:188)) |>
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
pca_census_data_prep <- joined_data_sf |> select(c(11:37))

pca_census_data <- pca_census_data_prep |>
  prcomp(scale. = TRUE)

factor_loadings_census <- pca_census_data$rotation |>
  as.data.frame() |>
  mutate(variable = names(pca_census_data$rotation)) |>
  arrange(PC1)

# join census PCA data to joined_data_sf
# add GEOID to PCA results to ensure proper joining
pca_results_census_with_id <- joined_data_sf |>
  select(GEOID) |>
  bind_cols(as.data.frame(pca_census_data$x))

joined_data_pca_census_sf <- joined_data_sf |>
  left_join(pca_results_census_with_id, by = c("GEOID" = "GEOID"))

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



# perform CAR (spatial error) model with stepwise selection
# This corrects for spatial autocorrelation in the errors
predictor_vars <- names(joined_data_sf)[11:37]

# stepwise selection function for spatial error models (CAR)
stepwise_spatial <- function(y_var, predictor_vars, data, listw, 
                             direction = "backward", alpha = 0.1) {
  
  if (direction == "backward") {
    # Start with full model
    current_vars <- predictor_vars
    formula_str <- paste(y_var, "~", paste(current_vars, collapse = " + "))
    current_model <- spatialreg::errorsarlm(as.formula(formula_str), data = data, listw = listw, Durbin = FALSE)
    current_aic <- AIC(current_model)
    
    cat("\n=== BACKWARD STEPWISE SELECTION ===\n")
    cat(sprintf("Starting model AIC: %.2f with %d variables\n", current_aic, length(current_vars)))
    
    improved <- TRUE
    iteration <- 1
    
    while (improved && length(current_vars) > 1) {
      improved <- FALSE
      best_aic <- current_aic
      best_vars <- current_vars
      var_to_remove <- NULL
      
      # Try removing each variable
      for (var in current_vars) {
        test_vars <- setdiff(current_vars, var)
        formula_str <- paste(y_var, "~", paste(test_vars, collapse = " + "))
        test_model <- tryCatch({
          spatialreg::errorsarlm(as.formula(formula_str), data = data, listw = listw, Durbin = FALSE)
        }, error = function(e) NULL)
        
        if (!is.null(test_model)) {
          test_aic <- AIC(test_model)
          if (test_aic < best_aic) {
            best_aic <- test_aic
            best_vars <- test_vars
            var_to_remove <- var
            improved <- TRUE
          }
        }
      }
      
      if (improved) {
        cat(sprintf("Iteration %d: Removed '%s', AIC improved from %.2f to %.2f\n", 
                    iteration, var_to_remove, current_aic, best_aic))
        current_vars <- best_vars
        current_aic <- best_aic
        formula_str <- paste(y_var, "~", paste(current_vars, collapse = " + "))
        current_model <- spatialreg::errorsarlm(as.formula(formula_str), data = data, listw = listw, Durbin = FALSE)
        iteration <- iteration + 1
      }
    }
    
    cat(sprintf("\nFinal model: %d variables, AIC = %.2f\n", length(current_vars), current_aic))
    cat("Selected variables:\n")
    cat(paste(current_vars, collapse = ", "), "\n")
    
    return(list(model = current_model, variables = current_vars, aic = current_aic))
    
  } else if (direction == "forward") {
    # Start with null model (intercept only)
    current_vars <- c()
    remaining_vars <- predictor_vars
    formula_str <- paste(y_var, "~ 1")
    current_model <- spatialreg::errorsarlm(as.formula(formula_str), data = data, listw = listw, Durbin = FALSE)
    current_aic <- AIC(current_model)
    
    cat("\n=== FORWARD STEPWISE SELECTION ===\n")
    cat(sprintf("Starting model AIC: %.2f with 0 variables\n", current_aic))
    
    improved <- TRUE
    iteration <- 1
    
    while (improved && length(remaining_vars) > 0) {
      improved <- FALSE
      best_aic <- current_aic
      best_vars <- current_vars
      var_to_add <- NULL
      
      # Try adding each remaining variable
      for (var in remaining_vars) {
        test_vars <- c(current_vars, var)
        formula_str <- paste(y_var, "~", paste(test_vars, collapse = " + "))
        test_model <- tryCatch({
          spatialreg::errorsarlm(as.formula(formula_str), data = data, listw = listw, Durbin = FALSE)
        }, error = function(e) NULL)
        
        if (!is.null(test_model)) {
          test_aic <- AIC(test_model)
          if (test_aic < best_aic) {
            best_aic <- test_aic
            best_vars <- test_vars
            var_to_add <- var
            improved <- TRUE
          }
        }
      }
      
      if (improved) {
        cat(sprintf("Iteration %d: Added '%s', AIC improved from %.2f to %.2f\n", 
                    iteration, var_to_add, current_aic, best_aic))
        current_vars <- best_vars
        remaining_vars <- setdiff(remaining_vars, var_to_add)
        current_aic <- best_aic
        formula_str <- paste(y_var, "~", paste(current_vars, collapse = " + "))
        current_model <- spatialreg::errorsarlm(as.formula(formula_str), data = data, listw = listw, Durbin = FALSE)
        iteration <- iteration + 1
      }
    }
    
    cat(sprintf("\nFinal model: %d variables, AIC = %.2f\n", length(current_vars), current_aic))
    cat("Selected variables:\n")
    cat(paste(current_vars, collapse = ", "), "\n")
    
    return(list(model = current_model, variables = current_vars, aic = current_aic))
  }
}

# Run forward stepwise selection
stepwise_result <- stepwise_spatial(
  y_var = "PC1",
  predictor_vars = predictor_vars,
  data = joined_data_pca_sf,
  listw = lw,
  direction = "forward"
)

# Extract the final CAR/spatial error model
sem_fit <- stepwise_result$model
selected_vars <- stepwise_result$variables

# Print summary of final model
cat("\n=== FINAL CAR MODEL SUMMARY ===\n")
sem_fit_df <- broom::tidy(sem_fit)
print(sem_fit_df)

# Also fit the full model for comparison
formula_str_full <- paste("PC1 ~", paste(predictor_vars, collapse = " + "))
sem_fit_full <- spatialreg::errorsarlm(
  as.formula(formula_str_full),
  data = joined_data_pca_sf,
  listw = lw,
  Durbin = FALSE
)
cat(sprintf("\nFull model AIC: %.2f\n", AIC(sem_fit_full)))
cat(sprintf("Selected model AIC: %.2f\n", AIC(sem_fit)))
cat(sprintf("AIC improvement: %.2f\n", AIC(sem_fit_full) - AIC(sem_fit)))

# plot spatial lag
joined_data_sf$sp_lag <- spdep::lag.listw(lw, joined_data_sf$pct_asian)
joined_data_sf |> ggplot(aes(fill=sp_lag, geometry = geometry))+
  geom_sf(aes(fill = sp_lag), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 10,
    name = "Spatial Lag of Frey Share (%)",
    limits = c(0, 20),
    na.value = "grey80"
    ) +
  labs(
    title = "Spatial Lag of Frey Share by Census Tract",
    subtitle = "Minneapolis, MN (Elections 2017, 2021, 2025)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# plot spatial residuals
joined_data_pca_sf$sp_resids <- sem_fit$residuals
joined_data_pca_sf |> ggplot(aes(fill = sp_resids, geometry = geometry))+
  geom_sf(aes(fill = sp_resids), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    name = "Spatial Residuals",
    limits = c(-5, 5),
    na.value = "grey80"
  ) +
  labs(
    title = "Spatial Error Model Residuals (PC1) by Census Tract",
    subtitle = "Minneapolis, MN - CAR Model"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
