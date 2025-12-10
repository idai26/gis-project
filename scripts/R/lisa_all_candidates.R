## LISA Cluster Analysis for All Candidate Vote Percentages (Tract Level)
## Iterates over all _pct columns and creates LISA cluster plots with Moran's I

library(tidyverse)
library(sf)
library(spdep)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")

# Create lisa_plots directory if it doesn't exist
if (!dir.exists("../outputs/lisa_plots")) {
  dir.create("../outputs/lisa_plots", recursive = TRUE)
}

# Load tract shapefile
tract_sf <- st_read("../data/shapefiles/tracts/mpls_tract_elections.shp", quiet = TRUE) |>
  st_transform(26915)

# Load tract-level election results
election_results <- read_csv("../data/processed/election_results/tract_election_results_all.csv", show_col_types = FALSE) |>
  mutate(tract_geoid = as.character(tract_geoid))

# Join election results to shapefile
election_sf <- tract_sf |>
  left_join(election_results, by = c("GEOID" = "tract_geoid"))

# Get all percentage columns
pct_columns <- names(election_results)[str_detect(names(election_results), "_pct$")]

cat("Found", length(pct_columns), "percentage columns to analyze:\n")
cat(paste(pct_columns, collapse = ", "), "\n\n")

# Function to perform LISA analysis on a given variable
perform_lisa_analysis <- function(sf_data, var_name) {
  # Remove rows with NA values for this variable
  sf_clean <- sf_data |>
    filter(!is.na(.data[[var_name]]))
  
  if (nrow(sf_clean) < 10) {
    warning(paste("Insufficient data for", var_name))
    return(NULL)
  }
  
  # Create spatial weights
  nb <- tryCatch({
    poly2nb(sf_clean, queen = TRUE)
  }, error = function(e) {
    warning(paste("Error creating neighbors for", var_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(nb)) return(NULL)
  
  lw <- tryCatch({
    nb2listw(nb, style = "W", zero.policy = TRUE)
  }, error = function(e) {
    warning(paste("Error creating weights for", var_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(lw)) return(NULL)
  
  # Calculate Global Moran's I
  moran_global <- tryCatch({
    moran.test(sf_clean[[var_name]], lw, zero.policy = TRUE)
  }, error = function(e) {
    warning(paste("Error calculating Moran's I for", var_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(moran_global)) return(NULL)
  
  # Calculate Local Moran's I (LISA)
  lisa <- tryCatch({
    localmoran(sf_clean[[var_name]], lw, zero.policy = TRUE)
  }, error = function(e) {
    warning(paste("Error calculating LISA for", var_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(lisa)) return(NULL)
  
  # Add LISA results to spatial data
  var_mean <- mean(sf_clean[[var_name]], na.rm = TRUE)
  var_lag <- lag.listw(lw, sf_clean[[var_name]], zero.policy = TRUE)
  
  sf_result <- sf_clean |>
    mutate(
      lisa_I = lisa[, 1],
      lisa_p = lisa[, 5],
      lisa_z = lisa[, 4],
      var_mean = var_mean,
      var_lag = var_lag,
      var_hl = if_else(.data[[var_name]] >= var_mean, "High", "Low"),
      lag_hl = if_else(var_lag >= var_mean, "High", "Low"),
      significant = lisa_p < 0.05,
      lisa_cluster = case_when(
        !significant ~ "Not Significant",
        var_hl == "High" & lag_hl == "High" ~ "High-High",
        var_hl == "Low" & lag_hl == "Low" ~ "Low-Low",
        var_hl == "High" & lag_hl == "Low" ~ "High-Low",
        var_hl == "Low" & lag_hl == "High" ~ "Low-High",
        TRUE ~ "Not Significant"
      ),
      lisa_cluster = factor(
        lisa_cluster,
        levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")
      )
    )
  
  list(
    spatial_data = sf_result,
    moran_global = moran_global,
    lw = lw
  )
}

# Function to create LISA cluster map
create_lisa_map <- function(sf_data, moran_global, var_name) {
  lisa_colors <- c(
    "High-High" = "#d7191c",
    "Low-Low" = "#2b83ba",
    "High-Low" = "#fdae61",
    "Low-High" = "#abdda4",
    "Not Significant" = "#f0f0f0"
  )
  
  # Format Moran's I and p-value
  moran_i <- round(moran_global$estimate[1], 3)
  moran_p <- moran_global$p.value
  p_text <- if (moran_p < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", round(moran_p, 3))
  }
  
  # Create readable variable name
  var_label <- var_name |>
    str_replace("_pct$", "") |>
    str_replace_all("_", " ") |>
    str_to_title()
  
  ggplot(sf_data) +
    geom_sf(aes(fill = lisa_cluster), color = "white", size = 0.1) +
    scale_fill_manual(
      name = "LISA Clusters",
      values = lisa_colors,
      drop = FALSE
    ) +
    labs(
      title = paste0("LISA Clusters (Tract): ", var_label, " (%)"),
      subtitle = paste0("Moran's I = ", moran_i, ", ", p_text)
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

# Store summary results
summary_results <- tibble(
  variable = character(),
  morans_i = numeric(),
  p_value = numeric(),
  n_high_high = integer(),
  n_low_low = integer(),
  n_high_low = integer(),
  n_low_high = integer(),
  n_not_sig = integer(),
  pct_significant = numeric()
)

# Iterate over all percentage columns
cat("Processing LISA analysis for all candidate percentages (TRACT LEVEL)...\n")
cat("=======================================================================\n\n")

for (var_name in pct_columns) {
  cat("Processing:", var_name, "... ")
  
  # Perform LISA analysis
  result <- perform_lisa_analysis(election_sf, var_name)
  
  if (is.null(result)) {
    cat("SKIPPED (insufficient data or error)\n")
    next
  }
  
  # Create LISA map
  lisa_map <- create_lisa_map(
    result$spatial_data,
    result$moran_global,
    var_name
  )
  
  # Save plot
  filename <- paste0("../outputs/lisa_plots/tract_", var_name, "_lisa.png")
  ggsave(
    filename = filename,
    plot = lisa_map,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  # Calculate summary statistics
  cluster_counts <- table(result$spatial_data$lisa_cluster)
  n_total <- nrow(result$spatial_data)
  n_sig <- sum(result$spatial_data$significant)
  
  summary_results <- summary_results |>
    add_row(
      variable = var_name,
      morans_i = result$moran_global$estimate[1],
      p_value = result$moran_global$p.value,
      n_high_high = as.integer(cluster_counts["High-High"] %||% 0),
      n_low_low = as.integer(cluster_counts["Low-Low"] %||% 0),
      n_high_low = as.integer(cluster_counts["High-Low"] %||% 0),
      n_low_high = as.integer(cluster_counts["Low-High"] %||% 0),
      n_not_sig = as.integer(cluster_counts["Not Significant"] %||% 0),
      pct_significant = round(100 * n_sig / n_total, 1)
    )
  
  cat("DONE (Moran's I =", round(result$moran_global$estimate[1], 3), ")\n")
}

# Print and save summary
cat("\n=======================================================================\n")
cat("SUMMARY OF LISA ANALYSIS RESULTS (TRACT LEVEL)\n")
cat("=======================================================================\n\n")

# Sort by absolute Moran's I
summary_results <- summary_results |>
  arrange(desc(abs(morans_i)))

print(summary_results, n = Inf)

# Save summary to CSV
write_csv(summary_results, "../outputs/lisa_plots/tract_lisa_summary.csv")
cat("\nSummary saved to: ../outputs/lisa_plots/tract_lisa_summary.csv\n")

# Identify variables with strongest spatial clustering
cat("\n=== TOP 10 VARIABLES BY SPATIAL CLUSTERING ===\n")
top_10 <- summary_results |>
  slice_head(n = 10) |>
  select(variable, morans_i, p_value, pct_significant)
print(top_10)

cat("\n=== ALL PLOTS SAVED TO: ../outputs/lisa_plots/ ===\n")
cat("Total plots generated:", nrow(summary_results), "\n")
