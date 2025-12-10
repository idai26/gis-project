## load packages
library(tidyverse)
library(sf)
library(spdep)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")

# function to normalize precinct names
normalize_precinct_name <- function(name) {
  name |>
    tolower() |>
    str_trim() |>
    str_replace_all("p-0+(\\d+)", "p-\\1")
}

# Configuration for each election year
election_config <- tribble(
  ~year, ~csv_file, ~shapefile, ~opponent_col, ~opponent_name,
  2017, "../data/processed/cvr/frey_vs_dehn_by_precinct.csv", "../data/raw/precincts/2016/mn_precincts16.shp", "Raymond Dehn_votes", "Dehn",
  2021, "../data/processed/cvr/frey_vs_knuth_by_precinct.csv", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "Kate Knuth_votes", "Knuth",
  2025, "../data/processed/cvr/fateh_vs_frey_by_precinct.csv", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "Omar Fateh_votes", "Fateh"
)

# Function to load and process election data
load_election_data <- function(year, csv_file, shapefile, opponent_col) {
  # Read results CSV
  results <- read_csv(csv_file, show_col_types = FALSE) |>
    mutate(Precinct = normalize_precinct_name(Precinct))
  
  # Load and clean shapefile
  sf_data <- st_read(shapefile, quiet = TRUE) |>
    select(1:7) |>
    filter(grepl("Minneapolis", PCTNAME)) |>
    mutate(Precinct = normalize_precinct_name(PCTNAME))
  
  # Merge and calculate vote shares
  sf_data |>
    left_join(results, by = "Precinct") |>
    filter(!is.na(`Jacob Frey_votes`)) |>
    mutate(
      Valid_votes = `Jacob Frey_votes` + .data[[opponent_col]],
      Frey_share = (`Jacob Frey_votes` / Valid_votes) * 100
    )
}

# Function to perform spatial analysis
perform_spatial_analysis <- function(sf_data) {
  # Create spatial weights
  nb <- poly2nb(sf_data, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Calculate Global Moran's I
  moran_global <- moran.test(sf_data$Frey_share, lw)
  
  # Calculate Local Moran's I (LISA)
  lisa <- localmoran(sf_data$Frey_share, lw)
  
  # Add LISA results to spatial data
  sf_data |>
    mutate(
      lisa_I = lisa[, 1],
      lisa_p = lisa[, 5],
      lisa_z = lisa[, 4],
      frey_mean = mean(Frey_share, na.rm = TRUE),
      frey_lag = lag.listw(lw, Frey_share),
      frey_hl = if_else(Frey_share >= frey_mean, "High", "Low"),
      lag_hl = if_else(frey_lag >= frey_mean, "High", "Low"),
      significant = lisa_p < 0.05,
      lisa_cluster = case_when(
        !significant ~ "Not Significant",
        frey_hl == "High" & lag_hl == "High" ~ "High-High",
        frey_hl == "Low" & lag_hl == "Low" ~ "Low-Low",
        frey_hl == "High" & lag_hl == "Low" ~ "High-Low",
        frey_hl == "Low" & lag_hl == "High" ~ "Low-High",
        TRUE ~ "Not Significant"
      ),
      lisa_cluster = factor(
        lisa_cluster,
        levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")
      )
    ) |>
    list(spatial_data = _, moran_global = moran_global, lw = lw)
}

# Function to create LISA cluster map
create_lisa_map <- function(sf_data, moran_global, year, opponent_name) {
  lisa_colors <- c(
    "High-High" = "#d7191c",
    "Low-Low" = "#2b83ba",
    "High-Low" = "#fdae61",
    "Low-High" = "#abdda4",
    "Not Significant" = "#f0f0f0"
  )
  
  ggplot(sf_data) +
    geom_sf(aes(fill = lisa_cluster), color = "white", size = 0.1) +
    scale_fill_manual(
      name = "LISA Clusters",
      values = lisa_colors,
      drop = FALSE
    ) +
    labs(
      title = paste0(year, " Minneapolis Mayoral Election - LISA Clusters"),
      subtitle = paste0(
        "Frey vs. ", opponent_name, " (Final Round)\n",
        "Global Moran's I = ", round(moran_global$estimate[1], 3),
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
}

# Function to create vote share map
create_vote_share_map <- function(sf_data, year) {
  ggplot(sf_data) +
    geom_sf(aes(fill = Frey_share), color = "white", size = 0.1) +
    scale_fill_gradient2(
      low = "#2166ac",      # Blue for opponent wins (< 50%)
      mid = "#f7f7f7",      # Light gray at 50% threshold
      high = "#b2182b",     # Red for Frey wins (> 50%)
      midpoint = 50,
      name = "Frey Share (%)",
      limits = c(0, 100),
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 1,
        barheight = 8
      )
    ) +
    labs(
      title = paste0(year, " Minneapolis Mayoral Election - Frey Share"),
      subtitle = "Red = Frey majority (>50%), Blue = Opponent majority (<50%)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

# Process all elections
election_results <- election_config |>
  mutate(
    data = pmap(
      list(year, csv_file, shapefile, opponent_col),
      ~ load_election_data(..1, ..2, ..3, ..4)
    ),
    analysis = map(data, perform_spatial_analysis),
    spatial_data = map(analysis, ~ .x$spatial_data),
    moran_global = map(analysis, ~ .x$moran_global),
    lw = map(analysis, ~ .x$lw)
  )

# Print summary statistics for each year
walk2(
  election_results$year,
  election_results$spatial_data,
  ~ {
    cat("\n=== ", .x, " LISA Cluster Summary ===\n")
    print(table(.y$lisa_cluster))
    cat("Percent significant:", 
        round(100 * sum(.y$significant) / nrow(.y), 1), "%\n")
    cat("Global Moran's I:", 
        round(election_results$moran_global[[which(election_results$year == .x)]]$estimate[1], 3), "\n")
  }
)

# Create plots directory if it doesn't exist
if (!dir.exists("../outputs/plots")) {
  dir.create("../outputs/plots", recursive = TRUE)
}

# Create and display maps for each year
for (i in seq_len(nrow(election_results))) {
  year <- election_results$year[i]
  opponent_name <- election_results$opponent_name[i]
  spatial_data <- election_results$spatial_data[[i]]
  moran_global <- election_results$moran_global[[i]]
  
  # Vote share map
  vote_share_map <- create_vote_share_map(spatial_data, year)
  print(vote_share_map)
  ggsave(
    filename = paste0("../outputs/plots/", year, "_frey_share.png"),
    plot = vote_share_map,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  # LISA cluster map
  lisa_map <- create_lisa_map(spatial_data, moran_global, year, opponent_name)
  print(lisa_map)
  ggsave(
    filename = paste0("../outputs/plots/", year, "_lisa_clusters.png"),
    plot = lisa_map,
    width = 10,
    height = 8,
    dpi = 300
  )
}

# Access individual results if needed:
# sf_2017 <- election_results$spatial_data[[1]]
# sf_2021 <- election_results$spatial_data[[2]]
# sf_2025 <- election_results$spatial_data[[3]]
