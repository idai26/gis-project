# =============================================================================
# Census Data for Minneapolis Tracts using tidycensus
# Replicates NHGIS tables from ACS 2019-2023 5-Year Estimates
# =============================================================================

library(tidyverse)
library(tidycensus)
library(sf)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")

# -----------------------------------------------------------------------------
# Define ACS variables matching NHGIS tables
# -----------------------------------------------------------------------------

# Table B02001: Race
race_vars <- c(
  race_total = "B02001_001",
  race_white = "B02001_002",
  race_black = "B02001_003",
  race_aian = "B02001_004",
  race_asian = "B02001_005",
  race_nhpi = "B02001_006",
  race_other = "B02001_007",
  race_two_plus = "B02001_008"
)

# Table B03002: Hispanic or Latino Origin by Race
hisp_race_vars <- c(
  hisp_total = "B03002_001",
  hisp_not_hisp = "B03002_002",
  hisp_nh_white = "B03002_003",
  hisp_nh_black = "B03002_004",
  hisp_nh_aian = "B03002_005",
  hisp_nh_asian = "B03002_006",
  hisp_nh_nhpi = "B03002_007",
  hisp_nh_other = "B03002_008",
  hisp_nh_two_plus = "B03002_009",
  hisp_latino = "B03002_012",
  hisp_lat_white = "B03002_013",
  hisp_lat_black = "B03002_014"
)

# Table B08301: Means of Transportation to Work
commute_vars <- c(
  commute_total = "B08301_001",
  commute_drove_alone = "B08301_003",
  commute_carpool = "B08301_004",
  commute_transit = "B08301_010",
  commute_bus = "B08301_011",
  commute_subway = "B08301_012",
  commute_rail = "B08301_013",
  commute_light_rail = "B08301_014",
  commute_taxi = "B08301_016",
  commute_bike = "B08301_018",
  commute_walk = "B08301_019",
  commute_other = "B08301_020",
  commute_wfh = "B08301_021"
)

# Table B11001: Household Type
hh_type_vars <- c(
  hh_total = "B11001_001",
  hh_family = "B11001_002",
  hh_married = "B11001_003",
  hh_other_family = "B11001_004",
  hh_male_no_spouse = "B11001_005",
  hh_female_no_spouse = "B11001_006",
  hh_nonfamily = "B11001_007",
  hh_living_alone = "B11001_008",
  hh_not_alone = "B11001_009"
)

# Table B14007: School Enrollment
school_vars <- c(
  school_total = "B14007_001",
  school_enrolled = "B14007_002",
  school_preschool = "B14007_003",
  school_kindergarten = "B14007_004",
  school_g1_4 = "B14007_005",
  school_g5_8 = "B14007_009",
  school_g9_12 = "B14007_013",
  school_undergrad = "B14007_017",
  school_grad = "B14007_018",
  school_not_enrolled = "B14007_019"
)

# Table B15003: Educational Attainment (25+)
educ_vars <- c(
  educ_total = "B15003_001",
  educ_no_school = "B15003_002",
  educ_less_9th = "B15003_003",
  educ_9th_12th = "B15003_016",
  educ_hs_diploma = "B15003_017",
  educ_ged = "B15003_018",
  educ_some_college = "B15003_019",
  educ_some_college_1yr = "B15003_020",
  educ_associates = "B15003_021",
  educ_bachelors = "B15003_022",
  educ_masters = "B15003_023",
  educ_professional = "B15003_024",
  educ_doctorate = "B15003_025"
)

# Table C16002: Household Language
lang_vars <- c(
  lang_total = "C16002_001",
  lang_english_only = "C16002_002",
  lang_spanish = "C16002_003",
  lang_spanish_limited = "C16002_004",
  lang_spanish_not_limited = "C16002_005",
  lang_other_ie = "C16002_006",
  lang_other_ie_limited = "C16002_007",
  lang_asian_pi = "C16002_009",
  lang_asian_pi_limited = "C16002_010",
  lang_other = "C16002_012",
  lang_other_limited = "C16002_013"
)

# Table B19013: Median Household Income
income_vars <- c(
  med_hh_income = "B19013_001"
)

# Table B23025: Employment Status (16+)
employ_vars <- c(
  employ_total = "B23025_001",
  employ_in_lf = "B23025_002",
  employ_civilian_lf = "B23025_003",
  employ_employed = "B23025_004",
  employ_unemployed = "B23025_005",
  employ_armed_forces = "B23025_006",
  employ_not_in_lf = "B23025_007"
)

# Table B25003: Tenure
tenure_vars <- c(
  tenure_total = "B25003_001",
  tenure_owner = "B25003_002",
  tenure_renter = "B25003_003"
)

# Table B25024: Units in Structure
units_vars <- c(
  units_total = "B25024_001",
  units_1_detached = "B25024_002",
  units_1_attached = "B25024_003",
  units_2 = "B25024_004",
  units_3_4 = "B25024_005",
  units_5_9 = "B25024_006",
  units_10_19 = "B25024_007",
  units_20_49 = "B25024_008",
  units_50_plus = "B25024_009",
  units_mobile = "B25024_010",
  units_boat_rv = "B25024_011"
)

# Table B25034: Year Structure Built
year_built_vars <- c(
  yrbuilt_total = "B25034_001",
  yrbuilt_2020_later = "B25034_002",
  yrbuilt_2010_2019 = "B25034_003",
  yrbuilt_2000_2009 = "B25034_004",
  yrbuilt_1990_1999 = "B25034_005",
  yrbuilt_1980_1989 = "B25034_006",
  yrbuilt_1970_1979 = "B25034_007",
  yrbuilt_1960_1969 = "B25034_008",
  yrbuilt_1950_1959 = "B25034_009",
  yrbuilt_1940_1949 = "B25034_010",
  yrbuilt_1939_earlier = "B25034_011"
)

# Table B25038: Tenure by Year Moved
year_moved_vars <- c(
  moved_total = "B25038_001",
  moved_owner = "B25038_002",
  moved_owner_2021_later = "B25038_003",
  moved_owner_2018_2020 = "B25038_004",
  moved_owner_2010_2017 = "B25038_005",
  moved_owner_2000_2009 = "B25038_006",
  moved_owner_1990_1999 = "B25038_007",
  moved_owner_1989_earlier = "B25038_008",
  moved_renter = "B25038_009",
  moved_renter_2021_later = "B25038_010",
  moved_renter_2018_2020 = "B25038_011",
  moved_renter_2010_2017 = "B25038_012",
  moved_renter_2000_2009 = "B25038_013",
  moved_renter_1990_1999 = "B25038_014",
  moved_renter_1989_earlier = "B25038_015"
)

# Table B25064: Median Gross Rent
rent_vars <- c(
  med_gross_rent = "B25064_001"
)

# Table B25070: Gross Rent as % of Income
rent_burden_vars <- c(
  rent_burden_total = "B25070_001",
  rent_burden_lt10 = "B25070_002",
  rent_burden_10_14 = "B25070_003",
  rent_burden_15_19 = "B25070_004",
  rent_burden_20_24 = "B25070_005",
  rent_burden_25_29 = "B25070_006",
  rent_burden_30_34 = "B25070_007",
  rent_burden_35_39 = "B25070_008",
  rent_burden_40_49 = "B25070_009",
  rent_burden_50_plus = "B25070_010",
  rent_burden_not_computed = "B25070_011"
)

# Table B09019: Household Type by Relationship
hh_relationship_vars <- c(
  hh_rel_total = "B09019_001",
  hh_rel_in_hh = "B09019_002",
  hh_rel_householder = "B09019_003",
  hh_rel_spouse_opp = "B09019_010",
  hh_rel_spouse_same = "B09019_011",
  hh_rel_partner_opp = "B09019_012",
  hh_rel_partner_same = "B09019_013",
  hh_rel_child = "B09019_014",
  hh_rel_grandchild = "B09019_018",
  hh_rel_sibling = "B09019_019",
  hh_rel_parent = "B09019_020",
  hh_rel_in_gq = "B09019_026"
)

# Table B01001: Sex by Age
age_vars <- c(
  age_total = "B01001_001",
  # Male age groups
  age_m_under5 = "B01001_003",
  age_m_5_9 = "B01001_004",
  age_m_10_14 = "B01001_005",
  age_m_15_17 = "B01001_006",
  age_m_18_19 = "B01001_007",
  age_m_20 = "B01001_008",
  age_m_21 = "B01001_009",
  age_m_22_24 = "B01001_010",
  age_m_25_29 = "B01001_011",
  age_m_30_34 = "B01001_012",
  age_m_35_39 = "B01001_013",
  age_m_40_44 = "B01001_014",
  age_m_45_49 = "B01001_015",
  age_m_50_54 = "B01001_016",
  age_m_55_59 = "B01001_017",
  age_m_60_61 = "B01001_018",
  age_m_62_64 = "B01001_019",
  age_m_65_66 = "B01001_020",
  age_m_67_69 = "B01001_021",
  age_m_70_74 = "B01001_022",
  age_m_75_79 = "B01001_023",
  age_m_80_84 = "B01001_024",
  age_m_85_plus = "B01001_025",
  # Female age groups
  age_f_under5 = "B01001_027",
  age_f_5_9 = "B01001_028",
  age_f_10_14 = "B01001_029",
  age_f_15_17 = "B01001_030",
  age_f_18_19 = "B01001_031",
  age_f_20 = "B01001_032",
  age_f_21 = "B01001_033",
  age_f_22_24 = "B01001_034",
  age_f_25_29 = "B01001_035",
  age_f_30_34 = "B01001_036",
  age_f_35_39 = "B01001_037",
  age_f_40_44 = "B01001_038",
  age_f_45_49 = "B01001_039",
  age_f_50_54 = "B01001_040",
  age_f_55_59 = "B01001_041",
  age_f_60_61 = "B01001_042",
  age_f_62_64 = "B01001_043",
  age_f_65_66 = "B01001_044",
  age_f_67_69 = "B01001_045",
  age_f_70_74 = "B01001_046",
  age_f_75_79 = "B01001_047",
  age_f_80_84 = "B01001_048",
  age_f_85_plus = "B01001_049"
)

# Combine all variables
all_vars <- c(
  race_vars,
  hisp_race_vars,
  commute_vars,
  hh_type_vars,
  school_vars,
  educ_vars,
  lang_vars,
  income_vars,
  employ_vars,
  tenure_vars,
  units_vars,
  year_built_vars,
  year_moved_vars,
  rent_vars,
  rent_burden_vars,
  hh_relationship_vars,
  age_vars
)

# -----------------------------------------------------------------------------
# Pull ACS data for Minneapolis tracts
# Minneapolis is in Hennepin County, MN (FIPS: 27053)
# -----------------------------------------------------------------------------

# Get ACS 2019-2023 5-year estimates for Hennepin County tracts
hennepin_acs <- get_acs(
  geography = "tract",
  variables = all_vars,
  state = "MN",
  county = "Hennepin",
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = FALSE
)

# Keep only point estimates (ending with "E"), remove MOE columns
hennepin_acs <- hennepin_acs |>
  select(GEOID, ends_with("E")) |>
  rename_with(~ str_remove(.x, "E$"), ends_with("E"))


# -----------------------------------------------------------------------------
# Load Minnesota tract shapefile and filter to Minneapolis
# -----------------------------------------------------------------------------

# Load the Minnesota tract shapefile
mn_tracts <- st_read("minnesota_tracts/tl_2024_27_tract.shp", quiet = TRUE)

# Hennepin County FIPS = 053
hennepin_tracts <- mn_tracts |>
  filter(COUNTYFP == "053")

# Load in Minneapolis Tracts
minneapolis_tracts <- read_csv("minneapolis_tract_crosswalk.csv") |>
  filter(cousub23 == "43000") |> 
  mutate(NAME = gsub("^0+|\\.00$", "", tract))

# filter Hennepin County to only Minneapolis tracts
hennepin_tracts <- hennepin_tracts |>
  filter(NAME %in% minneapolis_tracts$NAME)

# -----------------------------------------------------------------------------
# Merge ACS data with tract geometry
# -----------------------------------------------------------------------------

# Create GEOID from tract shapefile to match ACS
hennepin_tracts <- hennepin_tracts |>
  mutate(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE))

# Join ACS data to geometry
mpls_tracts_sf <- hennepin_tracts |>
  left_join(hennepin_acs, by = "GEOID")

# -----------------------------------------------------------------------------
# Calculate useful derived variables
# -----------------------------------------------------------------------------

mpls_tracts_sf <- mpls_tracts_sf |>
  mutate(
    # Race/ethnicity percentages
    pct_white = (hisp_nh_white / hisp_total) * 100,
    pct_black = (hisp_nh_black / hisp_total) * 100,
    pct_asian = (hisp_nh_asian / hisp_total) * 100,
    pct_hispanic = (hisp_latino / hisp_total) * 100,
    pct_other_race = ((hisp_nh_aian + hisp_nh_nhpi + hisp_nh_other + hisp_nh_two_plus) / hisp_total) * 100,
    
    # Educational attainment (25+)
    pct_bachelors_plus = ((educ_bachelors + educ_masters + educ_professional + educ_doctorate) / educ_total) * 100,
    pct_hs_or_less = ((educ_no_school + educ_hs_diploma + educ_ged) / educ_total) * 100,
    
    # School enrollment - % enrolled in undergrad or grad school (B14007)
    pct_higher_ed = ((school_undergrad + school_grad) / school_total) * 100,
    
    # Language - % English only households (C16002)
    pct_english_only = (lang_english_only / lang_total) * 100,
    
    # Housing tenure
    pct_owner_occupied = (tenure_owner / tenure_total) * 100,
    pct_renter_occupied = (tenure_renter / tenure_total) * 100,
    
    # Commute mode
    pct_drive_alone = (commute_drove_alone / commute_total) * 100,
    pct_transit = (commute_transit / commute_total) * 100,
    pct_wfh = (commute_wfh / commute_total) * 100,
    pct_bike_walk = ((commute_bike + commute_walk) / commute_total) * 100,
    
    # Employment
    unemployment_rate = (employ_unemployed / employ_civilian_lf) * 100,
    labor_force_participation = (employ_in_lf / employ_total) * 100,
    
    # Housing cost burden (30%+ of income on rent)
    pct_rent_burdened = ((rent_burden_30_34 + rent_burden_35_39 + rent_burden_40_49 + rent_burden_50_plus) / 
                           (rent_burden_total - rent_burden_not_computed)) * 100,
    
    # Housing age - % structures built after 2000 (B25034)
    pct_built_pre1940 = (yrbuilt_1939_earlier / yrbuilt_total) * 100,
    pct_built_post2000 = ((yrbuilt_2000_2009 + yrbuilt_2010_2019 + yrbuilt_2020_later) / yrbuilt_total) * 100,
    
    # Multi-family housing - % units in 2+ unit structures (B25024_004 through _009)
    pct_multifamily = ((units_2 + units_3_4 + units_5_9 + units_10_19 + units_20_49 + units_50_plus) / units_total) * 100,
    
    # Year moved into unit - combined owner + renter (B25038)
    pct_moved_pre2000 = ((moved_owner_1990_1999 + moved_owner_1989_earlier + 
                          moved_renter_1990_1999 + moved_renter_1989_earlier) / moved_total) * 100,
    pct_moved_2000_2009 = ((moved_owner_2000_2009 + moved_renter_2000_2009) / moved_total) * 100,
    pct_moved_2010_2020 = ((moved_owner_2010_2017 + moved_owner_2018_2020 + 
                            moved_renter_2010_2017 + moved_renter_2018_2020) / moved_total) * 100,
    pct_moved_2021_later = ((moved_owner_2021_later + moved_renter_2021_later) / moved_total) * 100,
    
    # Same-sex households - % same-sex spouse + partner (B09019)
    pct_same_sex_hh = ((hh_rel_spouse_same + hh_rel_partner_same) / hh_rel_in_hh) * 100,
    
    # Age distribution (B01001)
    # First calculate combined age buckets
    age_0_17 = age_m_under5 + age_m_5_9 + age_m_10_14 + age_m_15_17 +
               age_f_under5 + age_f_5_9 + age_f_10_14 + age_f_15_17,
    age_18_34 = age_m_18_19 + age_m_20 + age_m_21 + age_m_22_24 + age_m_25_29 + age_m_30_34 +
                age_f_18_19 + age_f_20 + age_f_21 + age_f_22_24 + age_f_25_29 + age_f_30_34,
    age_35_49 = age_m_35_39 + age_m_40_44 + age_m_45_49 +
                age_f_35_39 + age_f_40_44 + age_f_45_49,
    age_50_64 = age_m_50_54 + age_m_55_59 + age_m_60_61 + age_m_62_64 +
                age_f_50_54 + age_f_55_59 + age_f_60_61 + age_f_62_64,
    age_65_plus = age_m_65_66 + age_m_67_69 + age_m_70_74 + age_m_75_79 + age_m_80_84 + age_m_85_plus +
                  age_f_65_66 + age_f_67_69 + age_f_70_74 + age_f_75_79 + age_f_80_84 + age_f_85_plus,
    
    # Now calculate percentages for each age bucket
    pct_age_0_17 = (age_0_17 / age_total) * 100,
    pct_age_18_34 = (age_18_34 / age_total) * 100,
    pct_age_35_49 = (age_35_49 / age_total) * 100,
    pct_age_50_64 = (age_50_64 / age_total) * 100,
    pct_age_65_plus = (age_65_plus / age_total) * 100
  )

# -----------------------------------------------------------------------------
# Preview the data
# -----------------------------------------------------------------------------

cat("\n=== Minneapolis Tract Census Data Summary ===\n")
cat("Number of tracts:", nrow(mpls_tracts_sf), "\n")
cat("Variables:", ncol(mpls_tracts_sf), "\n\n")

cat("Summary Statistics:\n")
mpls_tracts_sf |>
  st_drop_geometry() |>
  summarise(
    across(
      c(med_hh_income, med_gross_rent, pct_white, pct_black, pct_hispanic, 
        pct_bachelors_plus, pct_owner_occupied, unemployment_rate,
        pct_age_0_17, pct_age_18_34, pct_age_35_49, pct_age_50_64, pct_age_65_plus),
      list(mean = ~ mean(.x, na.rm = TRUE), median = ~ median(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  print(n = 30)

# -----------------------------------------------------------------------------
# Save the data
# -----------------------------------------------------------------------------

# Save as geopackage (preserves geometry + attributes)
st_write(mpls_tracts_sf, "mpls_tracts_census.gpkg", delete_dsn = TRUE)

# Save as shapefile (create directory first)
if (!dir.exists("mpls_tracts_census")) {
  dir.create("mpls_tracts_census")
}
st_write(mpls_tracts_sf |> select(1:50), "mpls_tracts_census/mpls_tracts_census.shp", 
         delete_layer = TRUE)

# Save tabular data as CSV (without geometry)
mpls_tracts_sf |>
  st_drop_geometry() |>
  write_csv("mpls_tracts_census.csv")

cat("\nData saved to:\n")
cat("  - mpls_tracts_census.gpkg (GeoPackage with all variables)\n")
cat("  - mpls_tracts_census/mpls_tracts_census.shp (Shapefile subset)\n")
cat("  - mpls_tracts_census.csv (CSV without geometry)\n")

# -----------------------------------------------------------------------------
# Quick visualization
# -----------------------------------------------------------------------------

# Median Household Income map
ggplot(mpls_tracts_sf) +
  geom_sf(aes(fill = med_hh_income), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Median HH Income",
    labels = scales::dollar,
    option = "mako"
  ) +
  labs(
    title = "Median Household Income by Census Tract",
    subtitle = "Hennepin County, MN (ACS 2019-2023)",
    caption = "Source: U.S. Census Bureau, American Community Survey"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Percent Non-White Population map
ggplot(mpls_tracts_sf) +
  geom_sf(aes(fill = 100 - pct_white), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "% Non-White",
    option = "mako"
  ) +
  labs(
    title = "Percent Non-White Population by Census Tract",
    subtitle = "Hennepin County, MN (ACS 2019-2023)",
    caption = "Source: U.S. Census Bureau, American Community Survey"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Percent Bachelors Plus map
ggplot(mpls_tracts_sf) +
  geom_sf(aes(fill = pct_bachelors_plus), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "% Bachelors Plus",
    option = "mako"
  ) +
  labs(
    title = "Percent Bachelors Plus by Census Tract",
    subtitle = "Hennepin County, MN (ACS 2019-2023)",
    caption = "Source: U.S. Census Bureau, American Community Survey"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Percent Same-Sex Households map
ggplot(mpls_tracts_sf) +
  geom_sf(aes(fill = pct_same_sex_hh), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "% Same-Sex Households",
    option = "mako"
  ) +
  labs(
    title = "Percent Same-Sex Households by Census Tract",
    subtitle = "Hennepin County, MN (ACS 2019-2023)",
    caption = "Source: U.S. Census Bureau, American Community Survey"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Age 18-34 map
ggplot(mpls_tracts_sf) +
  geom_sf(aes(fill = pct_age_18_34), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "% Age 18-34",
    option = "mako"
  ) +
  labs(
    title = "Percent Age 18-34 by Census Tract",
    subtitle = "Hennepin County, MN (ACS 2019-2023)",
    caption = "Source: U.S. Census Bureau, American Community Survey"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
