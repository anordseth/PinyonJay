library(tidyverse)
library(sf)


# === SETUP === #
data_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/IMBCR_Pinyon_Jay/"


# === CLEANING FUNCTION === #
remove_quotes <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ str_replace_all(., '\\"', '') %>% str_remove_all('"')))
}


# === LOAD RAW DATA === #

# effort - point-level survey effort records w/ sampling date, BCR, and identifiers
effort <- read_csv(file.path(data_dir, "IMBCR.USFS.effort.csv")) %>% remove_quotes()

# locs - master sample frame w/ spatial coordinates + metadata for each sample unit
locs <- read_csv(file.path(data_dir, "IMBCR.USFS.samplelocs.csv")) %>% remove_quotes()

# pija - all IMBCR locations where Pinyon Jays were detected
pija <- read_csv(file.path(data_dir, "IMBCR.USFS.PIJA.detections.csv")) %>% remove_quotes()


# === MERGE EFFORT WITH LOCATIONS === #

# Join effort + location by transectnum and point
effort_locs <- effort %>%
  left_join(locs, by = c("transectnum", "point")) %>%
  rename(stratum_effort = stratum.x,
         stratum_locs = stratum.y,
         cyear_effort = cyear.x,
         cyear_locs = cyear.y)

# Filter out rows with no UTM coords
effort_locs <- effort_locs %>%
  filter(!is.na(easting), !is.na(northing))


# === ADD PRESENCE COLUMN === #

# Flag presence based on whether the sample unit had a PIJA detection in the detection dataset
effort_locs2 <- effort_locs %>%
  mutate(presence = if_else(paste0(transectnum, "_", point) %in%
                              paste0(pija$transectnum, "_", pija$point), 1, 0))


# === FINAL CLEANUP === #

# Convert UTM to lat/lon
coords_sf <- st_as_sf(effort_locs2, coords = c("easting", "northing"), crs = 32612)
coords_ll <- st_transform(coords_sf, crs = 4326)

head(coords_ll)

# Add region (e.g., from transect string if "BCR18" etc. is embedded)
effort_clean <- coords_ll %>%
  mutate(
    year = cyear_effort,
    month = cmonth,
    day = cday,
    region = str_extract(transectnum, "BCR\\d+"),
    uid = idsampleunits,
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(uid, transectnum, point, year, month, day, region, presence, lon, lat)

head(effort_clean)

# === COLLAPSE TO ONE ROW PER VISIT === #

effort_final <- effort_clean %>%
  group_by(transectnum, point, year, month, day, region, lon, lat) %>%
  dplyr::summarise(presence = max(presence), .groups = "drop")

head(effort_final)


# === EXPORT === #

# Export to CSV for validation in GEE or other platforms
write_csv(effort_final, "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R/1_Data/PJ_Obs/IMBCR_PJ_Validation.csv")



# === MAKING SURE BCRS ARE ASSIGNED === #

hist(effort_final$year)

effort_sf <- st_as_sf(effort_final, coords = c("lon", "lat"), crs = 4326)

# bcr_clip2 comes from PJBoundaries.R
pj_bcr <- st_join(effort_sf, bcr_clip2, join = st_intersects)

# Summarize presence points by region and year
presence_summary <- pj_bcr %>%
  filter(presence == 1) %>%
  count(bcr_label, year)


check <- effort_final %>%
  filter(region == "BCR9", 
         presence == 1)


