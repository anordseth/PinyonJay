pacman::p_load(dplyr, tidyr, here, ggplot2, sf)

# Load movement data
pj <- read.csv(here::here("1_Data", "PinyonJayGBIF2.csv")) %>%
  filter(year > 1999,
         year < 2023)

# Step 1: Filter rows where coordinateUncertaintyInMeters is < 2500 or missing
pj.cl <- pj %>%
  filter(coordinateUncertaintyInMeters < 1000 | is.na(coordinateUncertaintyInMeters))

# Step 2: Set the grain size (spatial resolution in meters)
grain.size <- 30

# Step 3: Convert to sf object using latitude and longitude columns (assuming these are available)
# Replace 'longitude' and 'latitude' with actual column names for coordinates
pj.sf <- pj.cl %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# Step 4: Reproject to a metric CRS (Web Mercator)
pj.sf <- pj.sf %>%
  st_transform(crs = 3857)

# Step 6: Apply rounding to mimic raster cell aggregation (grain size in meters)
pj.sf <- pj.sf %>%
  mutate(geometry = st_set_precision(geometry, precision = grain.size))

# Step 7: Remove duplicates based on geometry
pj.fin <- pj.sf %>%
  distinct(geometry, .keep_all = TRUE)

# Step 9: Plot Number of Pinyon Jay Observations per Year
ggplot(pj.fin, aes(x = year)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Pinyon Jay Observations per Year",
       x = "Year",
       y = "Number of Observations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(pj.fin, "1_Data/pj_clean.csv")
