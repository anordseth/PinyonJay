# Pinyon Jay Independent Model Validation in R (Code adapted from Crego et al. 2024)

library(sf)
library(tidyverse)
library(stars)
library(viridis)
library(tmap)
library(PRROC)
set.seed(1984)

theme_set(theme_bw(base_size = 16, base_family = 'Helvetica'))

# Load range shape
GirDist <- st_read("./data/RangesUpdatedJune2020_WGS84.shp") %>%
  st_transform(crs = 32737)
RGDistProb <- GirDist %>%
  filter(SUBSPECIES == 'reticulata', Population == 'Probable')

# Load GBIF validation points
GBIF19_20 <- st_read("./data/GBIF/GBIF_giraffe.shp") %>%
  filter(gbifID %in% c(3743309743,3743228653,3743096560,3743069540,3743042716,3743023556,
                       3743015636,3743006520,3070484707,3039199224,3031922498,3008550700,
                       2851114990,2850895049,2850686796,2850633945,2826424741,2823376444,
                       2823212314,2818835531,2814135458,2641411868,2641347841,2596302844,
                       2580199449,2540851241,2429594905,2429571110,2429401269,2397642062,
                       2397568595,2366102640,2366046955,2350360369,2283155464,2236244509))
GBIF19_20UTM <- st_transform(GBIF19_20, crs = 32737) %>% st_buffer(100) %>% st_transform(crs = 4326)

# Load GEC validation points
GEC15 <- st_read("./data/GEC/RetGir2015.shp") %>%
  st_transform(crs = 32737) %>%
  st_buffer(100) %>%
  st_transform(crs = 4326)

# Load model outputs
RetGir19_20 <- read_stars(".../HSI_RetGirAug17.tif")
RetGir15 <- read_stars(".../HSI_RetGirPrediction2015.tif")

# Extract mean HSI at GBIF validation points
GBIFmean <- aggregate(RetGir19_20, GBIF19_20UTM, FUN = mean, na.rm = TRUE)
GBIFplot <- data.frame(x = GBIFmean[[1]])

# Random background for GBIF
GBIFbbox <- st_as_sfc(st_bbox(GBIF19_20UTM)) %>%
  st_transform(crs = 32737) %>%
  st_buffer(1000) %>%
  st_crop(GirDist, .)
RandGBIF <- st_sample(GBIFbbox, size = 1000) %>% st_buffer(100) %>% st_transform(crs = 4326)
GBIFrand <- aggregate(RetGir19_20, RandGBIF, FUN = mean, na.rm = TRUE)
GBIFrandplot <- data.frame(x = GBIFrand[[1]])

# t-tests
var.test(GBIFplot$x, GBIFrandplot$x)
t.test(GBIFplot$x, GBIFrandplot$x, var.equal = FALSE)

# AUC-PR and metrics
tableGBIF <- map_dfr(1:100, function(i) {
  pres <- data.frame(y = GBIFplot$x)
  abs <- data.frame(y = sample(GBIFrandplot$x, nrow(pres)))
  df <- bind_rows(
    mutate(pres, x = 1),
    mutate(abs, x = 0)
  )
  df$s <- ifelse(df$y > 0.4620, 1, 0)
  cm <- yardstick::conf_mat(df, truth = factor(x), estimate = factor(s))
  sens <- cm$by_class['Sensitivity']
  prec <- cm$by_class['Precision']
  pr <- pr.curve(scores.class0 = pres$y, scores.class1 = abs$y, curve = FALSE)
  tibble(Sens = sens, Pres = prec, AUCPR = pr$auc.integral)
})
summary(tableGBIF)

# Repeat same steps for GEC validation
GECmean <- aggregate(RetGir15, GEC15, FUN = mean, na.rm = TRUE)
GECplot <- data.frame(x = GECmean[[1]])

GECbbox <- st_as_sfc(st_bbox(GEC15)) %>%
  st_transform(crs = 32737) %>%
  st_buffer(1000) %>%
  st_crop(GirDist, .)
RandGEC <- st_sample(GECbbox, size = 1000) %>% st_buffer(100) %>% st_transform(crs = 4326)
GECrand <- aggregate(RetGir15, RandGEC, FUN = mean, na.rm = TRUE)
GECrandplot <- data.frame(x = GECrand[[1]])

tableGEC <- map_dfr(1:100, function(i) {
  pres <- data.frame(y = GECplot$x)
  abs <- data.frame(y = sample(GECrandplot$x, nrow(pres)))
  df <- bind_rows(
    mutate(pres, x = 1),
    mutate(abs, x = 0)
  )
  df$s <- ifelse(df$y > 0.4620, 1, 0)
  cm <- yardstick::conf_mat(df, truth = factor(x), estimate = factor(s))
  sens <- cm$by_class['Sensitivity']
  prec <- cm$by_class['Precision']
  pr <- pr.curve(scores.class0 = pres$y, scores.class1 = abs$y, curve = FALSE)
  tibble(Sens = sens, Pres = prec, AUCPR = pr$auc.integral)
})
summary(tableGEC)