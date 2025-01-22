
pacman::p_load(dplyr, tidyr, here, ggplot2)

pj <- read.csv("/Users/aen/Documents/ORISE Postdoc/PinyonJayMacroecology/Data/IMBCR_Pinyon_Jay/IMBCR.USFS.PIJA.detections.csv")

head(pj)

# Filter to keep only one row for each combination of 'transectnum' and 'point'
pj_filtered <- pj %>%
  group_by(transectnum, point) %>%
  slice(1) %>%  # Keeps the first row for each unique combination
  ungroup()

# View the result
head(pj_filtered)

write.csv(pj_filtered, "/Users/aen/Documents/ORISE Postdoc/PinyonJayMacroecology/Data/IMBCR_Pinyon_Jay/IMBCR.USFS.PIJA.detections.cleaned.csv", row.names = FALSE)

