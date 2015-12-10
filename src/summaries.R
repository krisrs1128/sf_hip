
################################################################################
# Exploratory summaries of the SF HIP data
################################################################################

## ---- preprocessing ----
# filter tracts
tract_info <- tract_info %>%
  left_join(census_ids, by = "Tract2010")
colnames(tract_info)[colnames(tract_info) == "Id"] <- "id"
colnames(tract_info)[2:8] <- paste0("demo_", colnames(tract_info)[2:8])
tract_info <- tract_info[tract_info$demo_Pop2010 > 1000, ]

# get map data
gpclibPermit()
tract <- readOGR(dsn = "../data/processed_data/gz_2010_06_140_00_500k", layer = "gz_2010_06_140_00_500k")

tract <- fortify(tract, region = "GEO_ID")
tract_map <- merge(tract_info, tract, by = "id", all.x = TRUE)
tract_num <- tract_info %>%
  select(starts_with("demo"), starts_with("crime"), starts_with("license")) %>%
  as.matrix()

# filter questions, and cube root
tract_num <- tract_num[, colSums(tract_num != 0) >= 30]
tract_num <- (tract_num ^ (1 / 3) - 1) / (1 / 3)

rownames(tract_num) <- tract_info$Tract2010

## ---- map-demo ----
ggplot(tract_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = demo_Pop2010)) +
  coord_fixed()

## ---- map-stolen ----
ggplot(tract_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = crime_stolen_property)) +
  coord_fixed()

## ---- map-noncrime ----
ggplot(tract_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = crime_non_criminal)) +
  coord_fixed()

## ---- histos ----
ggplot(tract_info) +
  geom_point(aes(x = demo_Pop2010, y = demo_med_income))
for(j in seq_len(ncol(tract_num))) {
  print(ggplot(data.frame(tract_num)) +
          geom_histogram(aes_string(x = colnames(tract_num)[j])))
}

