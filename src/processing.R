
################################################################################
# Preprocessing the SFHIP data for later analysis
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
tract_ogr <- readOGR(dsn = "../data/processed_data/gz_2010_06_140_00_500k", layer = "gz_2010_06_140_00_500k")

tract <- fortify(tract_ogr, region = "GEO_ID")
tract_map <- merge(tract_info, tract, by = "id", all.x = TRUE)
tract_num <- tract_info %>%
  select(starts_with("demo"), starts_with("crime"), starts_with("license")) %>%
  as.matrix()

# some filtering and transformations
tract_num <- tract_num[, colSums(tract_num != 0) >= 30]
tract_num_unnormalized <- tract_num
tract_num <- (tract_num ^ (1 / 3) - 1) / (1 / 3)
tract_num <- scale(tract_num, center = T, scale = F)

rownames(tract_num) <- tract_info$Tract2010
rownames(tract_num_unnormalized) <- tract_info$Tract2010
