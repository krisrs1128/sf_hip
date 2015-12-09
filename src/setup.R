
################################################################################
# Simple experiment using canonical correlation on the SF HIP data
################################################################################

## ---- libraries ----
library("data.table")
library("FactoMineR")
library("ggplot2")
library("maptools")
library("rgdal")
library("plyr")
library("dplyr")
theme_set(theme_bw())

## ---- get-data ----
tract_info <- fread("data/processed_data/crime_census_alcohol.csv")
census_ids <- fread("data/processed_data/census_tract_demographics.csv") %>%
    select(Tract2010, Id)

tract_info <- tract_info %>%
  left_join(census_ids, by = "Tract2010")
colnames(tract_info)[colnames(tract_info) == "Id"] <- "id"
colnames(tract_info)[2:8] <- paste0("demo_", colnames(tract_info)[2:8])
tract_info <- tract_info[tract_info$demo_Pop2010 > 1000, ]

gpclibPermit()
tract <- readOGR(dsn = "data/processed_data/gz_2010_06_140_00_500k", layer = "gz_2010_06_140_00_500k")
tract <- fortify(tract, region = "GEO_ID")

tract_map <- merge(tract_info, tract, by = "id", all.x = TRUE)

ggplot(tract_info) +
  geom_point(aes(x = demo_Pop2010, y = demo_med_income))

tract_num <- tract_info %>%
  select(starts_with("demo"), starts_with("crime"), starts_with("license")) %>%
  as.matrix()

tract_num <- tract_num[, colSums(tract_num != 0) >= 30]
tract_num <- (tract_num ^ (1 / 3) - 1) / (1 / 3)

for(j in seq_len(ncol(tract_num))) {
  print(ggplot(data.frame(tract_num)) +
          geom_histogram(aes_string(x = colnames(tract_num)[j])))
  dev.new()
}

rownames(tract_num) <- tract_info$Tract2010

## simple pca
tract_pca <- princomp(scale(tract_num))
tract_scores <- data.frame(Tract2010 = rownames(tract_num),
                           tract_pca$scores)
ggplot(tract_scores) +
  geom_text(aes(x = Comp.1, y = Comp.2, label = Tract2010), size = 3)

tract_scores_map <- merge(tract_scores, tract_map, by = "Tract2010")

ggplot(tract_scores_map) +
  geom_polygon(aes(x = long, y = lat, fill = Comp.1, group = group)) +
  scale_fill_gradient2(midpoint = 0) +
  coord_fixed() +
  ggtitle("Shaded by comp.1")
ggplot(tract_scores_map) +
  geom_polygon(aes(x = long, y = lat, fill = Comp.2, group = group)) +
  scale_fill_gradient2(midpoint = 0) +
  coord_fixed() +
  ggtitle("Shaded by comp.2")

# to interpret, use the second component
tract_loadings <- data.frame(variable = rownames(tract_pca$loadings), tract_pca$loadings[, 1:2])
ggplot(tract_loadings) +
  geom_text(aes(x = Comp.1, y = Comp.2, label = variable), size = 3)

ggplot(tract_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = demo_Pop2010)) +
  coord_fixed()

ggplot(tract_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = crime_stolen_property)) +
  coord_fixed()
ggplot(tract_map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = crime_non_criminal)) +
  coord_fixed()

demo <- scale(tract_num[, grep("demo", colnames(tract_num))])
crime <- scale(tract_num[, grep("crime", colnames(tract_num))])
license <- scale(tract_num[, grep("license", colnames(tract_num))])

crime_license <- cancor(crime, license)
crime_license$cor

table_scores <- function(tab, fit, type = "xcoef") {
  scores <- data.frame(tract = rownames(tab),
                       score = tab %*% fit[[type]][, 1:2])
  loadings <- data.frame(variable = colnames(tab),
                         loadings = fit[[type]][, 1:2])
  p1 <- ggplot(scores) +
    geom_text(aes(x = score.1, y = score.2, label = tract), size = 3)
  p2 <- ggplot(loadings) +
    geom_text(aes(x = loadings.1, y = loadings.2, label = variable), size = 3)
  list(p1, p2)
}

table_scores(crime, crime_license, "xcoef")
table_scores(license, crime_license, "ycoef")

## finally can do multiple factor analysis
tract_mfa <- MFA(scale(tract_num), c(ncol(demo), ncol(crime), ncol(license)))
tract_mfa

census_ids <- fread("data/processed_data/census_tract_demographics.csv")
filter(census_ids, Tract2010 == 17700)
