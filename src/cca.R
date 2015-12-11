
################################################################################
# Canonical Correlations on a couple tables from the SF HIP data
################################################################################
## ---- run-cca ----
demo <- subcols(tract_num, "demo")
crime <- subcols(tract_num, "crime")
license <- subcols(tract_num, "license")

crime_license <- cancor(crime, license)
crime_license$cor

## ---- cca-get-plots ----
crime_scores <- table_scores(crime, crime_license, "xcoef")
license_scores <- table_scores(license, crime_license, "ycoef")

## ---- cca-crime-scores ----
crime_scores[[1]] +
  ggtitle("CCA Scores from Top Crime Eigenvectors")

## --- cca-license-scores ----
license_scores[[1]] +
  ggtitle("CCA Scores from Top License Eigenvectors")

## ---- cca-top-correlation ----
ggplot(data.frame(tract = crime_scores[[3]][, 1],
                  crime = crime_scores[[3]][, 2],
                  license = license_scores[[3]][, 2])) +
  geom_text(aes(x = crime, y = license, label = tract), size = 3) +
  coord_fixed() +
  ggtitle("Correlation between top scores")

## ---- cca-map-crime-comp-1 ----
cca_scores_map <- merge(crime_scores[[3]], tract_map, by = "Tract2010")
ggplot(cca_scores_map) +
  geom_polygon(aes(x = long, y = lat, fill = score.1, group = group)) +
  scale_fill_gradient2(midpoint = 0) +
  coord_fixed() +
  ggtitle("Shaded by comp.1")





