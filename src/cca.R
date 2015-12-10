
################################################################################
# Canonical Correlations on a couple tables from the SF HIP data
################################################################################

## ---- utils ----
table_scores <- function(tab, fit, type = "xcoef") {
  scores <- data.frame(Tract2010 = rownames(tab),
                       score = tab %*% fit[[type]][, 1:2])
  loadings <- data.frame(variable = colnames(tab),
                         loadings = fit[[type]][, 1:2])
  p1 <- ggplot(scores) +
    geom_text(aes(x = score.1, y = score.2, label = Tract2010), size = 3)
  p2 <- ggplot(loadings) +
    geom_text(aes(x = loadings.1, y = loadings.2, label = variable), size = 3)
  list(p1, p2, scores)
}

subcols <- function(X, start_name) {
  data.frame(X) %>%
    select(starts_with(start_name)) %>%
    as.matrix()
}

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





