
################################################################################
# PCA across census tracts
################################################################################

## ---- run-pca ----
tract_pca <- princomp(scale(tract_num))
tract_scores <- data.frame(Tract2010 = rownames(tract_num),
                           tract_pca$scores)

## ---- pca-scores ----
ggplot(tract_scores) +
  geom_text(aes(x = Comp.1, y = Comp.2, label = Tract2010), size = 3) +
  ggtitle("PCA Scores")

## ---- pca-map-comp1 ----
tract_scores_map <- merge(tract_scores, tract_map, by = "Tract2010")
ggplot(tract_scores_map) +
  geom_polygon(aes(x = long, y = lat, fill = Comp.1, group = group)) +
  scale_fill_gradient2(midpoint = 0) +
  coord_fixed() +
  ggtitle("Shaded by comp.1")

## ---- pca-map-comp2 ----
ggplot(tract_scores_map) +
  geom_polygon(aes(x = long, y = lat, fill = Comp.2, group = group)) +
  scale_fill_gradient2(midpoint = 0) +
  coord_fixed() +
  ggtitle("Shaded by comp.2")

## ---- pca-loadings ----
tract_loadings <- data.frame(variable = rownames(tract_pca$loadings), tract_pca$loadings[, 1:2])
ggplot(tract_loadings) +
  geom_text(aes(x = Comp.1, y = Comp.2, label = variable), size = 3)
