
################################################################################
# DPCOA using spatial map information
################################################################################

# get distances between census tracts
keep_ix <- match(census_ids$Id, tract_ogr$GEO_ID)
centers <- sapply(tract_ogr@polygons[keep_ix], function(x) x@labpt) %>%
  t() %>%
  data.frame()
centers$Tract2010 <- census_ids$Tract2010
centers <- centers %>% filter(Tract2010 %in% rownames(tract_num))
D <- dist(centers[, 1:2])

# run dpcoa
tdf <- data.frame(t(tract_num_unnormalized))
dpcoa_result <- dpcoa(tdf, sqrt(D), scannf = F, nf = 4)

dpcoa_scores <- dpcoa_result$dls
dpcoa_scores$Tract2010 <- as.integer(rownames(tract_num))

## ---- dpcoa-scores-plot ----
ggplot(dpcoa_scores) +
  geom_text(aes(x = CS1, y = CS2, label = Tract2010), size = 3)

ggplot(dpcoa_scores) +
  geom_text(aes(x = CS1, y = CS2, label = Tract2010), size = 3) +
  ggtitle("DPCoA Scores")

## ---- dpcoa-scores-map ----
dpcoa_map_scores <- dpcoa_scores %>%
  melt(id.vars = "Tract2010") %>%
  filter(variable %in% paste0("CS", 1:4)) %>%
  left_join(tract_map)

ggplot(dpcoa_map_scores) +
  geom_polygon(aes(x = long, y = lat, fill = value, group = group)) +
  scale_fill_gradient2(midpoint = 0) +
  facet_wrap(~ variable) +
  coord_fixed() +
  ggtitle("DPCoA Scores")

## ---- dpcoa-loadings ----
dpcoa_loadings <- data.frame(dpcoa_result$li,
                             variable = rownames(dpcoa_result$li)) %>%
                               process_loadings()
ggplot(dpcoa_loadings) +
  geom_text(aes(x = Axis1, y = Axis2, label = variable, col = group), size = 3) +
  ggtitle("Loadings from DPCoA")
