
################################################################################
# General purpose utilities for processing data
################################################################################

## ---- utils ----
process_loadings <- function(tract_loadings) {
  tract_loadings$group <- str_extract(tract_loadings$variable, "[^_]+")
  tract_loadings$variable <- tract_loadings$variable %>%
    str_extract("_[A-z0-9]+") %>%
    substr(2, 1000)
  tract_loadings
}

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

