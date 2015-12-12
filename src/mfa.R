
################################################################################
# Multiple factor analysis
################################################################################

## ---- run-mfa ----
tract_mfa <- MFA(tract_num, c(ncol(demo), ncol(crime), ncol(license)), graph = F)
head(tract_mfa$eig)
tract_mfa$group$RV

mfa_loadings <- tract_mfa$quanti.var$coord
mfa_scores <- data.frame(tract_num %*% mfa_loadings)
mfa_loadings <- data.frame(mfa_loadings, variable = rownames(mfa_loadings))
mfa_scores$Tract2010 <- tract_info$Tract2010

## ---- mfa-scores-loadings ----
ggplot(mfa_scores) +
  geom_text(aes(x = Dim.1, y = Dim.2, label = Tract2010), size = 2)+
  ggtitle("MFA Scores")
mfa_loadings <- process_loadings(mfa_loadings)
ggplot(mfa_loadings) +
  geom_text(aes(x = Dim.1, y = Dim.2, col = group, label = variable), size = 2) +
  ggtitle("MFA Loadings")

## ---- mfa-scores-map ----
mfa_scores_map <- mfa_scores %>%
  melt(id.vars = "Tract2010") %>%
  filter(variable %in% paste0("Dim.", 1:4)) %>%
  left_join(tract_map, by = "Tract2010")

ggplot(mfa_scores_map) +
  geom_polygon(aes(x = long, y = lat, fill = value, group = group)) +
  scale_fill_gradient2(midpoint = 0) +
  facet_wrap(~ variable) +
  coord_fixed() +
  ggtitle("MFA Scores across Census Tracts")
