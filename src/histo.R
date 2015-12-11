
################################################################################
# Histograms of transformed / untransformed
################################################################################

## ---- transformed-histograms ----
m_tract_unnorm <- tract_num_unnormalized[, c(1:15, 46:51)] %>%
  melt()
ggplot(m_tract_unnorm) +
  geom_histogram(aes(x = value)) +
  facet_wrap(~Var2) +
  ggtitle("Rates, before cube-root transformation and centering")

## ---- untransformed-histograms ----
m_tract_num <- tract_num[, c(1:15, 46:51)] %>%
  melt()
ggplot(m_tract_num) +
  geom_histogram(aes(x = value)) +
  facet_wrap(~Var2) +
  ggtitle("Rates, before cube-root transformation and centering")
