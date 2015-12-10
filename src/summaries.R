
################################################################################
# Exploratory summaries of the SF HIP data
################################################################################

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

