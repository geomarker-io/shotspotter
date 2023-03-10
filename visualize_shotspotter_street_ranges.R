library(sf)
library(dplyr)

d <- readRDS("shotspotter_street_ranges.rds")

d <- d |>
  filter(!is.na(street_ranges)) |>
  group_by(address_x, street_ranges) |>
  summarize(n = n()) |>
  rowwise() |>
  mutate(geometry = st_sfc(st_union(street_ranges))) |>
  st_as_sf() |>
  select(-street_ranges) |>
  arrange(n)

mapview::mapview(filter(d, n > 5), zcol = "n")


neigh <-
  cincy::neigh_cchmc_2020 |>
  filter(neighborhood %in% c("Avondale", "E. Price Hill", "W. Price Hill"))

d <-
  d |>
  st_transform(st_crs(neigh)) |>
  st_crop(neigh)


the_roads <-
  tigris::roads(state = "39", county = "061") |>
  st_transform(st_crs(neigh)) |>
  st_crop(neigh) |>
  filter(MTFCC %in% c("S1100", "S1200"))

library(ggplot2)

ggplot(d) +
  geom_sf(aes(linetype = MTFCC), data = the_roads, color = "light grey", linewidth = 1.5) +
  geom_sf(aes(color = n), linewidth = 1.5) +
  viridis::scale_color_viridis(trans = "log") +
  CB::theme_map()

ggsave("shots_map.svg")
