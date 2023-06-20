source(here::here("R/global.R"))

challenge_files <- list.files(here::here("data"), pattern = "20230617_challenge.*\\.gpx", full.names = TRUE)
challenge <- purrr::map(challenge_files, ~ read_gpx(gpx_file = .x)) %>% list_rbind()

challenge_sf <- challenge %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3067)

min_km <- function(t, d) {
  (t/60)/(d/1000)
}

challenge_sf.rep <- challenge_sf %>%
  mutate(time_sec = as.numeric(seconds(datetime)-seconds(lag(datetime))),
         cum_time_sec = cumsum(replace_na(time_sec, 0)),
         dist_m = as.numeric(st_distance(geometry, lag(geometry), by_element = TRUE)),
         cum_dist_m = cumsum(replace_na(dist_m, 0)),
         min_km = replace_na(min_km(time_sec, dist_m), 0))

# ggplot geom_point_sf
ggplot() +
  geom_sf(data = challenge_sf.rep, aes(color = log(min_km))) +
  scale_color_viridis_c() +
  theme_minimal()
