source(here::here("R/global.R"))

challenge_files <- list.files(here::here("data"), pattern = "20230617_challenge.*\\.gpx", full.names = TRUE)
challenge <- purrr::map(challenge_files, ~ read_gpx(gpx_file = .x)) %>% list_rbind()

challenge_sf <- challenge %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3067)



# ggplot geom_point_sf
ggplot() +
  geom_sf(data = challenge_sf, aes(color = hr)) +
  scale_color_viridis_c() +
  theme_minimal()
