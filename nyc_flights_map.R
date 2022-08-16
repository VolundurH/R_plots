flights <- nycflights13::flights
airports <- nycflights13::airports

earth = jpeg::readJPEG("D:/OneDrive - Lund University/maps/BlackMarble_2016_01deg.jpg") %>% 
  grid::rasterGrob()

flights %>% 
  select(flight, tailnum, origin, dest, year, month, day) %>% 
  left_join(airports %>% select(faa, lat, lon), by = c("origin"="faa")) %>% 
  rename(orig_lat = lat, orig_lon = lon) %>% 
  left_join(airports %>% select(lat, lon, faa), by = c("dest"="faa")) %>% 
  rename(dest_lat = lat, dest_lon = lon) %>% 
  drop_na() %>% 
  select(origin, dest, 8:11) %>% 
  group_by_all() %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  as_tibble %>% 
  mutate(vector = pmap(list(orig_lat, orig_lon, dest_lat, dest_lon), function(a,b,d,e){
                       geosphere::gcIntermediate(c(b,a), c(e,d)) %>% as_tibble}
                       )) %>% 
  mutate(group = 1:nrow(.)) %>% 
  unnest %>% 
  mutate(n = n/sum(n) %>% log()) %>% 
  ggplot() +
  annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) + 
  geom_line(aes(lon, lat, alpha = n, group = group), col="white") + 
  theme_void() +
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "black", colour = "black"), 
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
  ) +
  xlim(-140,-54) +
  ylim(20,55) +
  coord_equal()