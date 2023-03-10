## Figure_Kansas-PerennialStreamMap.R

source(file.path("code", "paths+packages.R"))

## load/prep data
# where KS GIS data is stored
dir_GIS <- "C:/Users/s947z036/OneDrive - University of Kansas/Research/Kansas/GISdata"

# streams
sf_streams <- 
  file.path(dir_GIS, "hydro", "streams", "perennial_1961to2009", "streams_perennial_1961_2009.shp") %>% 
  sf::st_read() %>% 
  # logicals indicating perennial flow in 1961 and 2009
  dplyr::mutate(perennial1961 = !is.na(on1961_1),
                perennial2009 = !is.na(on2009a),
                perennial1961and2009 = perennial1961 & perennial2009) %>% 
  # only streams that are perennial at least one of the years
  subset(perennial1961 | perennial2009)

# high plains aquifer extent
sf_HPA <- 
  file.path(dir_GIS, "hydro", "high_plains_aquifer_extent", "High_Plains_Aquifer_Extent.shp") %>% 
  sf::st_read()

# state of kansas
kansas <- 
  map_data("state") %>% 
  subset(region == "kansas")

## plot - just the data, will be annotated in Inkscape
ggplot() +
  geom_sf(data = sf_HPA, color = NA, fill = "black", alpha = 0.25) +
  geom_sf(data = sf_streams, aes(color = perennial1961and2009)) +
  geom_polygon(data = kansas, aes(x = long, y = lat), color = "black", fill = NA) +
  scale_color_manual(name = NULL, values = c("FALSE" = col.cat.red, "TRUE" = col.cat.blu),
                     labels = c("FALSE" = "Perennial in 1961 but not 2009", "TRUE" = "Perennial in 1961 and 2009")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.position = "none")

ggsave(file.path("figures+tables", "SiteMap_KansasStreams.png"),
       width = 190, height = 95, units = "mm")
ggsave(file.path("figures+tables", "SiteMap_KansasStreams.pdf"),
       width = 190, height = 95, units = "mm")

## dashed intermittent streams for Jim
ggplot() +
  geom_sf(data = sf_HPA, color = NA, fill = "black", alpha = 0.25) +
  geom_sf(data = sf_streams, aes(color = perennial1961and2009, linetype = perennial1961and2009)) +
  geom_polygon(data = kansas, aes(x = long, y = lat), color = "black", fill = NA) +
  scale_color_manual(name = NULL, values = c("FALSE" = col.cat.red, "TRUE" = col.cat.blu),
                     labels = c("FALSE" = "Perennial in 1961 but not 2009", "TRUE" = "Perennial in 1961 and 2009")) +
  scale_linetype_manual(name = NULL, values = c("FALSE" = "dotted", "TRUE" = "solid")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.position = "none")

ggsave(file.path("figures+tables", "SiteMap_KansasStreams-Dashed.png"),
       width = 190, height = 95, units = "mm")
ggsave(file.path("figures+tables", "SiteMap_KansasStreams-Dashed.pdf"),
       width = 190, height = 95, units = "mm")
