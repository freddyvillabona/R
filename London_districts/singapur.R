
library('sf')
library("ggplot2")
library("dplyr")
library("stringr")

london <- read_sf('data/ESRI/London_Borough_Excluding_MHW.shp')
london <- filter(london, NAME %in% c("Ealing", "Hillingdon", "Hounslow"))

# london_crimes

london_crimes <- read.csv("london_crimes.csv")
#london_crimes <- head(london_crimes,1000)
names(london_crimes)


points = st_as_sf(london_crimes, coords=c("longitude", "latitude"))
points <- points %>%
  st_set_crs(4326)

points <- st_transform(points, 4326)
london <- st_transform(london, 4326)


pnts_trans <- points %>% mutate(
  intersection = as.integer(st_intersects(points,london)))  %>%
  filter(intersection >=0)


crimes <- pnts_trans
ta <- as.data.frame(table(crimes$location_type))


pnts_trans <- pnts_trans %>% 
  st_as_sf()






# América

london <-london %>% 
  st_as_sf()

names(london)


A <- ggplot(data = london) +
  ggtitle("London") +
  xlab("") +
  ylab("") +
  geom_sf(aes(fill=NAME), show.legend = T, size = 0.05) +
  scale_fill_viridis_d(alpha = .6) +
  #geom_sf_text(aes(label = NAME), size=4)+
  # geom_label(aes(fill = Ucraniano), colour = "white", fontface = "bold") +
  #theme_void() +
  geom_sf(data=pnts_trans, size=.001, color="black") +
 # geom_sf(data=points, size=90, size=.001,color="#FCE5B5", alpha=.4) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 20)) +
  labs(fill= "Districts")

A

