

library('sf')
library("ggplot2")
library("dplyr")
library("stringr")

london <- read_sf('data/ESRI/London_Borough_Excluding_MHW.shp')

london_city <- read_sf('data/ESRI/London_Borough_Excluding_MHW.shp')

london_city <- london_city %>%
  mutate(n="A")

london_city <-london_city %>% 
  st_as_sf()

london <- filter(london, NAME %in% c("Ealing", "Hillingdon", "Hounslow"))

# london_crimes

london_crimes <- read.csv("london_crimes.csv")
london_crimes <- head(london_crimes,1000)
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



A <- ggplot() +
  ggtitle("London") +
  xlab("") +
  ylab("") +

  geom_sf(data = london_city, show.legend = F) +
  
 # geom_sf(data = london_city, mapping = aes(fill="London", color="yellow"),color="yellow", 
  #        show.legend = F, size = 0.05) +
  
  
  geom_sf(data = london, aes(fill=NAME), show.legend = T, size = 0.05) +
  scale_fill_viridis_d(alpha = .6) +

  #geom_sf_text(aes(label = NAME), size=4)+
  # geom_label(aes(fill = Ucraniano), colour = "white", fontface = "bold") +
  #theme_void() +
   geom_sf(data=points, size=.001, color="black") +
 # geom_sf(data=pnts_trans, size=.001, color="black") +
 # geom_sf(data=points, size=90, size=.001,color="#FCE5B5", alpha=.4) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 20)) +
  labs(fill= "Districts")

A


g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g
 

library(viridis)

ta$Var1 <- as.character(ta$Var1)
ta <- ta[with(ta, order(-ta$Freq)), ]
  

b <- ggplot(ta, aes(y= reorder(Var1,Freq) , x=Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual() +
  theme(legend.position='none')



library("patchwork")

A / b


library("cowplot")
ggdraw() +
  draw_plot(A, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(b, x = 0.5, y = .5, width = .3, height = .3) 



x = c(0, 0.5, 0), y = c(1, 1, 0.5))





ggplot(ta, aes(x=Var1, y=Freq)) +
  geom_bar()



