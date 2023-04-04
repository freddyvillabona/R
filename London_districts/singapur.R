

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
   geom_sf(data=pnts_trans, size=.2, color="black") +
 # geom_sf(data=pnts_trans, size=.001, color="black") +
 # geom_sf(data=points, size=90, size=.001,color="#FCE5B5", alpha=.4) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 20)) +
  labs(fill= "Districts") +
  theme_void() +
  labs(title="\nThefts reported in London", 
       subtitle = "",
       caption = "Datos: Anónimo (2023)\n",
       fill= "Districts") +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    panel.spacing = unit(0, "pt"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black"),
    legend.position = c(.18, .27),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    plot.title = element_text(size = 20, face="bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face="bold", hjust = 0.5),
    plot.caption = element_text(size = 10, face="bold", hjust = 1),
    panel.background = element_rect(fill = "white", colour = "white")
  )


A


g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g
 

library(viridis)

ta$Var1 <- as.character(ta$Var1)
ta <- ta[with(ta, order(-ta$Freq)), ]
  
ta$Var1 <- gsub("Further/Higher Educational Building",
                "Further", ta$Var1)
ta$Var1 <- gsub("Sports/Recreation Area",
                "Recreation Area", ta$Var1)



b <- ggplot(ta, aes(y= reorder(Var1,Freq) , x=Freq)) +
  geom_bar(stat = "identity", fill="gray60", color="gray40") +
  ylab("") +
  xlab("Reported cases") +
  #scale_fill_manual() +
  labs(fill= "Districts") +
  #theme_void() +
  labs(title="\nTitle", 
       fill= "Districts") +
  theme(
    legend.position='none',
    #axis.text.x = element_blank(), 
    #axis.ticks.x = element_blank(),
   # panel.spacing = unit(0, "pt"),
    #panel.border = element_blank(),
    #panel.grid.major.x = element_blank(),
    #strip.background = element_blank(),
   # strip.text = element_text(colour = "black"),
    #legend.box.just = "right",
    #legend.margin = margin(6, 6, 6, 6),
    plot.title = element_text(size = 20, face="bold", hjust = 0),
    plot.subtitle = element_text(size = 16, face="bold", hjust = 0.5),
    plot.caption = element_text(size = 10, face="bold", hjust = 1),
    panel.background = element_rect(fill = "white", colour = "white")
  )
  

b

#library("patchwork")

#A / b


library("cowplot")
ggdraw() +
  draw_plot(A, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(b, x = 0.45, y = .28, width = .2, height = .4) 









ggplot(ta, aes(x=Var1, y=Freq)) +
  geom_bar()



