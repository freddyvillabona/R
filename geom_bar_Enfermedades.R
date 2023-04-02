
library("dplyr")
library("ggplot2")

data <- read.csv("data.csv")

ggplot(subset(data, Freq > 0),
       aes(x = Cond_pre, y = Freq, fill = Cond_pre)) +
  geom_col() +
  facet_grid(~Age, space = "free_x", scales = "free_x", switch = "x") +
  scale_x_discrete(name = "Edad", expand = c(0, 1)) +
  ylab("Porcentaje de condición preexistente\n") +
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b",
                               "#abdda4", "#66c2a5", "#3288bd", "#9970ab")) +
  labs(title="\nPorcentaje de condición preexistente", 
       subtitle = "",
       caption = "Datos: Anónimo (2023)\n",
       fill= "Condición preexistente") +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    panel.spacing = unit(0, "pt"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(colour = "black"),
    legend.position = c(.42, .90),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    plot.title = element_text(size = 20, face="bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face="bold", hjust = 0.5),
    plot.caption = element_text(size = 10, face="bold", hjust = 1),
    panel.background = element_rect(fill = "white", colour = "white")
  )
