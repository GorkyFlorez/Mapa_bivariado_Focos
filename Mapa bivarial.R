library(sf)
library(ggplot2)
library(raster)
library(plyr)
library(tidyverse)
library(RColorBrewer)
library(ggspatial)

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Peru       <- getData('GADM', country='Peru', level=2) %>%st_as_sf() 
Per       <- getData('GADM', country='Peru', level=1) %>%st_as_sf() 

# Capas de focos de calor 
Foco = st_read("SHP/MODIS_C6_1_South_America_24h.shp")  %>% st_as_sf()
Focos <- st_transform(Foco ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Foco_Peru = st_intersection(Focos, Peru)

library(dplyr)
Resu = Foco_Peru %>%
  as_tibble %>%
  group_by(NAME_2)%>%
  summarize (N_Focos =n(),
             temp = mean(BRIGHT_T31))%>%
  ungroup() %>%
  mutate (temp = temp - 273.15)# para pasar grados F a C

Resu 
Peru_Focos_Res = inner_join(Peru,  Resu, by = "NAME_2")
Foco_Peru_xy <- cbind(Foco_Peru, st_coordinates(st_centroid(Foco_Peru$geometry)))
#write_sf(Peru_Focos_Res, "SHP/Focos_Pro.shp")
library(biscale)
k <- 4
datos.RM.bi <- bi_class(Peru_Focos_Res, y = temp, x = N_Focos, dim = k, style = "quantile")

breaks2 <- bi_class_breaks(Peru_Focos_Res, y = temp, x = N_Focos, style = "quantile", 
                           dim = k, dig_lab = c(x = 2, y = 1), split = TRUE)


# Colores usados para el mapa
color <- "PurpleOr"


Map= ggplot() + 
  geom_sf(data = Per , fill="#E3DCEC", color= NA)+
  geom_sf(data=datos.RM.bi, aes(fill = bi_class), color= NA, size = 0.1) +
  bi_scale_fill(pal = color, dim = k) +
  geom_sf(data = Peru , fill=NA, color= "gray", size=0.01)+
  geom_sf(data = Per , fill=NA, color= "black", size=1.5)+
  geom_point(data =Foco_Peru_xy , aes(x = X, y = Y),size=1, alpha=0.3, color="black", pch=21, fill="red")+
  theme_void()+
  theme(legend.position = "none")


library(gridGraphics)
# Crear la leyenda para el mapa
legend1 <- bi_legend(pal = color,
                     dim = k, 
                     xlab = "Nº de Focos",
                     ylab = "Temperatura Cº",
                     breaks = breaks2,
                     arrows = FALSE,
                     size = 8)+
  theme(axis.text.y  = element_text(color="black", size=10, angle=90),
        axis.text.x  = element_text(color="black", size=10),
        axis.title.x = element_text(color="black", size=11),
        axis.title.y = element_text(color="black", size=11))


library(cowplot)
Mapa= ggdraw() +
  coord_equal(xlim = c(0, 25), ylim = c(0, 25), expand = FALSE) +
  draw_plot(Map , width = 24, height = 24,x = 5, y = 0)+
  draw_plot(legend1 , width = 8, height =8,x = 2, y = 3)+
  
  annotate(geom = "text", x = 0.5, y = 22, hjust = 0, vjust = 1,
           label = "Mapa bivariado que compara los
focos de calor y la temperatura 
alcanzada por provincias en 
el Perú rojo (Numero de focos) 
y azul (Temperatura) 
en 07 de Setiembre 2022",
           size = 7,family="serif", face = "italic", color = "black")+
  annotate(geom = "text", x = 0.5, y = 3, hjust = 0, vjust = 1,
           label = "Elaborado: @gflorezc",
           size = 4,family="serif", face = "italic", color = "gray")+
  annotate(geom = "text", x = 0.5, y = 2.7, hjust = 0, vjust = 1,
           label = "Fuente: El sistema de información sobre incendios para la gestión de recursos (FIRMS)",
           size = 4,family="serif", face = "italic", color = "gray60")+


  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))


ggsave(plot=Mapa,"Mapa/Mapa bivariado.png",units = "cm",width = 25, #alto
       height = 25, #ancho
       dpi=1200)









