milk_production_rs_cities <- read.csv2('spreadsheet/table74_rs_cities.csv', skip = 3, stringsAsFactors = FALSE, encoding = "UTF-8")

milk_production_rs_cities <- milk_production_rs_cities[-(1:2),]
milk_production_rs_cities <- milk_production_rs_cities[-(498:510),]

colnames(milk_production_rs_cities) <- gsub("X", '', colnames(milk_production_rs_cities))
milk_production_rs_cities[, 1] <- gsub(" [(]RS[)]","",milk_production_rs_cities[, 1])
colnames(milk_production_rs_cities)[1] <- "Cities"

for(i in 2:ncol(milk_production_rs_cities)){
        milk_production_rs_cities[, i] <- gsub("[...]", "0", milk_production_rs_cities[, i])
        milk_production_rs_cities[, i] <- gsub("[-]", "0", milk_production_rs_cities[, i])
        milk_production_rs_cities[, i] <- as.numeric(as.character(unlist(milk_production_rs_cities[, i])))
}

if (!require(rgdal)) install.packages("rgdal")
library(rgdal)

if (!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(gganimate)) install.packages("gganimate")
library(gganimate)

if (!require(gifski)) install.packages("gifski")
library(gifski)

if(!require(transformr)) install.packages('transformr')
library(transformr)

if(!require(reshape)) install.packages('reshape')
library(reshape)

if(!require(av)) install.packages("av")
library(av)

shape_rs <- readOGR("shape/Municipios_IBGE.shp", "Municipios_IBGE", use_iconv = TRUE, encoding = "UTF-8")

shape_rs@data$Label_N[!shape_rs@data$Label_N %in% milk_production_rs_cities$Cities]
milk_production_rs_cities[239,1] <- "Maçambara"
milk_production_rs_cities[342,1] <- "Restinga Seca"
milk_production_rs_cities[369,1] <- "Santana do Livramento"
milk_production_rs_cities[483,1] <- "Vespasiano Correa"
milk_production_rs_cities[496,1] <- "Westfalia"

milk_production_rs_cities<-milk_production_rs_cities[order(milk_production_rs_cities$Cities),] 
for(j in 2:ncol(milk_production_rs_cities)){
     milk_production_rs_cities[,j] <- as.numeric(milk_production_rs_cities[,j])/1000
}
milk_production_rs_cities$id <- c(1:nrow(milk_production_rs_cities))
milk_production_rs_cities <- reshape::melt(milk_production_rs_cities, id.vars = c("Cities","id"))

shape_rs@data$id <- c(1:nrow(shape_rs@data))
shapefile_df <- fortify(shape_rs, region = 'id') %>% mutate(id = as.numeric(id))
shapefile_RS <- sp::merge(shapefile_df, shape_rs@data,by="id")
map_data <- shapefile_RS %>% left_join(milk_production_rs_cities, by = c("Label_N" = "Cities"))

colnames(map_data)[20] <- "year"
colnames(map_data)[21] <- "milk_production"

map_data <- map_data[,-(14:17)]
map_data <- map_data[,-(8:11)]

map_data[is.na(map_data$milk_production),]
map_data$year <- as.numeric(as.character(map_data$year))

quantile( milk_production_rs_cities$value[milk_production_rs_cities$variable==2017], p = (0:5)/5 )

map_data$cat <- ifelse(map_data$milk_production >= 14.063, 8, 
                ifelse(map_data$milk_production >= 8, 7, 
                ifelse(map_data$milk_production >= 4.212, 6, 
                ifelse(map_data$milk_production >= 1.119, 5, 
                ifelse(map_data$milk_production >= 0, 4, 4)))))

map_data$cat <- factor(map_data$cat, levels = c(8:4), labels = c("14.60 - 62.91", "8.01 - 14.60", "4.21 - 8.00", "1.12 - 4.21", "0.00 - 1.12"))

p <- ggplot() +
    geom_polygon(data = map_data, 
                 aes(fill = cat,
                     x = long, 
                     y = lat, 
                     group = group), 
                 color = "black", 
                 size = 0.1) +
    coord_equal() +
    theme(legend.position = "bottom", 
         legend.title = element_text(size = 20), 
         legend.text = element_text(size = 18), 
         plot.title = element_text(size = 24)) +
    labs(x = NULL, 
         y = NULL, 
         title = "Milk Production in {round(frame_time,0)} - Rio Grande do Sul - Brazil ") + 
    scale_fill_manual(values = rev(colorRampPalette(brewer.pal(5, "Greens"))(5)),
                      name = "Milk Production (Millions of Liters) - Source: IBGE, 2019.",
                      drop = FALSE,
                      guide = guide_legend(direction = "horizontal",
                                           keyheight = unit(4, units = "mm"),keywidth = unit(40, units = "mm"),
                                           title.position = 'top',
                                           title.hjust = 0.5,
                                           label.hjust = 0.5,
                                           nrow = 1,
                                           byrow = T,
                                           reverse = T,
                                           label.position = "bottom")) + 
    transition_time(year)

animate(p, nframes = 220, fps = 10, width = 1500, height = 1200, renderer = av_renderer('animation.mp4'))+  ease_aes('cubic-in-out')

animate(p, nframes = 55, fps = 10, width = 1500, height = 1200, renderer = gifski_renderer("gganimsss.gif")) +  ease_aes('cubic-in-out')