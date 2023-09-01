#Created by: Luciano Brum
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Custom function to install packages if not already installed
install_if_not_installed <- function(pkg, repo = "https://cran.r-project.org") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = repo)
    if (!requireNamespace(pkg, quietly = TRUE)) {
      warning(paste("Unable to install the '", pkg, "' package."))
    }
  }
}

# List of packages to install if not already installed
required_packages <- c("rgdal", "rgeos", "dplyr", "ggplot2", "gganimate", "RColorBrewer", "gifski", "transformr")

# Install required packages
lapply(required_packages, install_if_not_installed)

# Load required packages
lapply(required_packages, library, character.only = TRUE)

# file path with milk production of each city in RS state 
milk_production_rs_cities_path <- '../spreadsheet/table74_rs_cities.csv'

# import data 
milk_production_rs_cities <- read.csv2(milk_production_rs_cities_path, skip = 4, stringsAsFactors = FALSE, encoding = "UTF-8", sep = ',')

# removing useless data
milk_production_rs_cities <- milk_production_rs_cities[-498,]

# data processing
colnames(milk_production_rs_cities) <- gsub("X", '', colnames(milk_production_rs_cities))
milk_production_rs_cities[, 1] <- gsub(" [(]RS[)]","",milk_production_rs_cities[, 1])
colnames(milk_production_rs_cities)[1] <- "Cities"

for(i in 2:ncol(milk_production_rs_cities)){
        milk_production_rs_cities[, i] <- gsub("[...]", "0", milk_production_rs_cities[, i])
        milk_production_rs_cities[, i] <- gsub("[-]", "0", milk_production_rs_cities[, i])
        milk_production_rs_cities[, i] <- as.numeric(as.character(unlist(milk_production_rs_cities[, i])))
}

# shapefile path
shape_rs_path <- "../shape/Municipios_IBGE.shp"

# import shapefile
shape_rs <- readOGR(shape_rs_path, "Municipios_IBGE", use_iconv = TRUE, encoding = "UTF-8")

# check cities names between shapefile and .csv
shape_rs@data$Label_N[!shape_rs@data$Label_N %in% milk_production_rs_cities$Cities]

# turns different names equals
milk_production_rs_cities[239,1] <- "Maçambara"
milk_production_rs_cities[342,1] <- "Restinga Seca"
milk_production_rs_cities[369,1] <- "Santana do Livramento"
milk_production_rs_cities[483,1] <- "Vespasiano Correa"
milk_production_rs_cities[496,1] <- "Westfalia"

# data processing
milk_production_rs_cities <- milk_production_rs_cities[order(milk_production_rs_cities$Cities),] 
for(j in 2:ncol(milk_production_rs_cities)){
     milk_production_rs_cities[,j] <- as.numeric(milk_production_rs_cities[,j])/1000
}

# data transformation
milk_production_rs_cities$id <- c(1:nrow(milk_production_rs_cities))
milk_production_rs_cities <- reshape::melt(milk_production_rs_cities, id.vars = c("Cities","id"))

# data integration (csv data and shapefile) 
shape_rs@data$id <- c(1:nrow(shape_rs@data))
shapefile_df <- fortify(shape_rs, region = 'id') %>% mutate(id = as.numeric(id))
shapefile_RS <- sp::merge(shapefile_df, shape_rs@data,by="id")
map_data <- shapefile_RS %>% left_join(milk_production_rs_cities, by = c("Label_N" = "Cities"))

# better column names
colnames(map_data)[20] <- "year"
colnames(map_data)[21] <- "milk_production"

# data processing
map_data <- map_data[,-(14:17)]
map_data <- map_data[,-(8:11)]
map_data[is.na(map_data$milk_production),]
map_data$year <- as.numeric(as.character(map_data$year))

# quantil to use the right ranges for colors
quantile( milk_production_rs_cities$value[milk_production_rs_cities$variable==2018], p = (0:5)/5 )

map_data$cat <- ifelse(map_data$milk_production >= 13.4912, 8, 
                ifelse(map_data$milk_production >= 7.4184, 7, 
                ifelse(map_data$milk_production >= 3.7040, 6, 
                ifelse(map_data$milk_production >= 1.0870, 5, 
                ifelse(map_data$milk_production >= 0, 4, 4)))))

map_data$cat <- factor(map_data$cat, levels = c(8:4), labels = c("13.49 - 60.40", "7.42 - 13.49", "3.70 - 7.42", "1.09 - 3.70", "0.00 - 1.09"))

# generating map of milk production
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
         legend.title = element_text(size = 20, color = "white"), 
         legend.text = element_text(size = 18, color = "white"),
         legend.background = element_rect(fill = "black"),
         plot.title = element_text(size = 24, color = "white"),
         panel.background = element_rect(fill = "black"),
         panel.grid.minor.y = element_line(size =.1, color = "grey"),
         panel.grid.minor.x = element_line(size =.1, color = "grey"),
         panel.grid.major.y = element_line(size =.1, color = "grey"),
         panel.grid.major.x = element_line(size =.1, color = "grey"),
         plot.background = element_rect(fill = "black"),
         axis.text.x = element_text(color = "white"),
         axis.text.y = element_text(color = "white"),
         axis.title.x = element_text(color = "white"),
         axis.title.y = element_text(color = "white")) +
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

animate(p, nframes = 220, fps = 10, width = 1290, height = 1200, renderer = gifski_renderer("gif3.gif")) +  ease_aes('cubic-in-out')
