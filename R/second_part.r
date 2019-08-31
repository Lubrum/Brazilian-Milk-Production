if (!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
library(rgdal)

shape_rs <- readOGR('../shape/Municipios_IBGE.shp', use_iconv = TRUE, encoding = "utf8")
milk_production_2017_rs <- read.csv('../spreadsheet/table6783_rs.csv', skip = 5, stringsAsFactors = FALSE, encoding = "UTF-8", sep = ';')
milk_production_2006_rs <- read.csv2('../spreadsheet/table933_rs.csv', skip = 5, stringsAsFactors = FALSE, encoding = "UTF-8")

milk_production_2006_rs <- milk_production_2006_rs[,-2]
milk_production_2006_rs <- milk_production_2006_rs[-(493:503),]
milk_production_2017_rs <- milk_production_2017_rs[-(497:511),]
for(i in 1:nrow(milk_production_2017_rs)){
    milk_production_2017_rs[i,2] <- gsub("X", '0', milk_production_2017_rs[i,2])
    milk_production_2017_rs[i,1] <- gsub(" [(]RS[)]", "", milk_production_2017_rs[i,1])
}
for(i in 1:nrow(milk_production_2006_rs)){
    milk_production_2006_rs[i,2] <- gsub("X", '0', milk_production_2006_rs[i,2])
    milk_production_2006_rs[i,1] <- gsub(" [(]RS[)]", "", milk_production_2006_rs[i,1])
}

shape_rs$Label_N[!shape_rs$Label_N %in% milk_production_2006_rs$Município]

milk_production_2006_rs[235,1] <- "Maçambara"
milk_production_2006_rs[337,1] <- "Restinga Seca"
milk_production_2006_rs[364,1] <- "Santana do Livramento"
milk_production_2006_rs[478,1] <- "Vespasiano Correa"
milk_production_2006_rs[491,1] <- "Westfalia"

shape_rs$Label_N[!shape_rs$Label_N %in% milk_production_2017_rs$Município]

milk_production_2017_rs[238,1] <- "Maçambara"
milk_production_2017_rs[341,1] <- "Restinga Seca"
milk_production_2017_rs[368,1] <- "Santana do Livramento"
milk_production_2017_rs[482,1] <- "Vespasiano Correa"
milk_production_2017_rs[495,1] <- "Westfalia"

colnames(milk_production_2006_rs) <- c("City","Milk_2006")
colnames(milk_production_2017_rs) <- c("City","Milk_2017")

if (!require(reshape)) install.packages("reshape")
library(reshape)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

shape_rs@data$id <- c(1:nrow(shape_rs@data))
shapefile_df <- fortify(shape_rs, region = 'id') %>% mutate(id = as.numeric(id))
shapefile_RS <- sp::merge(shapefile_df, shape_rs@data,by="id")

map_data <- shapefile_RS %>% left_join(milk_production_2006_rs, by = c("Label_N" = "City"))
map_data <- map_data %>% left_join(milk_production_2017_rs, by = c("Label_N" = "City"))

map_data$Milk_2017 <- as.numeric(map_data$Milk_2017)
map_data$Milk_2006 <- as.numeric(map_data$Milk_2006)

map_data[is.na(map_data$Milk_2006), 19] <- 0
map_data[is.na(map_data$Milk_2017), 20] <- 0

for(i in 1:nrow(map_data)){
    if(map_data$Milk_2006[i] != 0){
        map_data$Percentual[i] <- (map_data$Milk_2017[i] - map_data$Milk_2006[i]) / map_data$Milk_2006[i]
    }
    else{
        map_data$Percentual[i] <- 100
    }
}
map_data$cat <- ifelse(map_data$Percentual < -0.5, 1, 
                ifelse(map_data$Percentual < -0.25, 2, 
                ifelse(map_data$Percentual < 0, 3, 
                ifelse(map_data$Percentual < 0.1, 4, 
                ifelse(map_data$Percentual < 0.2, 5, 
                ifelse(map_data$Percentual < 0.3, 6, 
                ifelse(map_data$Percentual < 0.5, 7, 
                ifelse(map_data$Percentual <= 1, 8, 
                ifelse(map_data$Percentual > 1 & map_data$Percentual != 100, 9, 10 )))))))))

map_data$cat <- factor(map_data$cat, levels = c(1:10), labels = c("-100% <-> -50%", "-50% <-> -25%", 
    "-25% <-> 0%", "0% <-> 10%", "10% <-> 20%", "20% <-> 30%", "30% <-> 50%", "50% <-> 100%", "100% or More", "Undetermined"))

if(!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

if(!require(maps)) install.packages('maps')
library(maps)

a <- ggplot() + 
geom_polygon(data = map_data, 
             aes(fill = cat,
                 x = long, 
                 y = lat,
                 group = group)
             color = "black", 
             size = 0.1) +
scale_fill_manual(values = c("#800000", "#FF0000", "#FFA07A", "#98FB98", "#11DD7F", "#3CB371", "#2E8B57", "#008000", "#005000", "#FFFFFF"),
                  name = "Milk Production Variation - Source: IBGE, 2019.",
                  drop = FALSE,
                  guide = guide_legend(direction = "horizontal",
                                       keyheight = unit(3, units = "mm"),
                                       keywidth = unit(18, units = "mm"),
                                       title.position = 'top',
                                       title.hjust = 0.5,
                                       label.hjust = 0.5,
                                       nrow = 1,
                                       byrow = T,
                                       reverse = T,
                                       label.position = "bottom")) +
coord_equal() +
theme(legend.position = "bottom", 
      legend.title = element_text(size = 12), 
      legend.text = element_text(size = 6), 
      plot.title = element_text(size = 15)) +
labs(x = NULL, 
     y = NULL, 
     title = "Milk Production Variation Between 2006 and 2017 in Rio Grande do Sul - Brazil ")

milk_properties_2006 <- read.csv('spreadsheet/table1227.csv', skip = 4, encoding = "UTF-8", stringsAsFactors = FALSE, sep=';')
milk_properties_2017 <- read.csv('spreadsheet/table6782.csv', skip = 4, encoding = "UTF-8", stringsAsFactors = FALSE, sep=';')

milk_properties_2006 <- milk_properties_2006[, -2]
milk_properties_2006 <- milk_properties_2006[-(493:503),]
milk_properties_2017 <- milk_properties_2017[, -2]
milk_properties_2017 <- milk_properties_2017[-(497:511),]

for(i in 1:nrow(milk_properties_2006)){
    milk_properties_2006[i,2] <- gsub("X", '0', milk_properties_2006[i,2])
    milk_properties_2006[i,1] <- gsub(" [(]RS[)]", "", milk_properties_2006[i,1])
}

for(i in 1:nrow(milk_properties_2017)){
    milk_properties_2017[i,2] <- gsub("X", '0', milk_properties_2017[i,2])
    milk_properties_2017[i,1] <- gsub(" [(]RS[)]", "", milk_properties_2017[i,1])
}

unique(map_data$Label_N[!map_data$Label_N %in% milk_properties_2006$Município])
unique(map_data$Label_N[!map_data$Label_N %in% milk_properties_2017$Município])

milk_properties_2006[235,1] <- "Maçambara"
milk_properties_2006[337,1] <- "Restinga Seca"
milk_properties_2006[364,1] <- "Santana do Livramento"
milk_properties_2006[478,1] <- "Vespasiano Correa"
milk_properties_2006[491,1] <- "Westfalia"
milk_properties_2017[238,1] <- "Maçambara"
milk_properties_2017[341,1] <- "Restinga Seca"
milk_properties_2017[368,1] <- "Santana do Livramento"
milk_properties_2017[482,1] <- "Vespasiano Correa"
milk_properties_2017[495,1] <- "Westfalia"

colnames(milk_properties_2006) <- c("City", "Properties_2006")
colnames(milk_properties_2017) <- c("City", "Properties_2017")

map_data <- map_data %>% left_join(milk_properties_2006, by = c("Label_N" = "City"))
map_data <- map_data %>% left_join(milk_properties_2017, by = c("Label_N" = "City"))

map_data$Properties_2006 <- as.numeric(map_data$Properties_2006)
map_data$Properties_2017 <- as.numeric(map_data$Properties_2017)

map_data[is.na(map_data$Properties_2006), 23] <- 0
map_data[is.na(map_data$Properties_2017), 24] <- 0

for(i in 1:nrow(map_data)){
    if(map_data$Properties_2006[i] != 0){
        map_data$Percentual_Properties[i] <- (map_data$Properties_2017[i] - map_data$Properties_2006[i]) / map_data$Properties_2006[i]
    }
    else{
        map_data$Percentual_Properties[i] <- 100
    }
}

map_data$catprop <- ifelse(map_data$Percentual_Properties < -0.8, 1, 
                ifelse(map_data$Percentual_Properties < -0.6, 2, 
                ifelse(map_data$Percentual_Properties < -0.4, 3, 
                ifelse(map_data$Percentual_Properties < -0.2, 4, 
                ifelse(map_data$Percentual_Properties < 0, 5, 
                ifelse(map_data$Percentual_Properties < 0.5, 6, 
                ifelse(map_data$Percentual_Properties <= 1, 7, 
                ifelse(map_data$Percentual_Properties > 1 & map_data$Percentual_Properties != 100, 8, 9 ))))))))

map_data$catprop <- factor(map_data$catprop, levels = c(1:9), labels = c("-100% <-> -80%", "-80% <-> -60%", 
    "-60% <-> -40%", "-40% <-> -20%", "-20% <-> 0%", "0% <-> 50%", "50% <-> 100%", "100% or More", "Undetermined"))

b <- ggplot() + 
geom_polygon(data = map_data, 
             aes(fill = catprop,
                 x = long, 
                 y = lat,
                 group = group)
             color = "black", 
             size = 0.1) +
scale_fill_manual(values = c("#600000","#990000", "#CC4444", "#FF967A", "#FFC9CC", "#11EE7F", "#3CB371", "#005000", "#FFFFFF"),
                  name = "Dairy Milk Farms - Source: IBGE, 2019.",
                  drop = FALSE,
                  guide = guide_legend(direction = "horizontal",
                                       keyheight = unit(3, units = "mm"),
                                       keywidth = unit(18, units = "mm"),
                                       title.position = 'top',
                                       title.hjust = 0.5,
                                       label.hjust = 0.5,
                                       nrow = 1,
                                       byrow = T,
                                       reverse = T,
                                       label.position = "bottom")) +
coord_equal() +
theme(legend.position = "bottom", 
      legend.title = element_text(size = 12), 
      legend.text = element_text(size = 6), 
      plot.title = element_text(size = 12)) +
labs(x = NULL, 
     y = NULL, 
     title = "Dairy Milk Farms Variation between 2006 - 2017 in Rio Grande do Sul - Brazil ")

if(!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

ggarrange(a, b, nrow = 2)