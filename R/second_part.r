if(!require(sf)){
    install.packages("sf")
    library(sf)
}

shape_rs <- st_read('shape/Municipios_IBGE.shp')
milk_production_2017_rs <- read.csv2('spreadsheet/table6783_rs.csv', skip=5, stringsAsFactors = FALSE, encoding="UTF-8")
milk_production_2006_rs <- read.csv2('spreadsheet/table933_rs.csv', skip=5, stringsAsFactors = FALSE, encoding="UTF-8")

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
#Different cities names in 2006 dataset: Maçambara, Restinga Seca, Santana do Livramento, Vespasiano Colorrea, Westfalia.
#Cities that do not exist in 2006 dataset: Balneário Pinhal, Capivari do Sul, Cidreira, Esteio.
#Cities that do not exist in shapefile: Pinto Bandeira.
milk_production_2006_rs[235,1] <- "Maçambara"
milk_production_2006_rs[337,1] <- "Restinga Seca"
milk_production_2006_rs[364,1] <- "Santana do Livramento"
milk_production_2006_rs[478,1] <- "Vespasiano Correa"
milk_production_2006_rs[491,1] <- "Westfalia"
shape_rs$Label_N[!shape_rs$Label_N %in% milk_production_2017_rs$Município]
#Different cities names in 2006 dataset: Maçambara, Restinga Seca, Santana do Livramento, Vespasiano Colorrea, Westfalia            
#Cities that do not exist in 2006 dataset: Esteio.
#Cities that do not exist in shapefile: Pinto Bandeira.
milk_production_2017_rs[238,1] <- "Maçambara"
milk_production_2017_rs[341,1] <- "Restinga Seca"
milk_production_2017_rs[368,1] <- "Santana do Livramento"
milk_production_2017_rs[482,1] <- "Vespasiano Correa"
milk_production_2017_rs[495,1] <- "Westfalia"
colnames(milk_production_2006_rs) <- c("City","Milk_2006")
colnames(milk_production_2017_rs) <- c("City","Milk_2017")

shape_rs <- sp::merge(shape_rs, milk_production_2017_rs, by.x="Label_N", by.y="City", all.x=T)
shape_rs <- sp::merge(shape_rs, milk_production_2006_rs, by.x="Label_N", by.y="City", all.x=T)

shape_rs$Milk_2017 <- as.numeric(shape_rs$Milk_2017)
shape_rs$Milk_2006 <- as.numeric(shape_rs$Milk_2006)

shape_rs[is.na(shape_rs$Milk_2006), 13] <- 0
shape_rs[is.na(shape_rs$Milk_2017), 12] <- 0

shape_rs$Diff <- shape_rs$Milk_2017 - shape_rs$Milk_2006
for(i in 1:nrow(shape_rs)){
    if(shape_rs$Milk_2006[i] != 0){
        shape_rs$Percentual[i] <- (shape_rs$Milk_2017[i] - shape_rs$Milk_2006[i]) / shape_rs$Milk_2006[i]
    }
    else{
        shape_rs$Percentual[i] <- 100
    }
}
for(i in 1:nrow(shape_rs)){
  if(shape_rs$Percentual[i] < (-0.5)){
    shape_rs$Color[i] <- "#800000"
    shape_rs$Range[i] <- "-100% <-> -50%"
  }
  else{if(shape_rs$Percentual[i] < (-0.25)){
    shape_rs$Color[i] <- "#FF0000"
    shape_rs$Range[i] <- "-50% <-> -25%"
  }
  else{if(shape_rs$Percentual[i] < 0.0){
    shape_rs$Color[i] <- "#FFA07A"
    shape_rs$Range[i] <- "-25% <-> 0%"
  }
  else{if(shape_rs$Percentual[i] < 0.1){
    shape_rs$Color[i] <- "#98FB98"
    shape_rs$Range[i] <- "0% <-> 10%"
  }
  else{if(shape_rs$Percentual[i] < 0.2){
    shape_rs$Color[i] <- "#00FF7F"
    shape_rs$Range[i] <- "10% <-> 20%"
  }
  else{if(shape_rs$Percentual[i] < 0.3){
    shape_rs$Color[i] <- "#3CB371"
    shape_rs$Range[i] <- "20% <-> 30%"
  }
  else{if(shape_rs$Percentual[i] < 0.5){
    shape_rs$Color[i] <- "#2E8B57"
    shape_rs$Range[i] <- "30% <-> 50%"
  }
  else{if(shape_rs$Percentual[i] < 1.0){
    shape_rs$Color[i] <- "#008000"
    shape_rs$Range[i] <- "50% <-> 100%"
  }
  else{if(shape_rs$Percentual[i] == 100){
    shape_rs$Color[i] <- "#FFFFFF"
    shape_rs$Range[i] <- "Undetermined"
  }
  else{if(shape_rs$Percentual[i] != 100 && shape_rs$Percentual[i] >= 1.0){
    shape_rs$Color[i] <- "#005000"
    shape_rs$Range[i] <- "100% or More"
  }}}}}}}}}}
}
shape_rs <- shape_rs[order(shape_rs$Percentual),]

if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
}

plot( shape_rs$geometry,
      col = shape_rs$Color,
      axes = TRUE,
      bg='light blue',
      main="Milk Production Variation between 2006 and 2017"
    )

legend(-62,-28.5,
      legend = c(rev(unique(shape_rs$Range))),
      fill = c(rev(unique(shape_rs$Color))),
      bg = "gray",
      cex=0.9
      )

text(-60.7,-27.9,
    "Variation Ranges",
    cex=.95
    )

milk_properties_2006 <- read.csv('spreadsheet/table1227.csv', skip = 4, encoding="UTF-8", stringsAsFactors = FALSE, sep=';')
milk_properties_2017 <- read.csv('spreadsheet/table6782.csv', skip = 4, encoding="UTF-8", stringsAsFactors = FALSE, sep=';')

milk_properties_2006 <- milk_properties_2006[,-2]
milk_properties_2006 <- milk_properties_2006[-(493:503),]
milk_properties_2017 <- milk_properties_2017[,-2]
milk_properties_2017 <- milk_properties_2017[-(497:511),]

for(i in 1:nrow(milk_properties_2006)){
    milk_properties_2006[i,2] <- gsub("X", '0', milk_properties_2006[i,2])
    milk_properties_2006[i,1] <- gsub(" [(]RS[)]", "", milk_properties_2006[i,1])
}

for(i in 1:nrow(milk_properties_2017)){
    milk_properties_2017[i,2] <- gsub("X", '0', milk_properties_2017[i,2])
    milk_properties_2017[i,1] <- gsub(" [(]RS[)]", "", milk_properties_2017[i,1])
}

milk_properties_2017[368,1] <- "Santana do Livramento"
milk_properties_2017[496,1] <- "Xangri-Lá"
milk_properties_2006[364,1] <- "Santana do Livramento"
milk_properties_2006[492,1] <- "Xangri-Lá"

colnames(milk_properties_2006) <- c("City", "Properties_2006")
colnames(milk_properties_2017) <- c("City", "Properties_2017")

shape_rs <- sp::merge(shape_rs, milk_properties_2006, by.x="Label_N", by.y="City", all.x=T)
shape_rs <- sp::merge(shape_rs, milk_properties_2017, by.x="Label_N", by.y="City", all.x=T)

shape_rs$Properties_2006 <- as.numeric(shape_rs$Properties_2006)
shape_rs$Properties_2017 <- as.numeric(shape_rs$Properties_2017)

shape_rs[is.na(shape_rs$Properties_2006),18] <- 0
shape_rs[is.na(shape_rs$Properties_2017),19] <- 0

for(i in 1:nrow(shape_rs)){
    if(shape_rs$Properties_2006[i] != 0){
        shape_rs$Percentual_Properties[i] <- (shape_rs$Properties_2017[i] - shape_rs$Properties_2006[i]) / shape_rs$Properties_2006[i]
    }
    else{
        shape_rs$Percentual_Properties[i] <- 100
    }
}

for(i in 1:nrow(shape_rs)){
    if(shape_rs$Percentual_Properties[i] < (-0.8)){
        shape_rs$Color_Properties[i] <- "#600000"
        shape_rs$Range_Properties[i] <- "-80% <-> -100%"
    }
    else{if(shape_rs$Percentual_Properties[i] < (-0.6)){
        shape_rs$Color_Properties[i] <- "#990000"
        shape_rs$Range_Properties[i] <- "-60% <-> -80%"
    }
    else{if(shape_rs$Percentual_Properties[i] < (-0.4)){
        shape_rs$Color_Properties[i] <- "#CC4444"
        shape_rs$Range_Properties[i] <- "-40% <-> -60%"
    }
    else{if(shape_rs$Percentual_Properties[i] < (-0.2)){
        shape_rs$Color_Properties[i] <- "#FF967A"
        shape_rs$Range_Properties[i] <- "-20% <-> -40%"
    }
    else{if(shape_rs$Percentual_Properties[i] < 0.0){
        shape_rs$Color_Properties[i] <- "#FFC9CC"
        shape_rs$Range_Properties[i] <- "-20% <-> 0%"
    }
    else{if(shape_rs$Percentual_Properties[i]<0.5){
        shape_rs$Color_Properties[i]<-"#98FB98"
        shape_rs$Range_Properties[i]<-"0% <-> 50%"
    }
    else{if(shape_rs$Percentual_Properties[i] < 1.0){
        shape_rs$Color_Properties[i] <- "#2E8B57"
        shape_rs$Range_Properties[i] <- "50% <-> 100%"
    }
    else{if(shape_rs$Percentual_Properties[i] == 100){
        shape_rs$Color_Properties[i] <- "#FFFFFF"
        shape_rs$Range_Properties[i] <- "Undetermined"
    }
    else{
        shape_rs$Color_Properties[i] <- "#005000"
        shape_rs$Range_Properties[i] <- "100% or more"
    }
}}}}}}}}

shape_rs <- shape_rs[order(shape_rs$Percentual_Properties),]

plot( shape_rs["Percentual_Properties"],
      col = shape_rs$Color_Properties,
      axes = TRUE,
      bg = 'light blue',
      main = "Milk Production Properties Variation between 2006 and 2017"
    )

legend( x = "topright",
        legend = c(rev(unique(shape_rs$Range_Properties))),
        fill = c(rev(unique(shape_rs$Color_Properties))),
        bg = "gray",
        cex = 0.7
      )

text(-42,-26.6,
    "Variation Ranges",
    cex=.95
    )
