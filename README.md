# Summary

This project is related to some researches about the Brazilian dairy milk production. This project was developed during my graduation in [Applied Computing](http://cursos.unipampa.edu.br/cursos/ppgcap/) in [Federal University of Pampa](http://novoportal.unipampa.edu.br/novoportal/), in partnership with [Brazilian Agricultural Research Colorporation](https://www.embrapa.br/en/international), also known as Embrapa.
The content of this project can be found in this [Expanded Resume](http://publicase.unipampa.edu.br/index.php/siepe/article/view/40708). It is in portuguese, but in time, I can provide an english version of it. The purpose is to realize an exploratory analysis of dairy milk production data to generate meaningful visualizations, with focus on Brazil and Rio Grande do Sul state.

# First Part

The first step is to get the data about the dairy milk production to perform the analysis. 
In Brazil, we can get this specific data here (https://sidra.ibge.gov.br/tabela/74). Here, we select just "Milk production", all years from the historic and the region units "Brazil" and "Rio Grande do Sul". The reason is that we will perform an exploratory analysis with all historic data from milk production in Rio Grande do Sul state and Brazil. You can download the data and specify the file format. In this case, we will choose the .csv (BR). Save this file in the same folder where you will create your R project.
From here we have two alternatives: you can manually open the csv file and remove some useless metadata or you can use the own R language to clean this data. For automation purpose, we will follow the second option.
Open your RStudio or other IDE with R language. Set you directory as the working directory with the RStudio or you can do this using:
```R
setwd("working_directory")
```
To sucessfully read the csv file, we need to use the **skip** and **nrows** arguments of **read.csv2**. Also, the first row and colunm have not valuable information, so we can remove it from the dataframe. 
```R
milk_production <- read.csv2('spreadsheet/table74.csv',skip=3, nrows=3)
milk_production <- milk_production[-1,-1]
```
We need to clean the years, because it is a String, not Integer, and it is in format "X....". We also reshape our dataframe to use it in **plotly**, setting the rows as colunms and we will have only one colunm named "years". We need to use **as.numeric** and **as.character** to convert the data from **factor** to **numeric**.
```R
colnames(milk_production) <- gsub("X",'',colnames(milk_production))
years <- as.integer(colnames(milk_production))
brazilian_milk <- as.numeric(as.character(unlist(milk_production[1,], use.names=FALSE)))
rs_milk <- as.numeric(as.character(unlist(milk_production[2,], use.names=FALSE)))
data <- data.frame(brazilian_milk, rs_milk, years)
```
Now we create two linear models (linear regression), one for Brazil and one for the RS State to use them in **plotly**.
```R
l<-lm(data$brazilian_milk/1000000~years,data=data)
l2<-lm(data$rs_milk/1000000~years,data=data)
```
Now we use the **plotly** function. First, we need to install and import the library and its dependences.
```R
if(!require(plotly)){
    install.packages("plotly")
    library(plotly)
}
font1 <- list(family = "Arial, sans-serif", size = 22, color = "black")
font2 <- list(size = 16, color = "black")
labelx <- list(title = "Years", titlefont = font1, showticklabels = TRUE, tickfont = font2, exponentformat = "E") 
labely <- list(title = "Milk Production (Billions of liters)", titlefont = font1, showticklabels = TRUE, tickfont = font2, exponentformat = "E")
plot_ly(data,x=~years) %>% 
add_trace(y = ~brazilian_milk/1000000,type = "bar", name = 'Brazil',marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(0,0,0)', width = 1.5))) %>% 
add_trace(y = ~rs_milk/1000000, type = "bar", name = 'RS', marker = list(color = 'rgb(225,58,58)',line = list(color = 'rgb(0,0,0)', width = 1.5))) %>% 
add_lines(y = fitted(l), name = 'Brazil', line = list(color = 'rgb(0,0,255)', width = 3)) %>% 
add_lines(y = fitted(l2), name = 'RS state', line = list(color = 'rgb(255,0,0)', width = 3)) %>% 
layout(xaxis = labelx, yaxis = labely)
```
![Alt text](figures/figure1.png?raw=true "Title")

We can get the milk production data of all states from Brazil and create an animated bar chart race. First we load the needed packages and load the [Data](https://sidra.ibge.gov.br/tabela/74).
```R
if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}
if(!require(janitor)){
    install.packages("janitor")
    library(janitor)
}
milk_production_states <- read.csv2('spreadsheet/table74_brazil.csv', skip=3, nrows=31, stringsAsFactors = FALSE, encoding="UTF-8")
```
If we check the data, we will see a need for cleaning. That is what we do next.
```R
milk_production_states <- milk_production_states[-(1:2),]
milk_production_states <- milk_production_states[-(29:32),]
colnames(milk_production_states) <- gsub("X",'',colnames(milk_production_states))
colnames(milk_production_states)[1] <- "Brazilian_States"
for(i in 2:ncol(milk_production_states)){
        milk_production_states[,i]<-gsub("[...]",'0',milk_production_states[,i])
        milk_production_states[,i]<-as.numeric(as.character(unlist(milk_production_states[,i])))
}
```
The next part is the data processing to create the animated bar plots. The method is based on this [Tutorial](https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da), from [Towards Data Science](https://towardsdatascience.com/). Go there for more information about it.
```R
milk_production_states <- milk_production_states %>% mutate_at(vars(colnames(milk_production_states)[2:45]),as.numeric) %>% gather(year,value,2:45) 
milk_production_states$year <- as.numeric(milk_production_states$year)

milk_production_states_1 <- milk_production_states %>%
    group_by(year) %>%
    mutate(rank = rank(-value),
           Value_rel = value/value[rank==1],
           Value_lbl = paste0(" ",round((value*1000)/1000000000,2)),
           show_time = case_when(year %in% c(2017) ~ 10,TRUE ~ 1),
           reveal_time = cumsum(show_time)) %>%
    group_by(Brazilian_States) %>% 
    filter(rank <= 10) %>%
    ungroup()

if(!require(gganimate)){
    install.packages("gganimate")
    library(gganimate)
}

staticplot = ggplot(milk_production_states_1, aes(rank, group = Brazilian_States, 
    fill = as.factor(Brazilian_States), color = as.factor(Brazilian_States))) +
    geom_tile(aes(y = value/2,height = value, width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(Brazilian_States, " "), vjust = 0.2, hjust = 1, size = 6)) +
    geom_text(aes(y = value, label = Value_lbl, hjust = 0, size = 6)) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line( size=.1, color="grey" ),
          panel.grid.minor.x = element_line( size=.1, color="grey" ),
          plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black", vjust=-1),
          plot.subtitle=element_text(size=22, hjust=0.5, face="italic", color="black"),
          plot.caption =element_text(size=15, hjust=0.5, face="italic", color="black"),
          plot.background=element_blank(),
          plot.margin = margin(2, 2, 8, 8, "cm"))

anim = staticplot + transition_time(year)+
    view_follow(fixed_x = TRUE)  +
  labs(title = 'Milk Production in Brazilian States (Billions of Liters): {round(frame_time)}',  
  subtitle  =  "Top 10 States",
  caption  = "Milk Production in Brazilian States in Billions of Liters | Fonte dos dados: Instituto Brasileiro de Estatística e Geografia.")

animate(anim, 880, fps = 44, width = 1600, height = 1200, renderer = gifski_renderer("gganim.gif", loop = FALSE)) +  ease_aes('cubic-in-out') 
```
![Alt text](figures/gif1.gif?raw=true "Title")

Now, the second part of our exploratory analysis, we will get the number of dairy farms from Brazil in [2006](https://sidra.ibge.gov.br/tabela/6783) and [2017](https://sidra.ibge.gov.br/tabela/933).
And them, we perform the same procedure that we did before with the milk production data. Note that **skip** and **nrows** arguments changed. Check the .csv file to notice the Diffs and what rows and colunms need to be removed.
```R
properties_2006 <- read.csv2('spreadsheet/table933.csv',skip=5, nrows=1)
properties_2017 <- read.csv2('spreadsheet/table6783.csv',skip=5, nrows=1)
properties_2006 <- properties_2006[,-(1:2)] #metadata removing
properties_2017 <- properties_2017[,-1]     #metadata removing
properties_2006 <- as.numeric(as.character(unlist(properties_2006[1,], use.names=FALSE)))
properties_2017 <- as.numeric(as.character(unlist(properties_2017[1,], use.names=FALSE)))
```
Now, with the **NROW** command, we noticed that we have not the same number of samples in 2006 and 2017. We saw that properties from 2017 had one range more than 2006 (the 2500-10000 hectares). So we add the two last samples and merged into one.
```R
NROW(properties_2006) == NROW(properties_2017)
properties_2017[18] <- properties_2017[18] + properties_2017[19] 
properties_2017 <- properties_2017[-19] 
```
Now we manually create two lists, one with the ranges of property area (in hectares) and the other is just a sequence of integers to sort the ranges properly in the graphics.
```R
properties_range <- c("0-0,1","0,1-0,2","0,2-0,5","0,5-1","1-2","2-3","3-4","4-5","5-10","10-20","20-50","50-100","100-200","200-500","500-1000","1000-2500","2500+","NA")
sequence <- seq(1,18)
```
Finally, we can draw two horizontal barplots showing the absolute and percentual variation values of dairy milk properties in Brazil.
```R
plot_ly(x = (properties_2017-properties_2006)/1000, y = reorder(properties_range,sequence),type = 'bar', orientation = 'h',marker = list(color = 'rgba(50, 171, 96, 0.6)',line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85), title="Range of property size (hectares)"), xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Variation in Number of Dairy Farms (thousands)"), font=list(size=8)) %>% 
add_annotations(xref = 'x1', yref = 'y',x = (properties_2017-properties_2006)/1000,  y = properties_range, text = paste(round((properties_2017-properties_2006)/1000, 0), 'k'), font = list(family = 'Arial', size = 8, color = 'rgb(0, 0, 0)'),showarrow = FALSE)
```
![Alt text](figures/figure2.png?raw=true "Title")
```R
plot_ly(x = ((properties_2017-properties_2006)/properties_2006)*100, y = reorder(properties_range,sequence),type = 'bar', orientation = 'h',marker = list(color = 'rgba(128, 0, 128, 0.6)',line = list(color = 'rgba(102, 102, 102, 1.0)', width = 1))) %>% 
layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85), title="Range of property size (hectares)"), xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE, title="Percentual Variation of Dairy Farms (%)"),font=list(size=8)) %>% 
add_annotations(xref = 'x1', yref = 'y',x = ((properties_2017-properties_2006)/properties_2006)*100,  y = properties_range, text = paste(round(((properties_2017-properties_2006)/properties_2006)*100, 2), '%'),font = list(family = 'Arial', size = 8, color = 'rgb(0, 0, 0)'),showarrow = FALSE)
```
![Alt text](figures/figure3.png?raw=true "Title")

We can improve the analysis by making just one plot. This last example we reduced the number of ranges.
```R
properties_range_2 <- c("0-10","10-20","20-50","50-100","100-200","200-1000","1000+","N.I")
properties_2006_2 <- c(sum(properties_2006[1:9]),properties_2006[10],properties_2006[11],properties_2006[12],properties_2006[13],sum(properties_2006[14:15]),sum(properties_2006[16:17]),properties_2006[18])
properties_2017_2 <- c(sum(properties_2017[1:9]),properties_2017[10],properties_2017[11],properties_2017[12],properties_2017[13],sum(properties_2017[14:15]),sum(properties_2017[16:17]),properties_2017[18])
sequence_2 <- c(1,2,3,4,5,6,7,8)

b<-plot_ly(y = (properties_2017_2-properties_2006_2)/1000, x = reorder(properties_range_2,sequence_2),type = 'bar', orientation = 'v',marker = list(color = 'rgba(171, 51, 96, 0.6)',line = list(color = 'rgba(0, 0, 0, 1.0)', width = 1))) %>% 
layout(annotations=list(x=reorder(properties_range_2,sequence_2),y=(properties_2017_2-properties_2006_2)/1000,text = paste(round((properties_2017_2-properties_2006_2)/1000, 0), 'k'),font = list(family = 'Arial', size = 15, color = 'rgb(0, 0, 0)'),showarrow = TRUE,xref = "x", yref = "y",ax=10,ay=20),showlegend=FALSE, yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title="Reduction of Dairy Farms by Range (thousands) (2006->2017)"), xaxis = list(tickfont = list(size=12,color = "black"),zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Range of property size (hectares). *N.I (Not Informed)"),font=list(size=11))
c<-plot_ly(y = ((properties_2017_2-properties_2006_2)/properties_2006_2)*100, x = reorder(properties_range_2,sequence_2),type = 'bar', orientation = 'v',marker = list(color = 'rgba(250, 0, 50, 0.6)',line = list(color = 'rgba(0, 0, 0, 1.0)', width = 1))) %>% 
layout(showlegend=FALSE,annotations=list(x=reorder(properties_range_2,sequence_2),y=((properties_2017_2-properties_2006_2)/properties_2006_2)*100,text = paste(round(((properties_2017_2-properties_2006_2)/properties_2006_2)*100, 0), '%'),font = list(family = 'Arial', size = 15, color = 'rgb(0, 0, 0)'),showarrow = TRUE,xref = "x2", yref = "y2",ax=10,ay=20),yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title="Percentual Reduction of Dairy Farms by Range (%)(2006->2017)"), xaxis = list(tickfont = list(size=12,color = "black"),zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Range of property size (hectares). *N.I (Not Informed)"),font=list(size=11))
subplot(b,c,titleX = TRUE, titleY = TRUE,margin = c(0.035,0,0,0.8)) %>% layout(title = "Brazilian Dairy Properties Variation between 2006 and 2017")
```
![Alt text](figures/figure4.png?raw=true "Title")

## Second Part
Here we will analyse how the milk production and the number of milk farms behaved in 2006-2017 period in all RS state from Brazil. First, we get the milk production data from [2006](https://sidra.ibge.gov.br/tabela/933) and [2017](https://sidra.ibge.gov.br/tabela/6783) by cities. Be sure to select data by cities of RS state only. We also need the shapefile with the cities of RS to plot maps. The shapefile of RS state can be found [Here](http://www.fepam.rs.gov.br/biblioteca/geo/bases_geo.asp).
```R
if(!require(sf)){
    install.packages("sf")
    library(sf)
}
shape_rs <- st_read('shape/Municipios_IBGE.shp')
milk_production_2017_rs <- read.csv2('spreadsheet/table6783_rs.csv', skip=5, stringsAsFactors = FALSE, encoding="UTF-8")
milk_production_2006_rs <- read.csv2('spreadsheet/table933_rs.csv', skip=5, stringsAsFactors = FALSE, encoding="UTF-8")
```
After checking the data, we need to perform some cleaning to deal with missing values and strings.
```R
milk_production_2006_rs<-milk_production_2006_rs[,-2]
milk_production_2006_rs<-milk_production_2006_rs[-(493:503),]
milk_production_2017_rs<-milk_production_2017_rs[-(497:511),]
for(i in 1:nrow(milk_production_2017_rs)){
    milk_production_2017_rs[i,2]<-gsub("X",'0',milk_production_2017_rs[i,2])
    milk_production_2017_rs[i,1]<-gsub(" [(]RS[)]","",milk_production_2017_rs[i,1])
}
for(i in 1:nrow(milk_production_2006_rs)){
    milk_production_2006_rs[i,2]<-gsub("X",'0',milk_production_2006_rs[i,2])
    milk_production_2006_rs[i,1]<-gsub(" [(]RS[)]","",milk_production_2006_rs[i,1])
}
```
With the command **which** and **%in%** below, we check the cities that are not present in the samples of 2006 and 2017 or have Diffs comparing with the shapefile. We deal with this in the sequence.
```R
shape_rs$Label_N[!shape_rs$Label_N %in% milk_production_2006_rs$Município]
#Different cities names in 2006 dataset: Maçambara, Restinga Seca, Santana do Livramento, Vespasiano Colorrea, Westfalia.
#Cities that do not exist in 2006 dataset: Balneário Pinhal, Capivari do Sul, Cidreira, Esteio.
#Cities that do not exist in shapefile: Pinto Bandeira.
milk_production_2006_rs[235,1]<-"Maçambara"
milk_production_2006_rs[337,1]<-"Restinga Seca"
milk_production_2006_rs[364,1]<-"Santana do Livramento"
milk_production_2006_rs[478,1]<-"Vespasiano Correa"
milk_production_2006_rs[491,1]<-"Westfalia"
shape_rs$Label_N[!shape_rs$Label_N %in% milk_production_2017_rs$Município]
#Different cities names in 2006 dataset: Maçambara, Restinga Seca, Santana do Livramento, Vespasiano Colorrea, Westfalia            
#Cities that do not exist in 2006 dataset: Esteio.
#Cities that do not exist in shapefile: Pinto Bandeira.
milk_production_2017_rs[238,1]<-"Maçambara"
milk_production_2017_rs[341,1]<-"Restinga Seca"
milk_production_2017_rs[368,1]<-"Santana do Livramento"
milk_production_2017_rs[482,1]<-"Vespasiano Correa"
milk_production_2017_rs[495,1]<-"Westfalia"
colnames(milk_production_2006_rs)<-c("City","Milk_2006")
colnames(milk_production_2017_rs)<-c("City","Milk_2017")
```
Now we merge the milk production datasets with the shapefile using the city names as the keys.
```R
shape_rs<-sp::merge(shape_rs, milk_production_2017_rs,by.x="Label_N",by.y="City", all.x=T)
shape_rs<-sp::merge(shape_rs, milk_production_2006_rs,by.x="Label_N",by.y="City", all.x=T)
```
Now we check and correct the rows with missing values for milk production. We also convert this values to numeric.
```R
shape_rs[is.na(shape_rs$Milk_2006),]
shape_rs[is.na(shape_rs$Milk_2017),]
shape_rs$Milk_2017[157]=0 #Esteio
shape_rs$Milk_2006[31]=0 #Balneário Pinhal
shape_rs$Milk_2006[88]=0 #Capivari do Sul
shape_rs$Milk_2006[109]=0 #Cidreira
shape_rs$Milk_2006[157]=0 #Esteio
shape_rs$Milk_2006[318]<-0 #Pinto Bandeira
shape_rs$Milk_2017<-as.numeric(shape_rs$Milk_2017)
shape_rs$Milk_2006<-as.numeric(shape_rs$Milk_2006)
```
Now we manually generate the percentual variation of milk production to each city. We also manually set a color and a string name to each range of percentual. Note that we could do much more here, analysing the distributions and use statistics to better choose the ranges. 
```R
shape_rs$Diff<-shape_rs$Milk_2017-shape_rs$Milk_2006
for(i in 1:496){
    if(shape_rs$Milk_2006[i]!=0){
        shape_rs$Percentual[i]<-(shape_rs$Milk_2017[i]-shape_rs$Milk_2006[i])/shape_rs$Milk_2006[i]
    }
    else{
        shape_rs$Percentual[i]<-100
    }
}
for(i in 1:496){
  if(shape_rs$Percentual[i]<(-0.5)){
    shape_rs$Color[i]<-"#800000"
    shape_rs$Range[i]<-"-100% <-> -50%"
  }
  else{if(shape_rs$Percentual[i]<(-0.25)){
    shape_rs$Color[i]<-"#FF0000"
    shape_rs$Range[i]<-"-50% <-> -25%"
  }
  else{if(shape_rs$Percentual[i]<0.0){
    shape_rs$Color[i]<-"#FFA07A"
    shape_rs$Range[i]<-"-25% <-> 0%"
  }
  else{if(shape_rs$Percentual[i]<0.1){
    shape_rs$Color[i]<-"#98FB98"
    shape_rs$Range[i]<-"0% <-> 10%"
  }
  else{if(shape_rs$Percentual[i]<0.2){
    shape_rs$Color[i]<-"#00FF7F"
    shape_rs$Range[i]<-"10% <-> 20%"
  }
  else{if(shape_rs$Percentual[i]<0.3){
    shape_rs$Color[i]<-"#3CB371"
    shape_rs$Range[i]<-"20% <-> 30%"
  }
  else{if(shape_rs$Percentual[i]<0.5){
    shape_rs$Color[i]<-"#2E8B57"
    shape_rs$Range[i]<-"30% <-> 50%"
  }
  else{if(shape_rs$Percentual[i]<1.0){
    shape_rs$Color[i]<-"#008000"
    shape_rs$Range[i]<-"50% <-> 100%"
  }
  else{if(shape_rs$Percentual[i]==100){
    shape_rs$Color[i]<-"#FFFFFF"
    shape_rs$Range[i]<-"Undetermined"
  }
  else{if(shape_rs$Percentual[i]!=100 && shape_rs$Percentual[i]>=1.0){
    shape_rs$Color[i]<-"#005000"
    shape_rs$Range[i]<-"100% or More"
  }}}}}}}}}}
}
shape_rs<-shape_rs[order(shape_rs$Percentual),]
```
Finally, the map. We manually design the position of legends and title. You can change the values to fit your needs.
```R
if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
}
plot(shape_rs$geometry,col = shape_rs$Color,axes = TRUE,bg='light blue',main="Milk Production Variation between 2006 and 2017")
legend(-62,-28.5,legend=c(rev(unique(shape_rs$Range))),fill =c(rev(unique(shape_rs$Color))),bg = "gray",cex=0.9)
text(-60.7,-27.9,"Variation Ranges",cex=.95)
```
![Alt text](figures/figure5.png.jpeg?raw=true "Title")

## Third Part
Now, we will create an animated map, showing how the milk production behaved in all cities from Rio Grande do Sul state, from Brazil. First, we download the needed [Data](https://sidra.ibge.gov.br/tabela/74). You need to check the data yourself before the cleaning stage to see what is wrong with the spreadsheet. We import and clean the data first.
```R
milk_production_rs_cities <- read.csv2('spreadsheet/table74_rs_cities.csv', skip=3, stringsAsFactors = FALSE, encoding="UTF-8")
milk_production_rs_cities<-milk_production_rs_cities[-(1:2),]
milk_production_rs_cities<-milk_production_rs_cities[-(498:510),]
colnames(milk_production_rs_cities) <- gsub("X",'',colnames(milk_production_rs_cities))
colnames(milk_production_rs_cities)[1] <- "Cities"
for(i in 2:ncol(milk_production_rs_cities)){
        milk_production_rs_cities[,i]<-gsub("[...]","0",milk_production_rs_cities[,i])
        milk_production_rs_cities[,i]<-gsub("[-]","0",milk_production_rs_cities[,i])
        milk_production_rs_cities[,i]<-as.numeric(as.character(unlist(milk_production_rs_cities[,i])))
}
milk_production_rs_cities[,1]<-gsub(" [(]RS[)]","",milk_production_rs_cities[,1])
```
We load some needed packages to plot animated maps and load the shapefile from RS cities.
```R
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(RColorBrewer)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(RColorBrewer)
}
if (!require(dplyr)) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
  require(dplyr)
}
shape_rs <- readOGR("shape/Municipios_IBGE.shp", "Municipios_IBGE",use_iconv=TRUE, encoding="UTF-8")
```
Now we check the cities that are not present in the IBGE dataset or if they have differences in comparison to the shapefile. We deal with this in the sequence.
```R
shape_rs@data$Label_N[!shape_rs@data$Label_N %in% milk_production_rs_cities$Cities]
milk_production_rs_cities[239,1]<-"Maçambara"
milk_production_rs_cities[342,1]<-"Restinga Seca"
milk_production_rs_cities[369,1]<-"Santana do Livramento"
milk_production_rs_cities[483,1]<-"Vespasiano Correa"
milk_production_rs_cities[496,1]<-"Westfalia"
```
Now we will change the milk production data to millions of liters and change the dataset shape to a longer format.
```R
milk_production_rs_cities<-milk_production_rs_cities[order(milk_production_rs_cities$Cities),] 
for(j in 2:ncol(milk_production_rs_cities)){
     milk_production_rs_cities[,j]<-as.numeric(milk_production_rs_cities[,j])/1000
}
milk_production_rs_cities$id <- c(1:nrow(milk_production_rs_cities))
milk_production_rs_cities <- reshape::melt(milk_production_rs_cities, id.vars=c("Cities","id"))
```
The IBGE dataset is ready to merge. Now we change the shapefile to dataframe type to use it in plot functions. And them we merge it with the dataset.
```R
shape_rs@data$id <- c(1:nrow(shape_rs@data))
shapefile_df <- fortify(shape_rs, region = 'id') %>% mutate(id = as.numeric(id))
shapefile_RS<-sp::merge(shapefile_df, shape_rs@data,by="id")
map_data <- shapefile_RS %>% left_join(milk_production_rs_cities, by = c("Label_N" = "Cities"))
colnames(map_data)[20]<-"year"
map_data<-map_data[,-(14:17)]
map_data<-map_data[,-(8:11)]
```
Now we check and correct if exists rows with missing values for milk production and turn the year colunm numeric.
```R
map_data[is.na(map_data$values),]
map_data$year<-as.numeric(as.character(map_data$year))
```
Now we use the concept of *quantile* of statistics to discover the best partitions in the data to generate the ranges of intervals.
```R
quantile( milk_production_rs_cities$value[milk_production_rs_cities$variable==2017], p = (0:5)/5 )
```
We get the numbers of previous quantile function to generate the data ranges to insert in the legend.
```R
map_data$cat <- ifelse(map_data$value >= 14.063, 8, ifelse(map_data$value >= 8, 
    7, ifelse(map_data$value >= 4.212, 6, ifelse(map_data$value >= 1.119, 5, ifelse(map_data$value >= 
       0, 4, 4)))))
map_data$cat <- factor(map_data$cat, levels = c(8:4), labels = c("14.60 - 62.91", "8.01 - 14.60", 
    "4.21 - 8.00", "1.12 - 4.21", "0.00 - 1.12"))
```
Finally, we use ggplot and animate to generate the maps and the final animation. There are a lot of tutorials showing how to make a animated map. You can check [here](https://dontfeedthefears.com/how-to-create-an-animated-map-in-r/#comment-15) how to create one.
```R
p <- ggplot() +
    geom_polygon(data = map_data, aes(fill = cat,x = long, y = lat, group = group)) +
    geom_path(data = map_data, aes(x = long, y = lat, group = group), color = "black", size = 0.1) +
    coord_equal() +
    theme(legend.position = "bottom", legend.title = element_text(size = 20), legend.text = element_text(size = 18), plot.title = element_text(size=24)) +
    labs(x = NULL, y = NULL, 
         title = "Milk Production in {round(frame_time,0)} - Rio Grande do Sul - Brazil "
    ) 
p <- p + scale_fill_manual(
    values = rev(colorRampPalette(brewer.pal(5, "Greens"))(5)),
    name = "Milk Production (Millions of Liters) - Source: IBGE, 2019.",
    drop = FALSE,
    guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(4, units = "mm"),keywidth = unit(40, units = "mm"),
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 0.5,
        nrow = 1,
        byrow = T,reverse = T,
        label.position = "bottom"
    )
)
p <- p + transition_time(year)
animate(p, nframes=220, fps = 10, width = 1400, height = 900, renderer = gifski_renderer("gganim.gif", loop = FALSE)) +  ease_aes('cubic-in-out') 
```

![Alt text](figures/gif2.gif?raw=true "Title")

And that's all. Later I will improve this analysis using more variables from IBGE to better know and show the reality of Milk Production.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
