# Summary

This project is related to some researches about the Brazilian dairy milk production. This project was developed during my graduation in [Applied Computing](http://cursos.unipampa.edu.br/cursos/ppgcap/) in [Federal University of Pampa](http://novoportal.unipampa.edu.br/novoportal/), in partnership with [Brazilian Agricultural Research Corporation](https://www.embrapa.br/en/international), also known as Embrapa.
The content of this project can be found in this [Expanded Resume](http://publicase.unipampa.edu.br/index.php/siepe/article/view/40708). It is in portuguese, but in time, I can provide an english version of it.

# How-to

The first step is to get the data about the dairy milk production to perform the analysis. 
In Brazil, we can get this specific data here (https://sidra.ibge.gov.br/tabela/74). Here, we select just "Milk production", all years from the historic and the region units "Brazil" and "Rio Grande do Sul". The reason is that we will perform an exploratory analysis with all historic data from milk production in Rio Grande do Sul state and Brazil. You can download the data and specify the file format. In this case, we will choose the .csv (BR). Save this file in the same folder where you will create your R project.
From here we have two alternatives: you can manually open the csv file and remove some useless metadata or you can use the own R language to clean this data. For automation purpose, we will follow the second option.
Open your RStudio or other IDE with R language. Set you directory as the working directory with the RStudio or you can do this using:
```R
setwd("working_directory")
```
To sucessfully read the csv file, we need to use the **skip** and **nrows** arguments of **read.csv2**. Also, the first row and colunm have not valuable information, so we can remove it from the dataframe. 
```R
milk_production <- read.csv2('Planilhas/table74.csv',skip=3, nrows=3)
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
install.packages("plotly")
library(plotly)
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
Now, the second part of our exploratory analysis, we will get the number of dairy farms from Brazil in [2006](https://sidra.ibge.gov.br/tabela/6783) and [2017](https://sidra.ibge.gov.br/tabela/933).
And them, we perform the same procedure that we did before with the milk production data. Note that **skip** and **nrows** arguments changed. Check the .csv file to notice the differences and what rows and colunms need to be removed.
```R
properties_2006 <- read.csv2('Planilhas/table933.csv',skip=5, nrows=1)
properties_2017 <- read.csv2('Planilhas/table6783.csv',skip=5, nrows=1)
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
## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
