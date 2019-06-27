# Summary

This project is related to some researches about the Brazilian dairy milk production. This project was developed during my graduation in Applied Computing in Federal University of Pampa, in partnership with Brazilian Agricultural Research Corporation (known as Embrapa).
The content of this project can be found in this expanded resume -> (http://publicase.unipampa.edu.br/index.php/siepe/article/view/40708). It is in portuguese, but in time, I can provide an english version of it.

# How-to

The first step is to get the data about the dairy milk production to perform the analysis. 

In Brazil, we can get this specific data here (https://sidra.ibge.gov.br/tabela/74). Here, we select just "Milk production", all years from the historic and the region units "Brazil" and "Rio Grande do Sul". The reason is that we will perform an exploratory analysis with all historic data from milk production in Rio Grande do Sul state and Brazil. You can download the data and specify the file format. In this case, we will choose the .csv (BR). Save this file in the same folder where you will create your R project.

From here we have two alternatives: you can manually open the csv file and remove some useless metadata or you can use the own R language to clean this data. For automation purpose, we will follow the second option.

Opem your RStudio or other IDE with R language. Set you directory as the working directory with the RStudio or you can do this using:

```R
setwd("working_directory")
```
To sucessfully read the csv file, we need to use the #### skip and #### nrows arguments of ####read.csv2. Also, one of the rows will not have valuable information, so we can remove it from the dataframe. 

```R
milk_production <- read.csv2('Planilhas/table74.csv',skip=3, nrows=3)
milk_production <- milk_production[-1,-1]
```
We need to clean the years, because they are Strings, not Integers, and they have some dirty characters to be removed. In the end, we reshape the dataframe to use it in plotly. In other words, we set the rows as colunms and we will have only one colunm called "years". We need to use as.numeric and as.character to convert the data from factor to numeric.
```R
colnames(milk_production) <- gsub("X",'',colnames(milk_production))
years <- as.integer(colnames(milk_production))
brazilian_milk <- as.numeric(as.character(unlist(milk_production[1,], use.names=FALSE)))
rs_milk <- as.numeric(as.character(unlist(milk_production[2,], use.names=FALSE)))
data <- data.frame(brazilian_milk, rs_milk, years)
```
Now we will create two linear models (linear regression), one for brazil and one for the RS state to use them in plotly function. Here we use just the years as a independent variable (or predictor).
```R
l<-lm(data$brazilian_milk/1000000~years,data=data)
l2<-lm(data$rs_milk/1000000~years,data=data)
```
To plot the graphics, first we need to install and import the library and its dependences.
```R
install.packages("plotly")
library(plotly)
```
And them, we plot a barplot with two regression lines from brazilian and RS milk production.
```R
font1 <- list(family = "Arial, sans-serif", size = 22, color = "black")
font2 <- list(size = 16, color = "black")
a <- list(title = "Years", titlefont = font1, showticklabels = TRUE, tickfont = font2, exponentformat = "E") 
b <- list(title = "Milk Production (Billions of liters)", titlefont = font1, showticklabels = TRUE, tickfont = font2, exponentformat = "E")
plot_ly(data,x=~years) %>% 
add_trace(y = ~brazilian_milk/1000000,type="bar", name = 'Brazil',marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(0,0,0)', width = 1.5))) %>% 
add_trace(y = ~rs_milk/1000000, type="bar",name = 'RS', marker = list(color = 'rgb(225,58,58)',line = list(color = 'rgb(0,0,0)', width = 1.5))) %>% 
add_lines(y = fitted(l), name='Brazil', line=list(color='rgb(0,0,255)', width = 3)) %>% 
add_lines(y=fitted(l2), name='RS state', line=list(color='rgb(255,0,0)', width = 3)) %>% 
layout(xaxis = a, yaxis = b)
```
![Alt text](figures/figure1.png?raw=true "Title")

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
