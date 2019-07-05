milk_production <- read.csv2('spreadsheet/table74.csv',skip=3, nrows=3)
milk_production <- milk_production[-1,-1]

colnames(milk_production) <- gsub("X",'',colnames(milk_production))
years <- as.integer(colnames(milk_production))
brazilian_milk <- as.numeric(as.character(unlist(milk_production[1,], use.names=FALSE)))
rs_milk <- as.numeric(as.character(unlist(milk_production[2,], use.names=FALSE)))
data <- data.frame(brazilian_milk, rs_milk, years)

l<-lm(data$brazilian_milk/1000000~years,data=data)
l2<-lm(data$rs_milk/1000000~years,data=data)

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

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}
if(!require(janitor)){
    install.packages("janitor")
    library(janitor)
}
milk_production_states <- read.csv2('spreadsheet/table74_brazil.csv', skip=3, nrows=31, stringsAsFactors = FALSE, encoding="UTF-8")
milk_production_states <- milk_production_states[-(1:2),]
milk_production_states <- milk_production_states[-(29:32),]
colnames(milk_production_states) <- gsub("X",'',colnames(milk_production_states))
colnames(milk_production_states)[1] <- "Brazilian_States"
for(i in 2:ncol(milk_production_states)){
        milk_production_states[,i]<-gsub("[...]",'0',milk_production_states[,i])
        milk_production_states[,i]<-as.numeric(as.character(unlist(milk_production_states[,i])))
}

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

staticplot <- ggplot(milk_production_states_1, aes(rank, group = Brazilian_States, 
    fill = as.factor(Brazilian_States), color = as.factor(Brazilian_States))) +
    geom_tile(aes(y = value/2,height = value, width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(Brazilian_States, " "), vjust = 0.2, hjust = 1, size = 6)) +
    geom_text(aes(y = value, label = Value_lbl, hjust = 0, size = 6)) +
    coord_flip(clip = "off", expand = FALSE) +
    transition_reveal(year, reveal_time) +
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

properties_2006 <- read.csv2('spreadsheet/table933.csv',skip=5, nrows=1)
properties_2017 <- read.csv2('spreadsheet/table6783.csv',skip=5, nrows=1)
properties_2006 <- properties_2006[,-(1:2)] 
properties_2017 <- properties_2017[,-1]     
properties_2006 <- as.numeric(as.character(unlist(properties_2006[1,], use.names=FALSE)))
properties_2017 <- as.numeric(as.character(unlist(properties_2017[1,], use.names=FALSE)))

NROW(properties_2006) == NROW(properties_2017)
properties_2017[18] <- properties_2017[18] + properties_2017[19] 
properties_2017 <- properties_2017[-19] 

properties_range <- c("0-0,1","0,1-0,2","0,2-0,5","0,5-1","1-2","2-3","3-4","4-5","5-10","10-20","20-50","50-100","100-200","200-500","500-1000","1000-2500","2500+","NA")
sequence <- seq(1,18)

plot_ly(x = (properties_2017-properties_2006)/1000, y = reorder(properties_range,sequence),type = 'bar', orientation = 'h',marker = list(color = 'rgba(50, 171, 96, 0.6)',line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85), title="Range of property size (hectares)"), xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Variation in Number of Dairy Farms (thousands)"), font=list(size=8)) %>% 
add_annotations(xref = 'x1', yref = 'y',x = (properties_2017-properties_2006)/1000,  y = properties_range, text = paste(round((properties_2017-properties_2006)/1000, 0), 'k'), font = list(family = 'Arial', size = 8, color = 'rgb(0, 0, 0)'),showarrow = FALSE)

plot_ly(x = ((properties_2017-properties_2006)/properties_2006)*100, y = reorder(properties_range,sequence),type = 'bar', orientation = 'h',marker = list(color = 'rgba(128, 0, 128, 0.6)',line = list(color = 'rgba(102, 102, 102, 1.0)', width = 1))) %>% 
layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85), title="Range of property size (hectares)"), xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE, title="Percentual Variation of Dairy Farms (%)"),font=list(size=8)) %>% 
add_annotations(xref = 'x1', yref = 'y',x = ((properties_2017-properties_2006)/properties_2006)*100,  y = properties_range, text = paste(round(((properties_2017-properties_2006)/properties_2006)*100, 2), '%'),font = list(family = 'Arial', size = 8, color = 'rgb(0, 0, 0)'),showarrow = FALSE)

properties_range_2 <- c("0-10","10-20","20-50","50-100","100-200","200-1000","1000+","N.I")
properties_2006_2 <- c(sum(properties_2006[1:9]),properties_2006[10],properties_2006[11],properties_2006[12],properties_2006[13],sum(properties_2006[14:15]),sum(properties_2006[16:17]),properties_2006[18])
properties_2017_2 <- c(sum(properties_2017[1:9]),properties_2017[10],properties_2017[11],properties_2017[12],properties_2017[13],sum(properties_2017[14:15]),sum(properties_2017[16:17]),properties_2017[18])
sequence_2 <- c(1,2,3,4,5,6,7,8)

b<-plot_ly(y = (properties_2017_2-properties_2006_2)/1000, x = reorder(properties_range_2,sequence_2),type = 'bar', orientation = 'v',marker = list(color = 'rgba(171, 51, 96, 0.6)',line = list(color = 'rgba(0, 0, 0, 1.0)', width = 1))) %>% 
layout(annotations=list(x=reorder(properties_range_2,sequence_2),y=(properties_2017_2-properties_2006_2)/1000,text = paste(round((properties_2017_2-properties_2006_2)/1000, 0), 'k'),font = list(family = 'Arial', size = 15, color = 'rgb(0, 0, 0)'),showarrow = TRUE,xref = "x", yref = "y",ax=10,ay=20),showlegend=FALSE, yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title="Reduction of Dairy Farms by Range (thousands) (2006->2017)"), xaxis = list(tickfont = list(size=12,color = "black"),zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Range of property size (hectares). *N.I (Not Informed)"),font=list(size=11))

c<-plot_ly(y = ((properties_2017_2-properties_2006_2)/properties_2006_2)*100, x = reorder(properties_range_2,sequence_2),type = 'bar', orientation = 'v',marker = list(color = 'rgba(250, 0, 50, 0.6)',line = list(color = 'rgba(0, 0, 0, 1.0)', width = 1))) %>% 
layout(showlegend=FALSE,annotations=list(x=reorder(properties_range_2,sequence_2),y=((properties_2017_2-properties_2006_2)/properties_2006_2)*100,text = paste(round(((properties_2017_2-properties_2006_2)/properties_2006_2)*100, 0), '%'),font = list(family = 'Arial', size = 15, color = 'rgb(0, 0, 0)'),showarrow = TRUE,xref = "x2", yref = "y2",ax=10,ay=20),yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title="Percentual Reduction of Dairy Farms by Range (%)(2006->2017)"), xaxis = list(tickfont = list(size=12,color = "black"),zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Range of property size (hectares). *N.I (Not Informed)"),font=list(size=11))
subplot(b,c,titleX = TRUE, titleY = TRUE,margin = c(0.035,0,0,0.8)) %>% layout(title = "Brazilian Dairy Properties Variation between 2006 and 2017")

milk_production_states_2 <- milk_production_states %>%
    group_by(year) %>%
    mutate(rank = rank(-value)) %>%
    group_by(Brazilian_States) %>% 
    ungroup()

milk_production_states_2 %>%
    plot_ly(
        x = ~value*1000, 
        y = ~reorder(Brazilian_States,-rank), 
        color = ~Brazilian_States, 
        frame = ~year, 
        type = 'bar'
    ) %>%
    layout(
        xaxis = list(
            title = "Milk Production Over the Years (Billions of liters)", ticktext = list("0B", "1B", "2B", "3B","4B", "5B","6B", "7B","8B", "9B"), 
            tickvals = list(0,1e+9,2e+9,3e+9,4e+9,5e+9,6e+9,7e+9,8e+9,9e+9),
            tickmode = "array"
        ), yaxis = list(
            title = "Brazilian States"
        )
    ) 











