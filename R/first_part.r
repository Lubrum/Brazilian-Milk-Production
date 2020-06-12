#Created by: Luciano Brum

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(!require(plotly)) install.packages('plotly')
library(plotly)

if(!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

#- yum install cargo         (Fedora/CentOS)
#- apt-get install cargo     (Debian/Ubuntu)
#- brew install rustc        (MacOS)
if(!require(gifski)) install.packages('gifski')
library(gifski)

#If failed, try installing:
#  * deb: libavfilter-dev (Debian, Ubuntu 18.04 and up)
#  * rpm: ffmpeg-devel (https://rpmfusion.org) (Fedora, CentOS, RHEL)
#  * csw: ffmpeg_dev (Solaris)
#  * brew: ffmpeg (MacOS)
#For Ubuntu Trusty (14.04) and Xenial (16.04) use this PPA:
#  sudo add-apt-repository -y ppa:cran/ffmpeg-3
#  sudo apt-get update
#  sudo apt-get install -y libavfilter-dev
if(!require(av)) install.packages('av')
library(av)

if(!require(gganimate)) install.packages('gganimate')
library(gganimate)

milk_production_path <- '../spreadsheet/table74_2018_br_rs.csv'

milk_production <- read.csv2(milk_production_path, skip = 4, nrows = 3, sep = ',')
milk_production <- milk_production[-3,-1]

colnames(milk_production) <- gsub("X", '', colnames(milk_production))
years <- as.integer(colnames(milk_production))
brazilian_milk <- as.numeric(as.character(unlist(milk_production[1,], use.names = FALSE)))
rs_milk <- as.numeric(as.character(unlist(milk_production[2,], use.names = FALSE)))
data <- data.frame(brazilian_milk, rs_milk, years)

l <- lm(data$brazilian_milk / 1000000 ~ years, data = data)
l2 <- lm(data$rs_milk / 1000000 ~ years, data = data)

font1 <- list(family = "Arial, sans-serif", size = 22, color = "white")
font2 <- list(size = 16, color = "white")
labelx <- list(title = "Years", titlefont = font1, showticklabels = TRUE, tickfont = font2, exponentformat = "E") 
labely <- list(title = "Milk Production (Billions of liters)", titlefont = font1, showticklabels = TRUE, tickfont = font2, exponentformat = "E", gridcolor = "#666666")

plot_ly(data, x = ~years) %>% 
add_trace(y = ~brazilian_milk / 1000000,
          type = "bar", 
          name = 'Brazil',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'rgb(8,48,107)', 
                                    width = 1))) %>% 
add_trace(y = ~rs_milk / 1000000, 
          type = "bar", 
          name = 'RS', 
          marker = list(color = '#FF0266',
                        line = list(color = 'rgb(107,48,8)', 
                                    width = 1))) %>% 
add_lines(y = fitted(l), 
          name = 'Brazil', 
          line = list(color = 'rgb(158,202,225)', 
                      width = 3,
                      dash = 'dot')) %>% 
add_lines(y = fitted(l2), 
          name = 'RS state', 
          line = list(color = '#FF0266', 
                      width = 3,
                      dash = 'dot')) %>% 
layout(xaxis = labelx, 
       yaxis = labely, 
       plot_bgcolor = "rgb(0, 0, 0)", 
       paper_bgcolor = "rgb(0, 0, 0)",
       legend = list(font = list(color = "#ffffff")))

milk_production_by_state_path <- '../spreadsheet/table74_2018_states.csv'

milk_production_states <- read.csv2(milk_production_by_state_path, skip = 4, nrows = 31, stringsAsFactors = FALSE, encoding = "UTF-8", sep = ',')
milk_production_states <- milk_production_states[-28,]
colnames(milk_production_states) <- gsub("X", '', colnames(milk_production_states))
colnames(milk_production_states)[1] <- "Brazilian_States"

for(i in 2:ncol(milk_production_states)){
        milk_production_states[,i] <- gsub("[...]", '0', milk_production_states[,i])
        milk_production_states[,i] <- as.numeric(as.character(unlist(milk_production_states[,i])))
}

Regions <- c(rep("N", 7), rep("NE", 9), rep("SE", 4), rep("S", 3), rep("MW", 4))
milk_production_states <- cbind(milk_production_states, Regions)
milk_production_states <- milk_production_states %>% gather(year, value, 2:46) 
milk_production_states$year <- as.numeric(milk_production_states$year)

milk_production_states_1 <- milk_production_states %>%
    group_by(year) %>%
    mutate(rank = rank(-value),
           Value_lbl = paste0(" ", round((value * 1000) / 1000000000, 2))) %>%
    filter(rank <= 10) %>%
    ungroup()

staticplot <- ggplot(milk_production_states_1, aes(x = rank, group = Regions)) +
  geom_tile(aes(y = value / 2 , height = value, width = 0.9, fill = as.factor(Regions)), alpha = 0.9) +
  geom_text(aes(y = value, label = Brazilian_States), size = 6, nudge_y = -350000, color = "white") +
  geom_text(aes(y = value, label = as.character(Value_lbl)), size = 7, nudge_y = 250000, color = "white") +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  labs(fill = "Region") +
  theme(legend.position = "right",
        legend.key.width = unit(3, "cm"),
        legend.key.size = unit(3, "cm"),
        legend.title = element_text(hjust = 0.5, size = 20, color = "white"),
        legend.text = element_text(size = 18, color = "white"),
        legend.background = element_rect(fill = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor.x = element_line(size =.1, color = "grey" ),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", colour = "white"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic", color = "white"),
        plot.caption = element_text(size = 15, hjust = 0.5, face = "italic", color = "white"))

anim <- staticplot + 
  transition_time(year) + 
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Milk Production in Brazilian States (Billions of Liters): {round(frame_time)}',  
       subtitle = "Top 10 States",
       caption = "Milk Production in Brazilian States in Billions of Liters | Source: Brazilian Institute of Statistics and Geography.")

animate(anim, width = 1700, height = 1000 ,nframes = 880, fps = 44, renderer = av_renderer('../figures/animation.mp4'))

animate(anim, 880, fps = 44, width = 1700, height = 1000, renderer = gifski_renderer("../figures/gganim1111.gif", loop = FALSE)) +
  ease_aes('cubic-in-out') 

properties_2006_path <- '../spreadsheet/table933_2006_brazil_properties.csv'
properties_2017_path <- '../spreadsheet/table6913_2017_brazil_properties.csv'
  
properties_2006 <- read.csv2(properties_2006_path, skip = 5, nrows = 1)
properties_2017 <- read.csv2(properties_2017_path, skip = 6, sep = ',')
properties_2006 <- properties_2006[,-(1:2)] 
properties_2017 <- properties_2017[-20,]     
properties_2006 <- as.numeric(as.character(unlist(properties_2006[1,])))
properties_2017 <- as.numeric(as.character(unlist(properties_2017[,2])))

NROW(properties_2006) == NROW(properties_2017)
properties_2017[17] <- properties_2017[17] + properties_2017[18] 
properties_2017 <- properties_2017[-18] 

properties_range <- c("0-0,1", "0,1-0,2", "0,2-0,5", "0,5-1", "1-2", "2-3", "3-4", "4-5", "5-10", "10-20", "20-50", "50-100", "100-200", "200-500", "500-1000", "1000-2500", "2500+", "NA")
sequence <- seq(1,18)

plot_ly(x = (properties_2017 - properties_2006) / 1000, 
        y = reorder(properties_range, sequence),
        type = 'bar', 
        orientation = 'h',
        marker = list(color = 'rgba(50, 171, 96, 0.6)',
                      line = list(color = 'rgba(50, 171, 96, 1.0)', 
                                  width = 1))) %>% 
layout(yaxis = list(showgrid = TRUE, 
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    domain = c(0, 0.85), 
                    title = "Range of property size (hectares)"),
       xaxis = list(zeroline = FALSE, 
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    showgrid = TRUE,
                    title = "Variation in Number of Dairy Farms (thousands)",
                    gridcolor = 'rgba(100, 100, 100, 0.6)'),
       font = list(size = 16, color = 'rgb(255, 255, 255)'),
       plot_bgcolor = "rgb(0, 0, 0)", 
       paper_bgcolor = "rgb(0, 0, 0)") %>% 
add_annotations(xref = 'x1', 
                yref = 'y',
                x = ((properties_2017 - properties_2006) / 1000) - 1,  
                y = properties_range, 
                text = paste(round((properties_2017 - properties_2006) / 1000, 1), 'k'),
                font = list(family = 'Arial', 
                            size = 16, 
                            color = 'rgb(255, 255, 255)'),
                showarrow = FALSE)

plot_ly(x = ((properties_2017 - properties_2006) / properties_2006) * 100, 
        y = reorder(properties_range, sequence), 
        type = 'bar', 
        orientation = 'h',
        marker = list(color = 'rgba(128, 0, 128, 0.6)',
                      line = list(color = 'rgba(102, 102, 102, 1.0)', 
                                  width = 1))) %>% 
layout(yaxis = list(showgrid = TRUE, 
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    domain = c(0, 0.85), 
                    title = "Range of property size (hectares)"), 
       xaxis = list(zeroline = FALSE, 
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    showgrid = TRUE, 
                    title = "Percentual Variation of Dairy Farms (%)",
                    gridcolor = 'rgba(100, 100, 100, 0.6)'),
       font = list(size = 16, color = 'rgb(255, 255, 255)'),
       plot_bgcolor = "rgb(0, 0, 0)", 
       paper_bgcolor = "rgb(0, 0, 0)") %>% 
add_annotations(xref = 'x1', 
                yref = 'y',
                x = ((properties_2017 - properties_2006) / properties_2006) * 100,  
                y = properties_range, 
                text = paste(round(((properties_2017 - properties_2006) / properties_2006) * 100, 2), '%'),
                font = list(family = 'Arial', 
                            size = 16, 
                            color = 'rgb(255, 255, 255)'),
                showarrow = FALSE)

properties_range_2 <- c("0-10", "10-20", "20-50", "50-100", "100-200", "200-1000", "1000+", "N.I")
properties_2006_2 <- c(sum(properties_2006[1:9]), properties_2006[10], properties_2006[11], properties_2006[12], properties_2006[13], sum(properties_2006[14:15]), sum(properties_2006[16:17]), properties_2006[18])
properties_2017_2 <- c(sum(properties_2017[1:9]), properties_2017[10], properties_2017[11], properties_2017[12], properties_2017[13], sum(properties_2017[14:15]), sum(properties_2017[16:17]), properties_2017[18])
sequence_2 <- c(1, 2, 3, 4, 5, 6, 7, 8)

b <- plot_ly(y = (properties_2017_2 - properties_2006_2) / 1000, 
             x = reorder(properties_range_2, sequence_2),
             type = 'bar', 
             orientation = 'v',
             marker = list(color = 'rgba(171, 51, 96, 0.6)',
                           line = list(color = 'rgba(255, 255, 255, 1.0)', 
                                       width = 1))) %>% 
layout(annotations = list(x = reorder(properties_range_2, sequence_2),
                          y = (properties_2017_2 - properties_2006_2) / 1000,
                          text = paste(round((properties_2017_2 - properties_2006_2) / 1000, 0), 'k'),
                          font = list(family = 'Arial', 
                                      size = 15, 
                                      color = 'rgb(255, 255, 255)'),
                          arrowcolor = "white",
                          xref = "x", 
                          yref = "y",
                          ax = 10,
                          ay = 20),
       showlegend = FALSE, 
       yaxis = list(showgrid = TRUE, 
                    gridcolor = "rgb(48, 48, 48)",
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    domain = c(0, 0.85),
                    title = "Reduction of Dairy Farms by Range (thousands) (2006->2017)"), 
       xaxis = list(tickfont = list(size = 12,
                                    color = "white"),
                    zeroline = FALSE, 
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    showgrid = TRUE,
                    title = "Range of property size (hectares). *N.I (Not Informed)"),
       font = list(size = 11, color = 'rgb(255, 255, 255)'),
       plot_bgcolor = "rgb(0, 0, 0)", 
       paper_bgcolor = "rgb(0, 0, 0)")

c <- plot_ly(y = ((properties_2017_2 - properties_2006_2) / properties_2006_2) * 100, 
             x = reorder(properties_range_2, sequence_2),
             type = 'bar', 
             orientation = 'v',
             marker = list(color = 'rgba(250, 0, 50, 0.6)',
                           line = list(color = 'rgba(255, 255, 255, 1.0)', 
                                       width = 1))) %>% 
layout(showlegend = FALSE,
       annotations = list(x = reorder(properties_range_2, sequence_2),
                          y = ((properties_2017_2 - properties_2006_2) /properties_2006_2) * 100,
                          text = paste(round(((properties_2017_2 - properties_2006_2) / properties_2006_2) * 100, 0), '%'),
                          font = list(family = 'Arial', 
                                      size = 15, 
                                      color = 'rgb(255, 255, 255)'),
                          arrowcolor = "white",
                          xref = "x2", 
                          yref = "y2",
                          ax = 10, 
                          ay = 20),
       yaxis = list(showgrid = TRUE, 
                    gridcolor = "rgb(48, 48, 48)",
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    domain = c(0, 0.85),
                    title = "Percentual Reduction of Dairy Farms by Range (%)(2006->2017)"), 
       xaxis = list(tickfont = list(size = 12,
                                    color = "white"),
                    zeroline = FALSE, 
                    showline = FALSE, 
                    showticklabels = TRUE, 
                    showgrid = TRUE,
                    title = "Range of property size (hectares). *N.I (Not Informed)"),
       font = list(size = 11, color = 'rgb(255, 255, 255)'),
       plot_bgcolor = "rgb(0, 0, 0)", 
       paper_bgcolor = "rgb(0, 0, 0)")
subplot(b, c, titleX = TRUE, titleY = TRUE, margin = c(0.035,0,0,0.8)) %>% 
layout(title = "Brazilian Dairy Properties Variation between 2006 and 2017")