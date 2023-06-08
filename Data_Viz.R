#loading the libraries
library(tidyverse) 
library(ggplot2) 
library(janitor)
library(readxl)
library(plotly)
library(treemapify)
library(dplyr)

#loading electronics data set 
electronics <- read_xlsx("C:\\Users\\User\\Downloads\\electronics.xlsx")
colnames(electronics)
View(electronics)
electronics <- clean_names(electronics, "lower_camel")

#Scatter Plot with trend line
scatter <- ggplot(data = electronics, mapping = aes(x = noOfCommercials, y = salesVolume))+
  geom_point()+
  geom_smooth(se = F, method = lm )+
  labs(x = "Number of Commercial", y = "Sales Volume")
ggplotly(scatter)

#loading kirklandregional data set
kirklandregional <- read_xlsx("C:\\Users\\User\\Downloads\\kirklandregional.xlsx")
#Data cleaning process
kirklandregional <- clean_names(kirklandregional, "lower_camel")
colnames(kirklandregional)
unique(kirklandregional$month)
kirklandregional$month <- ordered(kirklandregional$month, 
                                  level = c("Jan", "Feb", "Mar", "Apr", "May", 
                                            "Jun", "Jul", "Aug", "Sep", "Oct", 
                                            "Nov", "Dec"))

#ploating line graphs
line1 <- ggplot(data = kirklandregional, mapping = aes(x = month, y =  north,  group = 1))+
  geom_line(color = "blue")+
  labs(x = "Month", y = "North")
ggplotly(line1)

line2 <- ggplot(data = kirklandregional, mapping = aes(x = month, y =  south,  group = 1))+
  geom_line(color = "orange")+
  labs(x = "Month", y = "South")
 ggplotly(line2)

 line3 <- ggplot(data = kirklandregional, mapping = aes(x = month, group = 1))+
  geom_line(aes(y = north) , color = "blue")+
  geom_line(aes(y = south) , color = "orange")+
  labs(x = "Month", y = "Salels ($10000)")
ggplotly(line3)

#loading accountsmanaged data set
accounts_managed <- read_xlsx("C:\\Users\\User\\Downloads\\accounts_managed.xlsx")
View(accounts_managed)
accounts_managed <- clean_names(accounts_managed, "lower_camel")
colnames(accounts_managed)


#bar chart  
bar <- ggplot(data = accounts_managed, mapping = 
                aes(x = accountsManaged, 
                    y = reorder(manager, accountsManaged )))+
  geom_bar(fill = "skyblue", stat = "identity")+
  labs(x = "Accounts Managed", y = "Managers")
ggplotly(bar)

#loading billionaires data set 
billionaires <- read_xlsx("C:\\Users\\User\\Downloads\\billionaires.xlsx")
billionaires <- clean_names(billionaires, "lower_camel")

#bubble chart diagram
Buuble_chart <- ggplot(billionaires, aes(x = billionairesPer10MResidents, y = perCapitaIncome, 
                         size = numberOfBillionaires, color = country))+
  geom_point(alpha = 0.7)+
  scale_size(name = "Number of Billionaires")+
  theme(legend.position = "none")+
  xlab("Billionaires Per 10M Residents")+
  ylab("Per Capita Income")
ggplotly(Buuble_chart)

#cluster column chart using kirklandregional data set 
#First unpivot the table 

Newkirklandregional <- kirklandregional %>%
  pivot_longer(cols = c ("north" : "south"), 
               values_to = "values", names_to = "direction")
View(Newkirklandregional)
Newkirklandregional$direction <- if_else(Newkirklandregional$direction 
                                         == "north", "North", "South") 

#stacked column diagram
stacked <- ggplot(Newkirklandregional, aes(x = month, y = values, fill = direction))+
  geom_bar(stat = "identity")+
  labs(title = "Stacked Column Diagram", x = "Month", y = "Values", fill = "Direction")
ggplotly(stacked)

#clustered column diagram
clustered <- ggplot(Newkirklandregional, aes(x = month, y = values, fill = direction))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Clustered Column Diagram", x = "Month", y = "Values", fill = "Direction")
ggplotly(clustered)

#loading global100 data set 
global100 <- read_xlsx("C:\\Users\\User\\Downloads\\global100.xlsx")
str(global100)
global100 <- clean_names(global100, "snake")

#Tree map
ggplot(global100, aes(fill = continent, 
                      area = market_value_billions_us, label = company))+
  geom_treemap( color = "grey")+
   geom_treemap_text(size = 8)+
   labs(fill = "Continent")


#loading worldgdp2014 data set 
worldgdp2014 <- read_xlsx("C:\\Users\\User\\Downloads\\worldgdp2014.xlsx")
str(worldgdp2014)
worldgdp2014 <- clean_names(worldgdp2014, "snake")
n_distinct(worldgdp2014$country_name)
mapdata <- map_data("world")
mapdata <- dplyr::rename(mapdata, country_name = region)
mapdata1<- left_join(mapdata, worldgdp2014, by = "country_name")

mapdata1 <- mapdata1 %>% 
  filter(!is.na(mapdata1$gdp_2014_billions_us &
                            mapdata1$gdp_growth_2014_percent))

map <- ggplot(mapdata1, aes(x = long, y = lat, group = group, label = country_name))+
  geom_polygon(aes(fill = gdp_2014_billions_us), color = "black")+
  scale_fill_gradient(name = "GDP 2014 Billions ($)", low = "yellow", high = "red")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        rect = element_blank())
ggplotly(map)

#loading homesalesstacked data set
homesalesstacked <- read_xlsx("C:\\Users\\User\\Downloads\\homesalesstacked.xlsx")
homesalesstacked <- clean_names(homesalesstacked, "snake")

box <- ggplot(homesalesstacked, aes(x = location, y = selling_price, fill = location))+
  geom_boxplot()+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none")+
  xlab("Location")+
  ylab("Selling Price")
ggplotly(box)
