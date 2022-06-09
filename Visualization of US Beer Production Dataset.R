
###################################################################
#US Beer Production Dataset
#Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-03-31
###################################################################

beer_states <- read.csv("C:/Users/dalal/Desktop/CIT Modules + Lectures + Materials/- Sem 2/Data Analytics & Visualisation - DATA9005_27510/Assignments/Assignment 1/Beer Production Dataset/beer_states.csv")
beer_taxed <- read.csv("C:/Users/dalal/Desktop/CIT Modules + Lectures + Materials/- Sem 2/Data Analytics & Visualisation - DATA9005_27510/Assignments/Assignment 1/Beer Production Dataset/beer_taxed.csv")
brewer_size <- read.csv("C:/Users/dalal/Desktop/CIT Modules + Lectures + Materials/- Sem 2/Data Analytics & Visualisation - DATA9005_27510/Assignments/Assignment 1/Beer Production Dataset/brewer_size.csv")
brewing_materials <- read.csv("C:/Users/dalal/Desktop/CIT Modules + Lectures + Materials/- Sem 2/Data Analytics & Visualisation - DATA9005_27510/Assignments/Assignment 1/Beer Production Dataset/brewing_materials.csv")

###################################################################
#attaching the datasets to access directly by the variable names
###################################################################

attach(beer_states)
attach(beer_taxed)
attach(brewer_size)
attach(brewing_materials)

###################################################################
#Loading the required libraries
###################################################################

#Filtering and mutating data
library(tidyverse)
#Used to show transition over the years
library(gganimate)
#To show data on the US map
library(usmap)
#using 'ymd' function from 'lubridate' library to convert the string into Date format
library(lubridate)
#Formatting the continuous numeric values with commas e.g., 2,00,000
library(scales)
#to highlight particular data above a value
library(gghighlight)

# turning-off scientific notation like 1e+8
options(scipen=999)  

#Summary of the data sets
summary(beer_states)
summary(beer_taxed)
summary(brewer_size)
summary(brewing_materials)

###################################################################
#Analyzing Brewing Materials Dataset (brewing_materials.csv)
###################################################################

summary(brewing_materials)

#Count of 'material_type'
brewing_materials %>% 
  count(material_type)

#Count of 'type'
brewing_materials %>% 
  count(type)

#Combining year and month data into one variable with 1st date of every month to look at changes over time in graphs
#using 'ymd' function from 'lubridate' library to convert the string into Date format
brewing_materials <- brewing_materials %>%
  mutate(date = ymd(paste(year,month,1)))

#Column graph to look at the current number of barrels for this year/month for each Ingredient type over the years
brewing_materials %>%
  ggplot(aes(date, month_current, fill = type)) +
  geom_col() + 
  labs(x = "Year",
       y = "Number of barrels",
       title = "Number of barrels with the Ingredient type over time",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       fill = "Ingredient Type") + 
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Reference for formatting the graph (theme and labels): 
#https://viz-ggplot2.rsquaredacademy.com/labels.html

#Reference for formatting the continuous numeric values with commas e.g., 2,00,000:
#https://stackoverflow.com/questions/37713351/formatting-ggplot2-axis-labels-with-commas-and-k-mm-if-i-already-have-a-y-sc


#Some of the data from 2016 is not available in the 'brewing_materials.csv' dataset for some reason
#Filtering out the years after 2016 from the 'brewing_materials.csv' dataset
brewing_materials <- brewing_materials %>%
  filter(year < 2016)

#Filtering out the 'Total Non-Grain Products,Total Grain products and Total Used' types
brewing_materials %>%
  filter(!str_detect(material_type, "Total")) %>%
  ggplot(aes(date, month_current, fill = type)) +
  geom_col() + 
  labs(x = "Year",
       y = "Number of barrels",
       title = "Number of barrels with the Ingredient type (Till 2016)",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       fill = "Ingredient Type") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Reordering the 'type' based on the values of 'month_current' value
#Malt and malt products are the most used ingredients in the beer production
brewing_materials %>%
  filter(!str_detect(material_type, "Total")) %>%
  mutate(type = reorder(type, month_current)) %>%
  ggplot(aes(date, month_current, fill = type)) +
  geom_col() + 
  labs(x = "Year",
       y = "Number of barrels",
       title = "Number of barrels with the Ingredient type (Till 2016)",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       fill = "Ingredient Type") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Considering only the "Total Non-Grain products" and "Total Grain products"
brewing_materials %>%
  filter(material_type %in% c("Total Non-Grain products","Total Grain products")) %>%
  mutate(type = reorder(type, month_current)) %>%
  ggplot(aes(date, month_current, fill = type)) +
  geom_col() + 
  labs(x = "Year",
       y = "Number of barrels",
       title = "Number of barrels with the Ingredient type (Till 2016)",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       fill = "Ingredient Type") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#####################
#Distribution of the Ingredients used in Beer Production to get the clear picture
#####################

brewing_materials %>%
  mutate(type = reorder(type, month_current)) %>%
  ggplot(aes(type, month_current, fill = type)) +
  geom_col() +
  coord_flip() + 
  labs(x = "Ingredient Type",
       y = "Number of barrels",
       title = "Distribution of the Ingredients",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma)

#Distribution of Ingredients considering only the "Total Non-Grain products" and "Total Grain products"
brewing_materials %>%
  filter(str_detect(material_type, "Total")) %>%
  mutate(type = reorder(type, month_current)) %>%
  ggplot(aes(type, month_current, fill = material_type)) +
  geom_col() +
  coord_flip() + 
  labs(x = "Ingredient Type",
       y = "Number of barrels",
       title = "Distribution of the Ingredients",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma)

#Filtering out the 'Total Non-Grain Products,Total Grain products and Total Used' types
brewing_materials %>%
  filter(!str_detect(material_type, "Total")) %>%
  mutate(type = reorder(type, month_current)) %>%
  ggplot(aes(type, month_current, fill = material_type)) +
  geom_col() +
  coord_flip() + 
  labs(x = "Ingredient Type",
       y = "Number of barrels",
       title = "Distribution of the Ingredients",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       fill = "Ingredient Type") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#####################
#Line Trend Chart to see the seasonality over the years in the number of barrels considering 'Total Used' material type
#####################

brewing_materials %>%
  filter(material_type == "Total Used") %>%
  ggplot(aes(month, month_current, color = factor(year))) +
  geom_line() +
  labs(x = "Month",
       y = "Number of barrels",
       title = "Seasonality Changes over Time",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       color = "Year") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Higher number of barrels in the summer and lowest in November months

#####################
#Facet view of the Ingredient types over the years
#####################

#scales = "free" in the facet_wrap function to not have the fixed scaling for the number of barrels
brewing_materials %>% 
  filter(!str_detect(material_type, "Total")) %>%
  ggplot(aes(factor(year), y = month_current,fill = "red")) + 
  geom_col() + 
  facet_wrap(~type, scales = "free") + 
  labs(x = "Year",
       y = "Number of barrels",
       title = "Ingredients used in Beer Production over the years",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma)

#Filtering out the 'Total Non-Grain Products and Total Grain products' types
brewing_materials %>% 
  filter(material_type %in% c("Total Non-Grain products","Total Grain products")) %>%
  ggplot(aes(factor(year), y = month_current,fill = "red")) + 
  geom_col() + 
  facet_wrap(~type, scales = "free") + 
  labs(x = "Year",
       y = "Number of barrels",
       title = "Ingredients used in Beer Production over the years",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma)


###################################################################
#Analyzing Brewer Size Dataset (brewer_size.csv)
###################################################################

summary(brewer_size)

#####################
#Total Barrels produced over the years at that brewer size
#####################

brewer_size %>%
  mutate(brewer_size = reorder(brewer_size,total_barrels)) %>%
  ggplot(aes(factor(year), total_barrels, fill = brewer_size)) +
  geom_col() + 
  labs(x = "Year",
       y = "Total Barrels",
       title = "Total Barrels produced over the years at that brewer size",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       fill = "Brewer Size") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Removing 'Total' value from the 'brewer_size' variable
#Removing 'NA' values from 'total_barrels' variable
#Reordering the 'brewer_size' based on the values of 'total_barrels' value
brewer_size %>%
  filter(brewer_size != "Total", !is.na(total_barrels)) %>%
  mutate(brewer_size = reorder(brewer_size,total_barrels)) %>%
  ggplot(aes(factor(year), total_barrels, fill = brewer_size)) +
  geom_col() + 
  labs(x = "Year",
       y = "Total Barrels",
       title = "Total Barrels produced over the years at that brewer size",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)",
       fill = "Brewer Size") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Number of Brewers at that brewer size
brewer_size %>%
  filter(brewer_size != "Total") %>%
  mutate(brewer_size = reorder(brewer_size,n_of_brewers)) %>%
  ggplot(aes(brewer_size, n_of_brewers,fill = "orange")) +
  geom_col()+
  coord_flip() + 
  labs(x = "Number of Brewers",
        y = "Brewer Size",
        title = "Number of Brewers at that brewer size",
        caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma)

#####################
#Total Barrels produced over the years at that brewer size
#Concept Reference: https://rww.science/2020/04/01/a-quick-tidytuesday-on-beer-breweries-and-ingredients/
#####################

#Grouping by year and ranking using row_number() to get the data in descending order
brewer_size_rank <- brewer_size %>% 
  filter(brewer_size!="Total") %>% 
  group_by(year) %>% 
  mutate(rank = row_number())

#Applying 'facet_wrap' on 'year' to show the brewer sizes through various years
brewer_size_rank %>%  
  ggplot(aes(x = reorder(brewer_size, rank), y = n_of_brewers)) +
  geom_col(fill = "orange") +
  coord_flip() +
  theme_minimal() + 
  facet_wrap(~year) +
  labs(x = "Brewer Size",
       y = "Number of Brewers",
       title = "Total Barrels produced over the years at that brewer size",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma)

#Animating Total Barrels produced over the years at that brewer size using 'gganimate' library
brewer_size_rank %>%  
  ggplot(aes(x = reorder(brewer_size, rank), y = n_of_brewers)) +
  geom_col(fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Brewer Size",
       y = "Number of Brewers",
       subtitle = "{closest_state}",
       title = "Brewer size over the years",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma) +
  transition_states(year)

#Saving the animation in the local project path
anim_save("brewer_size_over_the_years.gif")


###################################################################
#Analyzing Beer States Dataset (beer_states.csv)
###################################################################

summary(beer_states)

#####################
#Pie Chart of the States ranked by Barrels produced
#####################

#Grouping by year and ranking using row_number() to get the 'barrels' in descending order
beer_states_top <- beer_states %>% 
  group_by(year) %>% 
  mutate(rank = row_number(barrels)) %>%
  group_by(state) %>% 
  filter(rank <= 4)

#Pie Chart of the States ranked by Barrels produced
ggplot(beer_states_top, aes(x = "",fill = state)) + 
  geom_bar(width=1, color="white") +
  coord_polar(theta = "y", start=0) + 
  labs(x = "",
       y = "",
       fill = "State",
       title = "Pie chart of Barrels Produced",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    plot.title = element_text(color = "red", size = 14, face = "bold"),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Ranking was not effective here as I gave rank <=4 but 10 states were visible.
#But the data in the pie chart reflects correctly the barrels produced per state

#####################
#Beer Produced by State
#Concept Reference: https://www.christopheryee.org/blog/tidytuesday-beer-production/
#####################

#I was trying to achieve the animated graph produced in the link above but was not successful enough.
#Reordering was not successful based on the increasing values of Total Barrels produced to the top

#Grouping by year and ranking using row_number() to get the 'barrels' in descending order
beer_states_top_20 <- beer_states %>% 
  group_by(year) %>% 
  mutate(rank = row_number(barrels)) %>%
  group_by(state) %>% 
  filter(rank <= 20)

#Animation of Beer Production by State over the years
beer_states_top_20 %>% 
  ggplot(aes(reorder(state, rank), barrels, fill=state)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(x = "State",
       y = "Total Barrels",
       subtitle = "Total Barrels over the Year: {closest_state}",
       title = "Beer Production by State over the years",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "none") + 
  scale_y_continuous(labels = comma) +
  transition_states(year)

#Saving the animation in the local project path
anim_save("beer_production_by_state.gif")


#####################
#Beer Produced by Each State showing the trend by 'type' (type of production)
#####################

#Column graph showing the Total Barrels produced per type of production over the years
#Filtering the 'total' value from the 'state' variable
beer_states %>% 
  filter(!state == "total") %>% 
  ggplot(aes(x = factor(year), y = barrels, fill = type)) +
  geom_col() + 
  labs(x = "Year",
       y = "Total Barrels",
       fill = "Type of Production",
       title = "Total Barrels produced per type of production",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma)

#Applying 'facet_wrap' on 'state' to show the Total Barrels produced by each state per type of production
beer_states %>% 
  filter(!state == "total") %>% 
  ggplot(aes(x = year, y = barrels, color = type)) +
  geom_line() +
  facet_wrap(~ state, scales = "free_y") +
  labs(x= "",
       y = "Barrels Produced",
       color = "Type of Production",
       title = "Beer Produced per State",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "bottom") + 
  scale_y_continuous(labels = comma)


#####################
#US Map of Beer Production per each type of production
#####################

#2019 'On-Premise' beer production type
beer_2019_on_premises <- beer_states %>%
  filter(state != "total", year == '2019', type == "On Premises")

plot_usmap(data = beer_2019_on_premises, values = "barrels", labels = T) + 
  scale_fill_continuous(low = "white", high = "red", 
                        name = "On-premise beer production (2019)") + 
  guides(fill = guide_colorbar(barwidth = 15, 
                               title.position = "top")) +
  labs(caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_blank(),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "top")
 
#2019 'Bottles and Cans' beer production type
beer_2019_bottles_cans <- beer_states %>%
  filter(state != "total", year == '2019', type == "Bottles and Cans")

plot_usmap(data = beer_2019_bottles_cans, values = "barrels", labels = T) + 
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "Bottles and Cans beer production (2019)") + 
  guides(fill = guide_colorbar(barwidth = 15,
                               title.position = "top")) +
  labs(caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_blank(),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "top")

#2019 'Kegs and Barrels' beer production type
beer_2019_kegs_barrels <- beer_states %>%
  filter(state != "total", year == '2019', type == "Kegs and Barrels")

plot_usmap(data = beer_2019_kegs_barrels, values = "barrels", labels = T) + 
  scale_fill_continuous(low = "white", high = "green", 
                        name = "Kegs and Barrels beer production (2019)"
  ) + 
  guides(fill = guide_colorbar(barwidth = 15, 
                               title.position = "top")) +
  labs(caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_blank(),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = "top")

#####################
#Beer Production in Top US States ranking per each type of production over the years
#Reference for gghighlight: https://cran.r-project.org/web/packages/gghighlight/vignettes/gghighlight.html
#####################

#'On-Premise' beer production type
beer_states %>%
 filter((type == "On Premises") & !is.na(barrels) & state != "total") %>%
  group_by(year) %>%
  ggplot(aes(x = factor(year),y = barrels,color = state)) + 
  geom_line(aes(group = state)) + 
  labs(x= "Year",
       y = "Barrels Produced",
       title = "'On-Premise' Beer Production Type State Rankings",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma) + 
  gghighlight(max(barrels) > 150000)

#'Bottles and Cans' beer production type
beer_states %>%
  filter((type == "Bottles and Cans") & !is.na(barrels) & state != "total") %>%
  group_by(year) %>%
  ggplot(aes(x = factor(year),y = barrels,color = state)) + 
  geom_line(aes(group = state)) + 
  labs(x= "Year",
       y = "Barrels Produced",
       title = "'Bottles and Cans' Beer Production Type State Rankings",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma) + 
  gghighlight(max(barrels) > 15000000)

#'Kegs and Barrels' beer production type
beer_states %>%
  filter((type == "Kegs and Barrels") & !is.na(barrels) & state != "total") %>%
  group_by(year) %>%
  ggplot(aes(x = factor(year),y = barrels,color = state)) + 
  geom_line(aes(group = state)) + 
  labs(x= "Year",
       y = "Barrels Produced",
       title = "'Kegs and Barrels' Beer Production Type State Rankings",
       caption = "Source:  Alcohol and Tobacco Tax and Trade Bureau (TTB)") + 
  theme(
    axis.title.x = element_text(face = "bold",size = 12),
    axis.title.y = element_text(face = "bold",size = 12),
    plot.title = element_text(color = "red", size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 1, face = "italic")) + 
  scale_y_continuous(labels = comma) + 
  gghighlight(max(barrels) > 1500000)


###################################################################

