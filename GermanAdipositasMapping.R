
### Map doodling with online data - adapted from rbloggers:

## http://www.r-bloggers.com/mapping-with-ggplot-create-a-nice-choropleth-map-in-r/

library(ggplot2)
library(rvest)
library(dplyr)
library(plyr)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)


## Build map of Germany - I got the shapefile from: 
# http://www.statsilk.com/maps/download-free-shapefile-maps

states.shp <- readShapeSpatial(file.choose())  # DEU_adm3.shp
class(states.shp)

names(states.shp)
print(states.shp$NAME_1)

# Create (or input) data to plot on map

num.states <- length(states.shp$NAME_1)

map.data <- data.frame(NAME_1 = states.shp$NAME_1, id = states.shp$ID_3)

head(map.data)

### Check if IDs are the same

map.data$id
states.shp$ID_3 # this checks out

unique(map.data$id, states.shp$ID_3)

# Fortify shape file to get it into a dataframe 

gpclibPermit()

states.shp.f <- fortify(states.shp, region = "ID_3")

class(states.shp.f)

head(states.shp.f)

# We merge the two dataframes together by the id variable, making sure to keep all observations in the 
# spatial data frame - IMPORTANT we need to order by order variable!!

merge.shp.coef<-merge(states.shp.f, map.data, by="id", all.x=TRUE)

final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 

## Read in website

fat_web <- read_html("http://de.statista.com/statistik/daten/studie/256599/umfrage/bmi--anteil-der-uebergewichtigen-in-deutschland-nach-bundeslaendern/")

## Create dataframe from table on website

fat_data <- fat_web %>%
    html_nodes("table") %>%
    .[[1]] %>% # first table on the page
    html_table(fill=TRUE)

# Delete some columns for the sake of brevity

fat_data$`1999` <- NULL
fat_data$`2003` <- NULL
fat_data$`2005` <- NULL
fat_data$`2009` <- NULL

## Turns out, states were read in, but values were not. Entered manually here:

fat_data$`2013` <- c(59.9, 59.4, 57.9, 57.5, 55.4, 54.2, 53.7, 52.9, 52.8, 52.4, 52.1, 51.4, 50.8,
                 50.7, 49.7, 47.3, 42.4)

# Remove "Deutschland column"

my.data <- fat_data[!fat_data$Bundesland == "Deutschland",]

## Rename columns

names(my.data)[1] <- "states"
colnames(my.data)[which(names(my.data) == "2013")] <- "year2013"
my.data$state <- sort(my.data$state) # Sort states alphabetically


## Some more renaming

colnames(final.plot)[which(names(final.plot) == "NAME_1")] <- "states"
colnames(final.plot)[which(names(final.plot) == "2013")] <- "year2013"

# Change two factor names due to misspelling

final.plot$states <- revalue(final.plot$states, c("Baden-W\xfcrttemberg"="Baden-Württemberg",
                                                        "Th\xfcringen" = "Thüringen"))


### Get an overview of how many rows per state there are (only works when plyr is detached!)

final.plot %>% 
    group_by(states) %>%
    summarize(t = length(states))

## Merge both dataframes by=states - POINT TO NOTE: The my.data frame has only 16 rows (for 16 states),
## with only one value per state
## BUT, we can just have the merge function fill everything in the merged set with the
## corresponding value for each state, sort = FALSE, and all = TRUE optional

merge.data <- merge(my.final.plot, my.data, by = "states", all = TRUE, sort = FALSE)

# Aggregate data to get mean latitude and mean longitude for each state to plot state names

statenames<- aggregate(cbind(long, lat) ~ states, data=merge.data, FUN=function(x) mean(range(x)))

dot <- data.frame(long = 11.566667, lat = 48.133333, city = "Munich") # Munich

## Plot map with state names

ggplot() +
    geom_polygon(data = merge.data, 
                 aes(x = long, y = lat, group = group, fill = year2013), 
                 color = "black", size = 0.25) + 
    coord_map() +
    scale_fill_gradient(name="Value", limits=c(0,80), low="green", high="red",
                        breaks=c(0,20,40,60,80), labels=c("Minimum[0]",20,40,60,"Maximum[80]")) +
    theme_nothing(legend = TRUE) +
    labs(title = "Prevalence of Adipositas in Germany") + 
    geom_text(data = statenames, aes(long, lat, label = states), size = 4, fontface = "bold", col = "black") +
    geom_point(data = dot, aes(long, lat), color = "bisque", size = 2) +
    geom_text(data = dot, aes(long, lat, label = city), size = 4, fontface = "bold", color = "black")


## Naturally, with more data (remember here only one value per state!!), we would get better separation
# and more detailed color differences...

# save map
ger_map <- ggplot() +
    geom_polygon(data = merge.data, 
                 aes(x = long, y = lat, group = group, fill = year2013), 
                 color = "black", size = 0.25) + 
    coord_map() +
    scale_fill_gradient(name="Percent", limits=c(0,80), low="white", high="red",
                        breaks=c(0,40,80), labels=c("Minimum[0]",40,"Maximum[80]")) +
    theme_nothing(legend = TRUE) +
    labs(title = "Prevalence of Adipositas in Germany") + 
    geom_text(data = statenames, aes(long, lat, label = states), size = 4, fontface = "bold", col = "black") +
    geom_point(data = dot, aes(long, lat), color = "bisque", size = 2) +
    geom_text(data = dot, aes(long, lat, label = city), size = 4, fontface = "bold", color = "black")

ggsave(ger_map, file = "ger_map.png", width = 6, height = 4.5, type = "cairo-png")

