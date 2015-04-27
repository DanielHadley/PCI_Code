
# Import data ####
setwd("C:/Users/dhadley/Documents/GitHub/PCI_Code")
setwd ("~/Documents/Git/PCI_Code") #at home



library(plyr)
library(ggplot2)
library(scales) # for changing from scientific notation
library(reshape2)
# Maping tools
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("RColorBrewer")
require("ggmap")


#### Map it with city-town GIS layer ####
# https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
# http://www.kevjohnson.org/making-maps-in-r/
# http://www.kevjohnson.org/making-maps-in-r-part-2/


# First prepare the street df
mass <- readOGR(dsn="./2015_Shapefile", layer="BL_SEGMENTS_2015")


shp       <- spTransform(mass, CRS("+proj=longlat +datum=WGS84"))
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)

data_fort   <- fortify(shp)

data_merged <- join(data_fort, shp.df, by="id")


# Map
map <- get_map(location = "Highland and Lowell St, Somerville, Massachusetts", zoom=14, maptype="roadmap", color = "bw")
ggmap(map)

ggmap(map) +
  geom_path(data=data_merged,size=2,
            aes(x=long,y=lat,group=group,color=OCI, alpha=.9))+
  labs(x="",y="")+
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scale_colour_gradientn(colours=rev(brewer.pal(9,"YlGnBu")))


# Map
map <- get_map(location = "Highland and Lowell St, Somerville, Massachusetts", zoom=14, maptype="roadmap", color = "bw")
ggmap(map)

ggmap(map) +
  geom_path(data=data_merged,size=2,
            aes(x=long,y=lat,group=group,color=OCI))+
  labs(x="",y="")+
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scale_colour_gradientn(colours=rev(brewer.pal(9,"YlGnBu")))


# A for loop that will create a dot map for every neighborhood you specify
neighborhoodList <- c("Assembly Square", "Ball Square", "City Hall", "Davis Square", "East Somerville", "Gilman Square", "Magoun Square", "Porter Square", "Prospect Hill", "Spring Hill", "Teele Square", "Ten Hills", "Union Square", "Winter Hill")

for (n in 1:(length(neighborhoodList))) {
  map <- get_map(location = paste(neighborhoodList[n], "Somerville, MA", sep=", "), zoom=16, maptype="roadmap", color = "bw")
  ggmap(map) +
    geom_path(data=data_merged,size=1.5,
              aes(x=long,y=lat,group=group,color=OCI))+
    labs(x="",y="")+
    theme(axis.text=element_blank(),axis.ticks=element_blank()) +
    scale_colour_gradientn(colours=rev(brewer.pal(9,"YlGnBu"))) + 
    ggtitle(neighborhoodList[n])
  
  ggsave(paste("./map_",neighborhoodList[n], ".png", sep=""), dpi=250, width=6, height=5)
  
}


## Now try to combine with past data
d <- read.csv("./PCI.csv")