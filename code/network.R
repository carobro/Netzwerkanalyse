## Semesterprojekt Sommersemester 2021
## @author Ernst Forer, Enea Gentilini, Carolin Bronowicz
## Group 2

rm(list=ls())
library(sp)
library(igraph)
library(leaflet)
library(sp)
# install.packages("igraph")

linie <- read.csv("C:/Users/caro1/Downloads/linie-mit-betriebspunkten.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE)
#linie <- linie[c(1:200),]

haltestellen <- linie[,c(7,9)]
g <- graph_from_data_frame(haltestellen, directed=TRUE)

roworder <- match(V(g)$name, as.character(haltestellen$Start))

V(g)$size <- 6 
V(g)$degree <- degree(g, mode="all")

# Tram line namess
E(g)$label <- E(g)$Start

plot.igraph(g, vertex.label.color="black",
            vertex.frame.color    =NA,vertex.color="black", 
            vertex.size =V(g)$degree,
            vertex.label.dist=0.3,
            vertex.label.degree=-pi/3, 
            vertex.label.font=2,
            vertex.label.cex   =0.5,
            edge.arrow.size=0, 
            edge.curved=0,edge.label=NA)



split <- strsplit(as.character(linie$geopos_start), ", ")
lat_anfang <- sapply(split, `[`, 1)
lon_anfang <- sapply(split, `[`, 2)
linie$Lat_start <- as.numeric(lat_anfang)
linie$Lon_start <- as.numeric(lon_anfang)

split <- strsplit(as.character(linie$geopos_ende), ",")
lat_ende <- sapply(split, `[`, 1)
lon_ende <- sapply(split, `[`, 2)
linie$Lat_ende <- as.numeric(lat_ende)
linie$Lon_ende <- as.numeric(lon_ende)

Start <- data.frame(linie$Haltestelle_Start, linie$Lat_start, linie$Lon_start)
names(Start) <- c("Haltestelle", "Lat", "Lon")
Ende <- data.frame(linie$Haltestelle_ende, linie$Lat_ende, linie$Lon_ende)
names(Ende) <- c("Haltestelle", "Lat", "Lon")

data <- rbind(Start, Ende)
roworder <- match(V(g)$name, as.character(data$Haltestelle))
# data_temp <- data[- (unique(data$Haltestelle)),]
V(g)$latitude  <- data[roworder,"Lat"]
V(g)$longitude <- data[roworder,"Lon"]
V(g)$name_long <- as.character(data[roworder,"Haltestelle"])
V(g)$size <- 6
coords <- matrix(c(V(g)$longitude,V(g)$latitude),ncol=2)
coords[is.na(coords)] <- 0
geolayout <- layout.norm(coords)
# remove loops
g <- simplify(g, remove.multiple = F, remove.loops = T) 


plot.igraph(g,layout=geolayout, vertex.label.color="black",vertex.frame.color    =NA,vertex.color="black", vertex.size =log10(V(g)$degree)*1.2+1,vertex.label.dist=0.3,vertex.label.degree=-pi/3, vertex.label=V(g)$name,vertex.label.font=2,vertex.label.cex   =0.5,edge.arrow.size=0, edge.curved=0,edge.label=NA)




# Extract data from graph
gg <- get.data.frame(g, "both")

# Convert graph vertices to SP points:
vert <- gg$vertices
coordinates(vert) <- ~longitude+latitude

# Convert edges to SP lines
edges <- gg$edges
edges <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ], vert[vert$name == edges[i, "to"], ]), "SpatialLines")
})
# add unique id to every edge
for (i in seq_along(edges)) {edges[[i]] <- spChFIDs(edges[[i]], as.character(i))}
edges <- do.call(rbind, edges)
edgesdf <- SpatialLinesDataFrame(edges,data.frame(gg$edges), match.ID = TRUE)

popup <- paste(vert$name_long,"<br/>Degree:",vert$degree)

leaflet(vert) %>% addProviderTiles("Esri.WorldTopoMap", group = "Terrain")  %>% 
  addPolylines(data = edges,color="black",weight=1)%>%
  addCircleMarkers(data=vert,radius=~ifelse(degree > 20, 6, 2),color = "black")
