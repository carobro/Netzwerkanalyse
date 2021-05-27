## Semesterprojekt Sommersemester 2021
## @author Ernst Forer, Enea Gentilini, Carolin Bronowicz
## Group 2

rm(list=ls())
library(sp)
library(igraph)
library(leaflet)
library(sp)
# install.packages("igraph")

IstSoll_Zeiten <- read.csv("https://data.sbb.ch/explore/dataset/ist-daten-sbb/download/?format=csv&timezone=Europe/Berlin&lang=de&use_labels_for_header=true&csv_separator=%3B", sep=";")
IstSoll_Zeiten_temp <- IstSoll_Zeiten[c(1:200),]
head(IstSoll_Zeiten_temp)

IstSoll_Zeiten <- read.csv("C:/Users/caro1/Downloads/istdaten.csv",header=TRUE)


# linie <- read.csv("C:/Users/caro1/Downloads/linie-mit-betriebspunkten.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE)
#linie <- linie[c(1:200),]
linie <-  read.csv("C:/Users/caro1/Downloads/linie-mit-betriebspunkten_bearbeitet.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE)


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

write.csv(linie, "linie.csv")


####
#IstSoll_Zeiten <- read.csv("https://data.sbb.ch/explore/dataset/ist-daten-sbb/download/?format=csv&timezone=Europe/Berlin&lang=de&use_labels_for_header=true&csv_separator=%3B", sep=";")
linie <- read.csv("linie.csv")
IstSoll_Zeiten <- read.csv("C:/Users/caro1/Downloads/2021-05-14_istdaten (1).csv", sep=";")
#IstSoll_Zeiten <- read.csv("C:/Users/caro1/Downloads/2021-01-14_istdaten.csv", sep =";")
#IstSoll_Zeiten <- read.csv("C:/Users/caro1/Downloads/2021-01-15_istdaten.csv", sep =";")

data <- merge(linie, IstSoll_Zeiten, by="BPUIC")

#data <- data.frame(data$BPUIC, data$Haltestelle.Abk.U.009F.rzung, data$LINIE, data$LINIENNAME, 
#              data$Haltestelle_Start, data$Lat_start, data$Lon_start, data$Haltestelle_ende,  
#              data$Lat_ende, data$Lon_ende, data$Betriebstag, data$Fahrtbezeichner, data$Linien.Text,
#              data$Linien.ID, data$Ankunftszeit, data$An.Prognose, data$Abfahrtszeit, data$Ab.Prognose)

data <- data.frame(data$BPUIC, data$Haltestelle.Abk.U.009F.rzung, data$LINIE, data$LINIENNAME, 
                   data$Haltestelle_Start, data$Lat_start, data$Lon_start, data$Haltestelle_ende,  
                   data$Lat_ende, data$Lon_ende, data$BETRIEBSTAG, data$FAHRT_BEZEICHNER, data$LINIEN_TEXT,
                   data$LINIEN_ID, data$ANKUNFTSZEIT, data$AN_PROGNOSE, data$ABFAHRTSZEIT, data$AB_PROGNOSE)

names(data) <- c("BPUIC", "HaltAbk", "Linie", "Linienname", "Halt_start", "Lat_start", "Lon_start",
                 "Halt_ende", "Lat_ende", "Lon_ende", "Betriebstag", "FahrtID", "Typ", "LinienID", "Ankunftszeit",
                 "Ankunftszeit_Prognose", "Abfahrszeit", "Abfahrszeit_Prognose")


# dtparts <- unlist(strsplit(as.character(data$Ankunftszeit_Prognose),'[T]'))


Zug <- data[data$Linie=="850",]

#write.csv(Zug, "Zug_data.csv")
#Test <- Zug[Zug$LinienID=="19154",]

#Zug <- read.csv("Zug_data.csv", sep=";")
#plot(c(Zug$Lat_start, Zug$Lon_start))

dt_an <- strsplit(as.character(Zug$Ankunftszeit_Prognose),'[T]')
dt_ab <- strsplit(as.character(Zug$Abfahrszeit_Prognose),'[T]')
dt_plan_ab <- strsplit(as.character(Zug$Abfahrszeit),'[T]')
dt_plan_an <- strsplit(as.character(Zug$Ankunftszeit),'[T]')

an <- array(1:length(data)/2)
ab <- array(1:length(data)/2)
plan_an <- array(1:length(data)/2)
plan_ab <- array(1:length(data)/2)

for(i in 1:length(dt_an)){
  an[i] = dt_an[[i]][2]
  ab[i] = dt_ab[[i]][2]
  plan_ab[i] = dt_plan_ab[[i]][2]
  plan_an[i] = dt_plan_an[[i]][2]
}
Zug$ab<- ab
Zug$an <- an
Zug$plan_ab <- plan_ab
Zug$plan_an <- plan_an


Abfahrszeit <- strptime(Zug$Abfahrszeit, format = "%Y-%m-%dT%H:%M", tz ="CET")
Ankunftszeit <- strptime(Zug$Ankunftszeit, format = "%Y-%m-%dT%H:%M", tz ="CET")
Abfahrszeit_Plan <- strptime(Zug$Abfahrszeit_Prognose, format = "%Y-%m-%dT%H:%M", tz ="CET")
Ankunftszeit_Plan <- strptime(Zug$Ankunftszeit_Prognose, format = "%Y-%m-%dT%H:%M", tz ="CET")



Abfahrszeit <- strptime(Zug$Abfahrszeit, format = "%d.%m.%Y %H:%M", tz ="CET")
Ankunftszeit <- strptime(Zug$Ankunftszeit, format = "%d.%m.%Y %H:%M", tz ="CET")
Abfahrszeit_Plan <- strptime(Zug$Abfahrszeit_Prognose, format = "%d.%m.%Y %H:%M", tz ="CET")
Ankunftszeit_Plan <- strptime(Zug$Ankunftszeit_Prognose, format = "%d.%m.%Y %H:%M", tz ="CET")

Diff_Abfahrt <- as.data.frame(as.numeric(difftime(Abfahrszeit_Plan, Abfahrszeit)))
Diff_Abfahrt[is.na(Diff_Abfahrt)] <-0
min(Diff_Abfahrt)
max(Diff_Abfahrt)

Diff_Ankunft <- as.data.frame(as.numeric(difftime(Ankunftszeit_Plan, Ankunftszeit)))
Diff_Ankunft[is.na(Diff_Ankunft)] <-0
min(Diff_Ankunft)
max(Diff_Ankunft)

WarteHalt <- as.data.frame(as.numeric(difftime(Ankunftszeit, Abfahrszeit)))
WarteHalt_Plan <- as.data.frame(as.numeric(difftime(Ankunftszeit_Plan, Abfahrszeit_Plan)))

#Fahrt <- Diff_Fahrt-Diff_Fahrt_Ist
# Differenz <- data.frame(Diff_Abfahrt, Diff_Ankunft, Diff_Fahrt, Diff_Fahrt_Ist, Fahrt)

df <- data.frame(Diff_Abfahrt, Zug$HaltAbk, Zug$ab, Zug$LinienID, Zug$Abfahrszeit, Diff_Ankunft, Zug$Ankunftszeit)
names(df) <- c("Diff_Abfahrt", "HaltAbk", "Abfahrtszeit_kurz", "LinienID", "Abfahrt", "Diff_Ankunft", "Ankunft")


library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

df <-tibble(df)

df$HaltAbk <- factor(df$HaltAbk, levels =c("SG", "SGBR", "SGWI", "GSS", "FLA", "UZW", "WIL", "SIR","ESL", "GUN", "AD" ,"EL" ,"SCHK", "RAET", "WHE", "WGR"))

ggplot(df, aes(x=Abfahrt, y=HaltAbk, fill=Diff_Abfahrt)) + geom_tile() +coord_fixed(ratio =9/2) + 
  scale_fill_distiller(palette = "OrRd", direction=1)+ 
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank())

ggplot(df, aes(x=Ankunft, y=HaltAbk, fill=Diff_Ankunft)) + geom_tile() +coord_fixed(ratio =9/2) + 
  scale_fill_distiller(palette = "YlGnBu")+ 
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank())






linie <-  read.csv("C:/Users/caro1/Downloads/linie-mit-betriebspunkten_bearbeitet.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE)

merged <- left_join(df, linie, by=c("HaltAbk"= "Haltestelle.Abk.U.009F.rzung"))


#?left_join
haltestellen <- merged[,c(13,15)]
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

event <- data.frame(47.460771958190215, 9.027590052621342)
names(event) <- c("Lat", "Lon")
leaflet(vert) %>% addProviderTiles("Esri.WorldTopoMap", group = "Terrain")  %>% 
  addPolylines(data = edges,color="red",weight=1)%>%
  addCircleMarkers(data=vert,radius=~ifelse(degree > 20, 6, 2),color = "red") %>%
  addCircleMarkers(data=event, popup = paste0("<strong> 14.05.2021 </strong>
                                        <br> Strecke Zürich HB - St.Gallen 
                                        <br> zwischen Sirnach und Wil
                                        <br> Personenunfall
                                        <br> Unterbruch behoben um 12:57 Uhr
                                        <br> Maximale Verspätung: ", max(Diff_Abfahrt/60), " minuten"))
