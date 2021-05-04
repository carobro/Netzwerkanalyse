## Semesterprojekt Sommersemester 2021
## @author Ernst Forer, Enea Gentilini, Carolin Bronowicz
## Group 2

rm(list=ls())
library(sp)
  
betriebspunkte <- read.csv("https://data.sbb.ch/explore/dataset/linie-mit-betriebspunkten/download/?format=csv&timezone=Europe/Berlin&lang=de&use_labels_for_header=true&csv_separator=%3B", sep=";")
head(betriebspunkte)

IstSoll_Zeiten <- read.csv("https://data.sbb.ch/explore/dataset/ist-daten-sbb/download/?format=csv&timezone=Europe/Berlin&lang=de&use_labels_for_header=true&csv_separator=%3B", sep=";")
head(IstSoll_Zeiten)