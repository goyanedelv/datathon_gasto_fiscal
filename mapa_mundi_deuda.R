setwd("/datathon_gasto_fiscal/")
library(googleVis)
library(htmlwidgets)

data=read.csv("04_intermediate/countries_debt.csv")
names(data)[1]="Paises"
names(data)[11]="Deuda-2017"

Geo=gvisGeoChart(data, locationvar="Paises", 
                 colorvar="Deuda-2017",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

