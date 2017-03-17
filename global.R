library(shiny)
library(ggplot2)
library(data.table)
library(scales)

dataTL = as.data.table(read.table("Promo5.csv", 
                    sep = ",", 
                    stringsAsFactors = FALSE, 
                    header = TRUE))

dataTL$Weight = as.integer(dataTL$Weight)
dataTL$Start = as.Date(dataTL$Start, format = "%d-%m-%Y" )
dataTL$End = as.Date(dataTL$End, format = "%d-%m-%Y")
logged = FALSE