rm(list = ls())
setwd("~")

library(dplyr)

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"

data <- read.csv(paste(inp, "data.csv", sep = "/"))
bp <- read.csv(paste(inp, "Bienes_publicos.csv", sep = "/"))
colnames(bp)[4] <- "tot_del"
bp <- select(bp, -c(year, muni))
data <- data %>% 
  inner_join(bp, by = "muniYear")