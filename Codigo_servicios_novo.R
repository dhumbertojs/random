rm(list = ls())
setwd("~")

library(dplyr)

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"

del <- read.csv(paste(inp, "delitos.csv", sep = "/"), stringsAsFactors = F)
del <- select(del, -c(muni, year))
ser <- read.csv(paste(inp, "viv.csv", sep = "/"), stringsAsFactors = F)

bp <- inner_join(del, ser, by = "muniYear")
bp <- bp %>% 
  select("muniYear", "muni", "year", "total",  "hom", "tomas_dom_agua", "tomas_elec", "sist_dren") %>% 
  mutate(
    muni = formatC(muni, width=5, format="d", flag = "0") 
  )

pob <- read.csv(paste(inp, "censo.csv", sep = "/"))

bp <- inner_join(bp, pob, by = "muniYear")

#Tasa por cada 100,000 habitantes####
bp <- bp %>% 
  mutate(
    tasa_tot_del = (total/Pob_Total) * 100000, 
    tasa_hom = (hom/Pob_Total) * 100000, 
    tasa_agua = (tomas_dom_agua/Pob_Total) * 100000, 
    tasa_elec = (tomas_elec/Pob_Total) * 100000, 
    tasa_dren = (sist_dren/Pob_Total) * 100000
  )