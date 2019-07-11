rm(list = ls())
setwd("~")

library(dplyr)

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"

del <- read.csv(paste(inp, "delitos.csv", sep = "/"), stringsAsFactors = F)
del <- select(del, -c(muni, year))
ser <- read.csv(paste(inp, "viv.csv", sep = "/"), stringsAsFactors = F)

bp <- full_join(del, ser, by = "muniYear")
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

bp <- bp %>% 
  group_by(muni) %>% 
  mutate(
    lag_del_1 = lag(tasa_tot_del, n = 1),
    lag_hom_1 = lag(tasa_hom, n = 1),
    lag_agua_1 = lag(tasa_agua, n = 1),
    lag_elec_1 = lag(tasa_elec, n = 1),
    lag_dren_1 = lag(tasa_dren, n = 1),
    
    lag_del_2 = lag(tasa_tot_del, n = 2),
    lag_hom_2 = lag(tasa_hom, n = 2),
    lag_agua_2 = lag(tasa_agua, n = 2),
    lag_elec_2 = lag(tasa_elec, n = 2),
    lag_dren_2 = lag(tasa_dren, n = 2),
    
    lag_del_3 = lag(tasa_tot_del, n = 3),
    lag_hom_3 = lag(tasa_hom, n = 3),
    lag_agua_3 = lag(tasa_agua, n = 3),
    lag_elec_3 = lag(tasa_elec, n = 3),
    lag_dren_3 = lag(tasa_dren, n = 3),
    
    lag_del_4 = lag(tasa_tot_del, n = 4),
    lag_hom_4 = lag(tasa_hom, n = 4),
    lag_agua_4 = lag(tasa_agua, n = 4),
    lag_elec_4 = lag(tasa_elec, n = 4),
    lag_dren_4 = lag(tasa_dren, n = 4),
    
    lag_del_5 = lag(tasa_tot_del, n = 5),
    lag_hom_5 = lag(tasa_hom, n = 5),
    lag_agua_5 = lag(tasa_agua, n = 5),
    lag_elec_5 = lag(tasa_elec, n = 5),
    lag_dren_5 = lag(tasa_dren, n = 5)
  ) %>% 
  ungroup()

as.data.frame(bp, row.names = NULL)
write.csv(bp, paste(inp, "Bienes_publicos.csv", sep = "/"), row.names = F)