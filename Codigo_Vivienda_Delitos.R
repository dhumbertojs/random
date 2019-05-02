rm(list=ls())
setwd("~")

inp = "/Users/Emmanuel RB/Documents/tesis/input"
out = "/Users/Emmanuel RB/Documents/tesis/Datos"

library (readxl)
library (dplyr)

####Creditos y urbanizacion (agua, electricidad y parques)####
vivienda <- read_excel(paste(inp, "Carac_vivienda.xlsx", sep = "/"), range = "a4:mp2676")
vivienda <- vivienda %>%
  mutate(
  edos = ifelse(nchar(Clave) == 2, 1, 0),
  quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                       ifelse(substr(Clave, 3, 5) == "997" , 1, 
                              ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                     ifelse(substr(Clave, 3, 5) == "999", 1, 0)))))
) %>% filter(edos != 1 & quit != 1) %>%
  select(-c("edos", "quit"))

names(vivienda) <- c("Clave", "Nombre", 
                     "abastecimiento_agua_1994", "plantas_potab_1994", "capacidad_plantas_1994", "volumen_anual_1994", "sist_agua_entubada_1994", "tomas_dom_agua_1994", "local_agua_1994", "sist_dren_1994", "loc_dren_1994", "tomas_elec_1994", "loc_elec_1994", "parques_1994", "jardines_1994", "cap_tot_presa_1994", "cap_ut_presa_1994", "vol_anual_presa_1994", 
                     "abastecimiento_agua_1995", "plantas_potab_1995", "capacidad_plantas_1995", "volumen_anual_1995", "sist_agua_entubada_1995", "tomas_dom_agua_1995", "local_agua_1995", "sist_dren_1995", "loc_dren_1995", "tomas_elec_1995", "loc_elec_1995", "parques_1995", "jardines_1995", "cap_tot_presa_1995", "cap_ut_presa_1995", "vol_anual_presa_1995", 
                     "abastecimiento_agua_1996", "plantas_potab_1996", "capacidad_plantas_1996", "volumen_anual_1996", "sist_agua_entubada_1996", "tomas_dom_agua_1996", "local_agua_1996", "sist_dren_1996", "loc_dren_1996", "tomas_elec_1996", "loc_elec_1996", "parques_1996", "jardines_1996", "cap_tot_presa_1996", "cap_ut_presa_1996", "vol_anual_presa_1996", 
                     "abastecimiento_agua_1997", "plantas_potab_1997", "capacidad_plantas_1997", "volumen_anual_1997", "sist_agua_entubada_1997", "tomas_dom_agua_1997", "local_agua_1997", "sist_dren_1997", "loc_dren_1997", "tomas_elec_1997", "loc_elec_1997", "parques_1997", "jardines_1997", "cap_tot_presa_1997", "cap_ut_presa_1997", "vol_anual_presa_1997", 
                     "abastecimiento_agua_1998", "plantas_potab_1998", "capacidad_plantas_1998", "volumen_anual_1998", "sist_agua_entubada_1998", "tomas_dom_agua_1998", "local_agua_1998", "sist_dren_1998", "loc_dren_1998", "tomas_elec_1998", "loc_elec_1998", "parques_1998", "jardines_1998", "cap_tot_presa_1998", "cap_ut_presa_1998", "vol_anual_presa_1998", 
                     "abastecimiento_agua_1999", "plantas_potab_1999", "capacidad_plantas_1999", "volumen_anual_1999", "sist_agua_entubada_1999", "tomas_dom_agua_1999", "local_agua_1999", "sist_dren_1999", "loc_dren_1999", "tomas_elec_1999", "loc_elec_1999", "parques_1999", "jardines_1999", "cap_tot_presa_1999", "cap_ut_presa_1999", "vol_anual_presa_1999", 
                     "abastecimiento_agua_2000", "plantas_potab_2000", "capacidad_plantas_2000", "volumen_anual_2000", "sist_agua_entubada_2000", "tomas_dom_agua_2000", "local_agua_2000", "sist_dren_2000", "loc_dren_2000", "tomas_elec_2000", "loc_elec_2000", "parques_2000", "jardines_2000", "cap_tot_presa_2000", "cap_ut_presa_2000", "vol_anual_presa_2000", 
                     "abastecimiento_agua_2001", "plantas_potab_2001", "capacidad_plantas_2001", "volumen_anual_2001", "sist_agua_entubada_2001", "tomas_dom_agua_2001", "local_agua_2001", "sist_dren_2001", "loc_dren_2001", "tomas_elec_2001", "loc_elec_2001", "parques_2001", "jardines_2001", "cap_tot_presa_2001", "cap_ut_presa_2001", "vol_anual_presa_2001", 
                     "abastecimiento_agua_2002", "plantas_potab_2002", "capacidad_plantas_2002", "volumen_anual_2002", "sist_agua_entubada_2002", "tomas_dom_agua_2002", "local_agua_2002", "sist_dren_2002", "loc_dren_2002", "tomas_elec_2002", "loc_elec_2002", "parques_2002", "jardines_2002", "cap_tot_presa_2002", "cap_ut_presa_2002", "vol_anual_presa_2002", 
                     "abastecimiento_agua_2003", "plantas_potab_2003", "capacidad_plantas_2003", "volumen_anual_2003", "sist_agua_entubada_2003", "tomas_dom_agua_2003", "local_agua_2003", "sist_dren_2003", "loc_dren_2003", "tomas_elec_2003", "loc_elec_2003", "parques_2003", "jardines_2003", "cap_tot_presa_2003", "cap_ut_presa_2003", "vol_anual_presa_2003", 
                     "abastecimiento_agua_2004", "plantas_potab_2004", "capacidad_plantas_2004", "volumen_anual_2004", "sist_agua_entubada_2004", "tomas_dom_agua_2004", "local_agua_2004", "sist_dren_2004", "loc_dren_2004", "tomas_elec_2004", "loc_elec_2004", "parques_2004", "jardines_2004", "cap_tot_presa_2004", "cap_ut_presa_2004", "vol_anual_presa_2004", 
                     "abastecimiento_agua_2005", "plantas_potab_2005", "capacidad_plantas_2005", "volumen_anual_2005", "sist_agua_entubada_2005", "tomas_dom_agua_2005", "local_agua_2005", "sist_dren_2005", "loc_dren_2005", "tomas_elec_2005", "loc_elec_2005", "parques_2005", "jardines_2005", "cap_tot_presa_2005", "cap_ut_presa_2005", "vol_anual_presa_2005", 
                     "abastecimiento_agua_2006", "plantas_potab_2006", "capacidad_plantas_2006", "volumen_anual_2006", "sist_agua_entubada_2006", "tomas_dom_agua_2006", "local_agua_2006", "sist_dren_2006", "loc_dren_2006", "tomas_elec_2006", "loc_elec_2006", "parques_2006", "jardines_2006", "cap_tot_presa_2006", "cap_ut_presa_2006", "vol_anual_presa_2006", 
                     "abastecimiento_agua_2007", "plantas_potab_2007", "capacidad_plantas_2007", "volumen_anual_2007", "sist_agua_entubada_2007", "tomas_dom_agua_2007", "local_agua_2007", "sist_dren_2007", "loc_dren_2007", "tomas_elec_2007", "loc_elec_2007", "parques_2007", "jardines_2007", "cap_tot_presa_2007", "cap_ut_presa_2007", "vol_anual_presa_2007", 
                     "abastecimiento_agua_2008", "plantas_potab_2008", "capacidad_plantas_2008", "volumen_anual_2008", "sist_agua_entubada_2008", "tomas_dom_agua_2008", "local_agua_2008", "sist_dren_2008", "loc_dren_2008", "tomas_elec_2008", "loc_elec_2008", "parques_2008", "jardines_2008", "cap_tot_presa_2008", "cap_ut_presa_2008", "vol_anual_presa_2008", 
                     "abastecimiento_agua_2009", "plantas_potab_2009", "capacidad_plantas_2009", "volumen_anual_2009", "sist_agua_entubada_2009", "tomas_dom_agua_2009", "local_agua_2009", "sist_dren_2009", "loc_dren_2009", "tomas_elec_2009", "loc_elec_2009", "parques_2009", "jardines_2009", "cap_tot_presa_2009", "cap_ut_presa_2009", "vol_anual_presa_2009", 
                     "abastecimiento_agua_2010", "plantas_potab_2010", "capacidad_plantas_2010", "volumen_anual_2010", "sist_agua_entubada_2010", "tomas_dom_agua_2010", "local_agua_2010", "sist_dren_2010", "loc_dren_2010", "tomas_elec_2010", "loc_elec_2010", "parques_2010", "jardines_2010", "cap_tot_presa_2010", "cap_ut_presa_2010", "vol_anual_presa_2010", 
                     "abastecimiento_agua_2011", "plantas_potab_2011", "capacidad_plantas_2011", "volumen_anual_2011", "sist_agua_entubada_2011", "tomas_dom_agua_2011", "local_agua_2011", "sist_dren_2011", "loc_dren_2011", "tomas_elec_2011", "loc_elec_2011", "parques_2011", "jardines_2011", "cap_tot_presa_2011", "cap_ut_presa_2011", "vol_anual_presa_2011", 
                     "abastecimiento_agua_2012", "plantas_potab_2012", "capacidad_plantas_2012", "volumen_anual_2012", "sist_agua_entubada_2012", "tomas_dom_agua_2012", "local_agua_2012", "sist_dren_2012", "loc_dren_2012", "tomas_elec_2012", "loc_elec_2012", "parques_2012", "jardines_2012", "cap_tot_presa_2012", "cap_ut_presa_2012", "vol_anual_presa_2012", 
                     "abastecimiento_agua_2013", "plantas_potab_2013", "capacidad_plantas_2013", "volumen_anual_2013", "sist_agua_entubada_2013", "tomas_dom_agua_2013", "local_agua_2013", "sist_dren_2013", "loc_dren_2013", "tomas_elec_2013", "loc_elec_2013", "parques_2013", "jardines_2013", "cap_tot_presa_2013", "cap_ut_presa_2013", "vol_anual_presa_2013", 
                     "abastecimiento_agua_2014", "plantas_potab_2014", "capacidad_plantas_2014", "volumen_anual_2014", "sist_agua_entubada_2014", "tomas_dom_agua_2014", "local_agua_2014", "sist_dren_2014", "loc_dren_2014", "tomas_elec_2014", "loc_elec_2014", "parques_2014", "jardines_2014", "cap_tot_presa_2014", "cap_ut_presa_2014", "vol_anual_presa_2014", 
                     "abastecimiento_agua_2015", "plantas_potab_2015", "capacidad_plantas_2015", "volumen_anual_2015", "sist_agua_entubada_2015", "tomas_dom_agua_2015", "local_agua_2015", "sist_dren_2015", "loc_dren_2015", "tomas_elec_2015", "loc_elec_2015", "parques_2015", "jardines_2015", "cap_tot_presa_2015", "cap_ut_presa_2015", "vol_anual_presa_2015")

viv <- data.frame()

nombres <- 1994:2015
x = 1994

pb = txtProgressBar(min=1, max=length(nombres), style=3)
for(x in 1:length(nombres)) {
  
  tempo <- vivienda %>%
    select(Clave, ends_with(as.character(nombres[x]))) %>%
    mutate_all(funs(as.numeric)) %>% 
    mutate(year = nombres[x])
  
  tempo$Clave = formatC(tempo$Clave, width=5, format="d", flag = "0") 
  tempo$muniYear = paste(tempo$Clave, tempo$year, sep = "_")
    
  names(tempo) = c("muni", "abastecimiento_agua", "plantas_potab", "capacidad_plantas", "volumen_anual", "sist_agua_entubada", "tomas_dom_agua", "local_agua", "sist_dren", "loc_dren", "tomas_elec", "loc_elec", "parques", "jardines", "cap_tot_presa", "cap_ut_presa", "vol_anual_presa", "year", "muniYear")
  
  viv = bind_rows(viv, tempo)
  
  rm(tempo)
  setTxtProgressBar(pb, x)
  
}

rm(vivienda, nombres, x)

viv <- select(viv, muniYear, muni, year, tomas_dom_agua, tomas_elec, #jardines, parques, 
              sist_dren)

summary(viv)
as.data.frame(viv, row.names = NULL)
write.csv(viv, paste(out, "viv.csv", sep = "/"), row.names = F, fileEncoding ="UTF-8")

rm(viv)

####Delitos registrados 1994-2010####
del <- read_excel(paste(inp, "Delitos.xlsx", sep = "/"), range = "a4:dq2676")
colnames(del) <- c("Clave", "Nombre", 
                   "total_1994", "danio_1994", "delsex_1994", "hom_1994", "les_1994", "robo_1994", "otros_1994", 
                   "total_1995", "danio_1995", "delsex_1995", "hom_1995", "les_1995", "robo_1995", "otros_1995", 
                   "total_1996", "danio_1996", "delsex_1996", "hom_1996", "les_1996", "robo_1996", "otros_1996", 
                   "total_1997", "danio_1997", "delsex_1997", "hom_1997", "les_1997", "robo_1997", "otros_1997", 
                   "total_1998", "danio_1998", "delsex_1998", "hom_1998", "les_1998", "robo_1998", "otros_1998", 
                   "total_1999", "danio_1999", "delsex_1999", "hom_1999", "les_1999", "robo_1999", "otros_1999", 
                   "total_2000", "danio_2000", "delsex_2000", "hom_2000", "les_2000", "robo_2000", "otros_2000", 
                   "total_2001", "danio_2001", "delsex_2001", "hom_2001", "les_2001", "robo_2001", "otros_2001", 
                   "total_2002", "danio_2002", "delsex_2002", "hom_2002", "les_2002", "robo_2002", "otros_2002", 
                   "total_2003", "danio_2003", "delsex_2003", "hom_2003", "les_2003", "robo_2003", "otros_2003", 
                   "total_2004", "danio_2004", "delsex_2004", "hom_2004", "les_2004", "robo_2004", "otros_2004", 
                   "total_2005", "danio_2005", "delsex_2005", "hom_2005", "les_2005", "robo_2005", "otros_2005", 
                   "total_2006", "danio_2006", "delsex_2006", "hom_2006", "les_2006", "robo_2006", "otros_2006", 
                   "total_2007", "danio_2007", "delsex_2007", "hom_2007", "les_2007", "robo_2007", "otros_2007", 
                   "total_2008", "danio_2008", "delsex_2008", "hom_2008", "les_2008", "robo_2008", "otros_2008", 
                   "total_2009", "danio_2009", "delsex_2009", "hom_2009", "les_2009", "robo_2009", "otros_2009", 
                   "total_2010", "danio_2010", "delsex_2010", "hom_2010", "les_2010", "robo_2010", "otros_2010")

del <- del %>%
  mutate(
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0)))))
  ) %>% filter(edos != 1 & quit != 1) %>%
  select(-c("edos", "quit"))

delitos <- data.frame()

nombres <- 1994:2010
x = 1994

pb = txtProgressBar(min=1, max=length(nombres), style=3)
for(x in 1:length(nombres)) {
  
  tempo <- del %>%
    select(Clave, ends_with(as.character(nombres[x]))) %>%
    mutate_all(funs(as.numeric)) %>% 
    mutate(year = nombres[x])
  
  tempo$Clave = formatC(tempo$Clave, width=5, format="d", flag = "0")
  tempo$muniYear = factor(paste(tempo$Clave, tempo$year, sep = "_"))
  
  names(tempo) = c("muni", "total", "danio", "delsex", "hom", "les", "robo", "otros", "year", "muniYear")
  
  delitos = bind_rows(delitos, tempo)
  
  rm(tempo)
  setTxtProgressBar(pb, x)
  
}

#rm(del, nombres, x)
summary(delitos)

####Exportar datos####
summary(delitos)
as.data.frame(delitos, row.names = NULL)
write.csv(delitos, paste(out, "delitos.csv", sep = "/"), row.names = F, fileEncoding ="UTF-8")
rm(delitos)