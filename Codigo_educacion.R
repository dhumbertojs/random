rm(list=ls())
setwd("~")

inp = "/Users/Emmanuel RB/Documents/tesis/input"
out = "/Users/Emmanuel RB/Documents/tesis/Datos"

library (readxl)
library (dplyr)

edu <- read_excel(paste(inp, "culturales_educativas.xlsx", sep = "/"), range = "a4:ad2676")
colnames(edu) <- c("Clave", "Municipio", "mayor6_leer_esc_1995", "may5_asistencia_1995", "may5_nivel_1995", "may18_prof_1995", "may18_posg_1995", "may15_escol_mean_1995", "may5_indigena_1995", "mayor6_leer_esc_2000", "may5_asistencia_2000", "may5_nivel_2000", "may18_prof_2000", "may18_posg_2000", "may15_escol_mean_2000", "may5_indigena_2000", "mayor6_leer_esc_2005", "may5_asistencia_2005", "may5_nivel_2005", "may18_prof_2005", "may18_posg_2005", "may15_escol_mean_2005", "may5_indigena_2005", "mayor6_leer_esc_2010", "may5_asistencia_2010", "may5_nivel_2010", "may18_prof_2010", "may18_posg_2010", "may15_escol_mean_2010", "may5_indigena_2010")

#edu90 <- read.delim(paste(inp, "ITER_NALTXT90.txt", sep = "/"))
#[1] "entidad"    "nom_ent"    "mun"        "nom_mun"    "loc"       
#[6] "nom_loc"    "longitud"   "latitud"    "altitud"    "p_total"   
#[11] "hombres"    "mujeres"    "pob_lee"    "pob_no_l"   "alfabet"   
#[16] "analfbet"   "asis_esc"   "n_as_esc"   "asis6_es"   "n_as6_es"  
#[21] "n_hab_esp"  "habla_esp"  "sin_ins"    "prim_inc"   "prim_com"  
#[26] "ins_pprim"  "p_e_act"    "p_e_inac"   "pob_ocup"   "sec_prim"  
#[31] "sec_sec"    "sec_ter"    "t_vivhab"   "viv_part"   "ocup_viv"  
#[36] "prom_viv"   "prom_cua"   "pared_la"   "techo_la"   "piso_tie"  
#[41] "viv_1_c"    "viv_2_c"    "c_agua_ent" "c_drenaje"  "c_e_elect" 
#[46] "viv_pprop

edu <- edu %>% mutate(
  edos = ifelse(nchar(Clave) == 2, 1, 0),
  quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                       ifelse(substr(Clave, 3, 5) == "997" , 1, 
                              ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                     ifelse(substr(Clave, 3, 5) == "999", 1, 0)))))
) %>%
  filter(edos != 1 & quit != 1) %>%
  select(-Municipio) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate(
    tasa95_00 = ((log(mayor6_leer_esc_2000 / mayor6_leer_esc_1995)) / 5),
    
    mayor6_leer_esc_1996 = round(mayor6_leer_esc_1995 * exp(tasa95_00 * 1)),
    mayor6_leer_esc_1997 = round(mayor6_leer_esc_1995 * exp(tasa95_00 * 2)),
    mayor6_leer_esc_1998 = round(mayor6_leer_esc_1995 * exp(tasa95_00 * 3)),
    mayor6_leer_esc_1999 = round(mayor6_leer_esc_1995 * exp(tasa95_00 * 4)),
    
    tasa00_05 = ((log(mayor6_leer_esc_2005 / mayor6_leer_esc_2000)) / 5),
    
    mayor6_leer_esc_2001 = round(mayor6_leer_esc_2000 * exp(tasa00_05 * 1)),
    mayor6_leer_esc_2002 = round(mayor6_leer_esc_2000 * exp(tasa00_05 * 2)),
    mayor6_leer_esc_2003 = round(mayor6_leer_esc_2000 * exp(tasa00_05 * 3)),
    mayor6_leer_esc_2004 = round(mayor6_leer_esc_2000 * exp(tasa00_05 * 4)),
    
    tasa05_10 = ((log(mayor6_leer_esc_2010 / mayor6_leer_esc_2005)) / 5),
    
    mayor6_leer_esc_2006 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 1)),
    mayor6_leer_esc_2007 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 2)),
    mayor6_leer_esc_2008 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 3)),
    mayor6_leer_esc_2009 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 4)),
    
    #esto me lo estoy inventando, necesito revisar si esto se puede
    mayor6_leer_esc_2011 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 6)),
    mayor6_leer_esc_2012 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 7)),
    mayor6_leer_esc_2013 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 8)),
    mayor6_leer_esc_2014 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 9)),
    mayor6_leer_esc_2015 = round(mayor6_leer_esc_2005 * exp(tasa05_10 * 10)),
    
    tasa95_00 = ((log(may5_asistencia_2000 / may5_asistencia_1995)) / 5),
    
    may5_asistencia_1996 = round(may5_asistencia_1995 * exp(tasa95_00 * 1)),
    may5_asistencia_1997 = round(may5_asistencia_1995 * exp(tasa95_00 * 2)),
    may5_asistencia_1998 = round(may5_asistencia_1995 * exp(tasa95_00 * 3)),
    may5_asistencia_1999 = round(may5_asistencia_1995 * exp(tasa95_00 * 4)),
    
    tasa00_05 = ((log(may5_asistencia_2005 / may5_asistencia_2000)) / 5),
    
    may5_asistencia_2001 = round(may5_asistencia_2000 * exp(tasa00_05 * 1)),
    may5_asistencia_2002 = round(may5_asistencia_2000 * exp(tasa00_05 * 2)),
    may5_asistencia_2003 = round(may5_asistencia_2000 * exp(tasa00_05 * 3)),
    may5_asistencia_2004 = round(may5_asistencia_2000 * exp(tasa00_05 * 4)),
    
    tasa05_10 = ((log(may5_asistencia_2010 / may5_asistencia_2005)) / 5),
    
    may5_asistencia_2006 = round(may5_asistencia_2005 * exp(tasa05_10 * 1)),
    may5_asistencia_2007 = round(may5_asistencia_2005 * exp(tasa05_10 * 2)),
    may5_asistencia_2008 = round(may5_asistencia_2005 * exp(tasa05_10 * 3)),
    may5_asistencia_2009 = round(may5_asistencia_2005 * exp(tasa05_10 * 4)),
    
    #esto me lo estoy inventando, necesito revisar si esto se puede
    may5_asistencia_2011 = round(may5_asistencia_2005 * exp(tasa05_10 * 6)),
    may5_asistencia_2012 = round(may5_asistencia_2005 * exp(tasa05_10 * 7)),
    may5_asistencia_2013 = round(may5_asistencia_2005 * exp(tasa05_10 * 8)),
    may5_asistencia_2014 = round(may5_asistencia_2005 * exp(tasa05_10 * 9)),
    may5_asistencia_2015 = round(may5_asistencia_2005 * exp(tasa05_10 * 10)),
    
    tasa95_00 = ((log(may5_nivel_2000 / may5_nivel_1995)) / 5),
    
    may5_nivel_1996 = round(may5_nivel_1995 * exp(tasa95_00 * 1)),
    may5_nivel_1997 = round(may5_nivel_1995 * exp(tasa95_00 * 2)),
    may5_nivel_1998 = round(may5_nivel_1995 * exp(tasa95_00 * 3)),
    may5_nivel_1999 = round(may5_nivel_1995 * exp(tasa95_00 * 4)),
    
    tasa00_05 = ((log(may5_nivel_2005 / may5_nivel_2000)) / 5),
    
    may5_nivel_2001 = round(may5_nivel_2000 * exp(tasa00_05 * 1)),
    may5_nivel_2002 = round(may5_nivel_2000 * exp(tasa00_05 * 2)),
    may5_nivel_2003 = round(may5_nivel_2000 * exp(tasa00_05 * 3)),
    may5_nivel_2004 = round(may5_nivel_2000 * exp(tasa00_05 * 4)),
    
    tasa05_10 = ((log(may5_nivel_2010 / may5_nivel_2005)) / 5),
    
    may5_nivel_2006 = round(may5_nivel_2005 * exp(tasa05_10 * 1)),
    may5_nivel_2007 = round(may5_nivel_2005 * exp(tasa05_10 * 2)),
    may5_nivel_2008 = round(may5_nivel_2005 * exp(tasa05_10 * 3)),
    may5_nivel_2009 = round(may5_nivel_2005 * exp(tasa05_10 * 4)),
    
    #esto me lo estoy inventando, necesito revisar si esto se puede
    may5_nivel_2011 = round(may5_nivel_2005 * exp(tasa05_10 * 6)),
    may5_nivel_2012 = round(may5_nivel_2005 * exp(tasa05_10 * 7)),
    may5_nivel_2013 = round(may5_nivel_2005 * exp(tasa05_10 * 8)),
    may5_nivel_2014 = round(may5_nivel_2005 * exp(tasa05_10 * 9)),
    may5_nivel_2015 = round(may5_nivel_2005 * exp(tasa05_10 * 10)),
    
    tasa95_00 = ((log(may18_prof_2000 / may18_prof_1995)) / 5),
    
    may18_prof_1996 = round(may18_prof_1995 * exp(tasa95_00 * 1)),
    may18_prof_1997 = round(may18_prof_1995 * exp(tasa95_00 * 2)),
    may18_prof_1998 = round(may18_prof_1995 * exp(tasa95_00 * 3)),
    may18_prof_1999 = round(may18_prof_1995 * exp(tasa95_00 * 4)),
    
    tasa00_05 = ((log(may18_prof_2005 / may18_prof_2000)) / 5),
    
    may18_prof_2001 = round(may18_prof_2000 * exp(tasa00_05 * 1)),
    may18_prof_2002 = round(may18_prof_2000 * exp(tasa00_05 * 2)),
    may18_prof_2003 = round(may18_prof_2000 * exp(tasa00_05 * 3)),
    may18_prof_2004 = round(may18_prof_2000 * exp(tasa00_05 * 4)),
    
    tasa05_10 = ((log(may18_prof_2010 / may18_prof_2005)) / 5),
    
    may18_prof_2006 = round(may18_prof_2005 * exp(tasa05_10 * 1)),
    may18_prof_2007 = round(may18_prof_2005 * exp(tasa05_10 * 2)),
    may18_prof_2008 = round(may18_prof_2005 * exp(tasa05_10 * 3)),
    may18_prof_2009 = round(may18_prof_2005 * exp(tasa05_10 * 4)),
    
    #esto me lo estoy inventando, necesito revisar si esto se puede
    may18_prof_2011 = round(may18_prof_2005 * exp(tasa05_10 * 6)),
    may18_prof_2012 = round(may18_prof_2005 * exp(tasa05_10 * 7)),
    may18_prof_2013 = round(may18_prof_2005 * exp(tasa05_10 * 8)),
    may18_prof_2014 = round(may18_prof_2005 * exp(tasa05_10 * 9)),
    may18_prof_2015 = round(may18_prof_2005 * exp(tasa05_10 * 10)),
    
    tasa95_00 = ((log(may18_posg_2000 / may18_posg_1995)) / 5),
    
    may18_posg_1996 = round(may18_posg_1995 * exp(tasa95_00 * 1)),
    may18_posg_1997 = round(may18_posg_1995 * exp(tasa95_00 * 2)),
    may18_posg_1998 = round(may18_posg_1995 * exp(tasa95_00 * 3)),
    may18_posg_1999 = round(may18_posg_1995 * exp(tasa95_00 * 4)),
    
    tasa00_05 = ((log(may18_posg_2005 / may18_posg_2000)) / 5),
    
    may18_posg_2001 = round(may18_posg_2000 * exp(tasa00_05 * 1)),
    may18_posg_2002 = round(may18_posg_2000 * exp(tasa00_05 * 2)),
    may18_posg_2003 = round(may18_posg_2000 * exp(tasa00_05 * 3)),
    may18_posg_2004 = round(may18_posg_2000 * exp(tasa00_05 * 4)),
    
    tasa05_10 = ((log(may18_posg_2010 / may18_posg_2005)) / 5),
    
    may18_posg_2006 = round(may18_posg_2005 * exp(tasa05_10 * 1)),
    may18_posg_2007 = round(may18_posg_2005 * exp(tasa05_10 * 2)),
    may18_posg_2008 = round(may18_posg_2005 * exp(tasa05_10 * 3)),
    may18_posg_2009 = round(may18_posg_2005 * exp(tasa05_10 * 4)),
    
    #esto me lo estoy inventando, necesito revisar si esto se puede
    may18_posg_2011 = round(may18_posg_2005 * exp(tasa05_10 * 6)),
    may18_posg_2012 = round(may18_posg_2005 * exp(tasa05_10 * 7)),
    may18_posg_2013 = round(may18_posg_2005 * exp(tasa05_10 * 8)),
    may18_posg_2014 = round(may18_posg_2005 * exp(tasa05_10 * 9)),
    may18_posg_2015 = round(may18_posg_2005 * exp(tasa05_10 * 10)),
    
    tasa95_00 = ((log(may15_escol_mean_2000 / may15_escol_mean_1995)) / 5),
    
    may15_escol_mean_1996 = round(may15_escol_mean_1995 * exp(tasa95_00 * 1)),
    may15_escol_mean_1997 = round(may15_escol_mean_1995 * exp(tasa95_00 * 2)),
    may15_escol_mean_1998 = round(may15_escol_mean_1995 * exp(tasa95_00 * 3)),
    may15_escol_mean_1999 = round(may15_escol_mean_1995 * exp(tasa95_00 * 4)),
    
    tasa00_05 = ((log(may15_escol_mean_2005 / may15_escol_mean_2000)) / 5),
    
    may15_escol_mean_2001 = round(may15_escol_mean_2000 * exp(tasa00_05 * 1)),
    may15_escol_mean_2002 = round(may15_escol_mean_2000 * exp(tasa00_05 * 2)),
    may15_escol_mean_2003 = round(may15_escol_mean_2000 * exp(tasa00_05 * 3)),
    may15_escol_mean_2004 = round(may15_escol_mean_2000 * exp(tasa00_05 * 4)),
    
    tasa05_10 = ((log(may15_escol_mean_2010 / may15_escol_mean_2005)) / 5),
    
    may15_escol_mean_2006 = round(may15_escol_mean_2005 * exp(tasa05_10 * 1)),
    may15_escol_mean_2007 = round(may15_escol_mean_2005 * exp(tasa05_10 * 2)),
    may15_escol_mean_2008 = round(may15_escol_mean_2005 * exp(tasa05_10 * 3)),
    may15_escol_mean_2009 = round(may15_escol_mean_2005 * exp(tasa05_10 * 4)),
    
    #esto me lo estoy inventando, necesito revisar si esto se puede
    may15_escol_mean_2011 = round(may15_escol_mean_2005 * exp(tasa05_10 * 6)),
    may15_escol_mean_2012 = round(may15_escol_mean_2005 * exp(tasa05_10 * 7)),
    may15_escol_mean_2013 = round(may15_escol_mean_2005 * exp(tasa05_10 * 8)),
    may15_escol_mean_2014 = round(may15_escol_mean_2005 * exp(tasa05_10 * 9)),
    may15_escol_mean_2015 = round(may15_escol_mean_2005 * exp(tasa05_10 * 10)),
    
    tasa95_00 = ((log(may5_indigena_2000 / may5_indigena_1995)) / 5),
    
    may5_indigena_1996 = round(may5_indigena_1995 * exp(tasa95_00 * 1)),
    may5_indigena_1997 = round(may5_indigena_1995 * exp(tasa95_00 * 2)),
    may5_indigena_1998 = round(may5_indigena_1995 * exp(tasa95_00 * 3)),
    may5_indigena_1999 = round(may5_indigena_1995 * exp(tasa95_00 * 4)),
    
    tasa00_05 = ((log(may5_indigena_2005 / may5_indigena_2000)) / 5),
    
    may5_indigena_2001 = round(may5_indigena_2000 * exp(tasa00_05 * 1)),
    may5_indigena_2002 = round(may5_indigena_2000 * exp(tasa00_05 * 2)),
    may5_indigena_2003 = round(may5_indigena_2000 * exp(tasa00_05 * 3)),
    may5_indigena_2004 = round(may5_indigena_2000 * exp(tasa00_05 * 4)),
    
    tasa05_10 = ((log(may5_indigena_2010 / may5_indigena_2005)) / 5),
    
    may5_indigena_2006 = round(may5_indigena_2005 * exp(tasa05_10 * 1)),
    may5_indigena_2007 = round(may5_indigena_2005 * exp(tasa05_10 * 2)),
    may5_indigena_2008 = round(may5_indigena_2005 * exp(tasa05_10 * 3)),
    may5_indigena_2009 = round(may5_indigena_2005 * exp(tasa05_10 * 4)),
    
    #esto me lo estoy inventando, necesito revisar si esto se puede
    may5_indigena_2011 = round(may5_indigena_2005 * exp(tasa05_10 * 6)),
    may5_indigena_2012 = round(may5_indigena_2005 * exp(tasa05_10 * 7)),
    may5_indigena_2013 = round(may5_indigena_2005 * exp(tasa05_10 * 8)),
    may5_indigena_2014 = round(may5_indigena_2005 * exp(tasa05_10 * 9)),
    may5_indigena_2015 = round(may5_indigena_2005 * exp(tasa05_10 * 10))
  )

educacion <- data.frame()

nombres <- 1995:2015
x = 1995

pb = txtProgressBar(min=1, max=length(nombres), style=3)
for(x in 1:length(nombres)) {
  
  tempo <- edu %>%
    select(Clave, ends_with(as.character(nombres[x]))) %>%
    mutate_all(funs(as.numeric)) %>%
    mutate(year = nombres[x])
  
  tempo$Clave = formatC(tempo$Clave, width=5, format="d", flag = "0") 
  
  names(tempo) = c("Clave", "mayor6_leer_esc", "may5_asistencia", "may5_nivel", "may18_prof", "may18_posg", "may15_escol_mean", "may5_indigena", "year")
  
  educacion = bind_rows(educacion, tempo)
  
  rm(tempo)
  setTxtProgressBar(pb, x)
  
}
rm(pb, edu)

educacion <- educacion %>%
  mutate(
    Clave = formatC(Clave, width = 5, format="d", flag="0"),
    muniYear = paste(Clave, year, sep = "_")
  )

summary(educacion)

as.data.frame(educacion, row.names = NULL)
write.csv(educacion, paste(out, "educacion.csv", sep = "/"), row.names = F)