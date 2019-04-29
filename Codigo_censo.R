rm(list=ls())
##Borra el ambiente
setwd("~")
#no pone directorio de trabajo

##Código para estimar la población a nivel municipal cada año
#Supuesto: se calcula la tasa exponencial y suponemos que cada año crece a ese ritmo

#Directorios
inp = "/Users/Emmanuel RB/Documents/tesis/input"
out = "/Users/Emmanuel RB/Documents/tesis/Datos"

library(readxl)
library(dplyr)

censo80 <- read_excel(paste(inp, "Censo_1980.xlsx", sep = "/"), sheet = 5, range = "a7:f2431")
censo80 <- censo80[,-1]
colnames(censo80) <- c("Entidad", "Municipio", "Pob_tot", "Hombres", "Mujeres")
censo80 <- censo80 %>%
  filter(Municipio != "Total") %>%
  mutate(
    cve_edo = substr(Entidad, 1, 2),
    cve_mun = substr(Municipio, 1, 3),
    clave = paste(cve_edo,cve_mun, sep = ""),
    year = 1980,
    muniYear = factor(paste(clave, year, sep = "_"))
  ) %>%
  select(clave, muniYear, Pob_tot)#, Hombres, Mujeres)

#Para corroborar utilicé levels(as.factor(variable)) en el censo original hay un "!"
colnames(censo80) <- c("Clave", "muniYear_80", "Total_80")#, "Hombres_80", "Mujeres_80")

censo90 <- read_excel(paste(inp, "Censo_1990.xlsx", sep = "/"), range = "a5:ac2675")
#para utilizar sólo los municipios revise con tail() y fui quitando los renglones vacios
censo90 <- censo90 %>%
  mutate(
    year = 1990,
    muniYear = factor(paste(Clave, year, sep = "_")),
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0)))))) %>%
  filter(edos == 0 & quit == 0) %>%
  select(Clave, muniYear, Total)#, Hombres, Mujeres)
colnames(censo90) <- c("Clave", "muniYear_90", "Total_90")#, "Hombres_90", "Mujeres_90")

censo95 <- read_excel(paste(inp, "Conteo_1995.xlsx", sep = "/"), range = "a5:ab2675")
censo95 <- censo95  %>%
  mutate(
    year = 1995,
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0))))),
    muniYear = factor(paste(Clave, year, sep = "_"))) %>% 
  filter(edos == 0 & quit == 0) %>% 
  select(Clave, muniYear, Total)#, Hombres, Mujeres)
colnames(censo95) <- c("Clave", "muniYear_95", "Total_95")#, "Hombres_95", "Mujeres_95")

censo00 <- read_excel(paste(inp, "Censo_2000.xlsx", sep = "/"), range = "a5:x2676")
censo00 <- censo00 %>%
  mutate(
    year = 2000,
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0))))),
    muniYear = factor(paste(Clave, year, sep = "_"))
  ) %>% 
  filter(edos == 0 & quit == 0) %>%
  select(Clave, muniYear, Total)#, Hombres, Mujeres)
colnames(censo00) <- c("Clave", "muniYear_00", "Total_00")#, "Hombres_00", "Mujeres_00" )

censo05 <- read_excel(paste(inp, "Conteo_2005.xlsx", sep = "/"), range = "a5:y2677")
censo05 <- censo05 %>% 
  mutate(
    year = 2005,
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0))))),
    muniYear = factor(paste(Clave, year, sep = "_"))
  ) %>%
  filter(edos == 0 & quit == 0) %>%
  select(Clave, muniYear, Total)#, Hombre, Mujer)
colnames(censo05) <- c("Clave", "muniYear_05", "Total_05")#, "Hombres_05", "Mujeres_05")

censo10 <- read_excel(paste(inp, "Censo_2010.xlsx", sep = "/"), range = "a5:z2677")
censo10 <- censo10 %>% 
  mutate(
    year = 2010,
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0))))),
    muniYear = factor(paste(Clave, year, sep = "_"))
  ) %>%
  filter(edos == 0 & quit == 0) %>%
  select(Clave, muniYear, Total)#, Hombre, Mujer)
colnames(censo10) <- c("Clave", "muniYear_10", "Total_10")#, "Hombres_10", "Mujeres_10")

int15 <- "/Users/Emmanuel RB/Documents/ICA/Tesis/input/Intercensal 2015"

ags15 <- read_excel(paste(int15, "01_poblacion_ags.xls", sep = "/"), sheet = 3, range = "a8:g1305")
colnames(ags15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
ags15 <- ags15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

bc15 <- read_excel(paste(int15, "01_poblacion_bc.xls", sep = "/"), sheet = 3, range = "a8:g657")
colnames(bc15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
bc15 <- bc15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

bcs15 <- read_excel(paste(int15, "01_poblacion_bcs.xls", sep = "/"), sheet = 3, range = "a8:g657")
colnames(bcs15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
bcs15 <- bcs15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

cam15 <- read_excel(paste(int15, "01_poblacion_cam.xls", sep = "/"), sheet = 3, range = "a8:g1299")
colnames(cam15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
cam15 <- cam15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

cdmx15 <- read_excel(paste(int15, "01_poblacion_cdmx.xls", sep = "/"), sheet = 3, range = "a8:g1839")
colnames(cdmx15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
cdmx15 <- cdmx15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

chih15 <- read_excel(paste(int15, "01_poblacion_chih.xls", sep = "/"), sheet = 3, range = "a8:g7283")
colnames(chih15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
chih15 <- chih15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

chis15 <- read_excel(paste(int15, "01_poblacion_chis.xls", sep = "/"), sheet = 3, range = "a8:g12730")
colnames(chis15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
chis15 <- chis15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

coah15 <- read_excel(paste(int15, "01_poblacion_coah.xls", sep = "/"), sheet = 3, range = "a8:g4204")
colnames(coah15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
coah15 <- coah15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

col15 <- read_excel(paste(int15, "01_poblacion_col.xls", sep = "/"), sheet = 3, range = "a8:g1197")
colnames(col15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
col15 <- col15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

dgo15 <- read_excel(paste(int15, "01_poblacion_dgo.xls", sep = "/"), sheet = 3, range = "a8:g4318")
colnames(dgo15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
dgo15 <- dgo15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

gro15 <- read_excel(paste(int15, "01_poblacion_gro.xls", sep = "/"), sheet = 3, range = "a8:g8854")
colnames(gro15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
gro15 <- gro15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

gto15 <- read_excel(paste(int15, "01_poblacion_gto.xls", sep = "/"), sheet = 3, range = "a8:g5080")
colnames(gto15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
gto15 <- gto15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

hgo15 <- read_excel(paste(int15, "01_poblacion_hgo.xls", sep = "/"), sheet = 3, range = "a8:g9190")
colnames(hgo15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
hgo15 <- hgo15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

jal15 <- read_excel(paste(int15, "01_poblacion_jal.xls", sep = "/"), sheet = 3, range = "a8:g13372")
colnames(jal15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
jal15 <- jal15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

mex15 <- read_excel(paste(int15, "01_poblacion_mex.xls", sep = "/"), sheet = 3, range = "a8:g13606")
colnames(mex15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
mex15 <- mex15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

mich15 <- read_excel(paste(int15, "01_poblacion_mich.xls", sep = "/"), sheet = 3, range = "a8:g12286")
colnames(mich15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
mich15 <- mich15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

mor15 <- read_excel(paste(int15, "01_poblacion_mor.xls", sep = "/"), sheet = 3, range = "a8:g3657")
colnames(mor15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
mor15 <- mor15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

nay15 <- read_excel(paste(int15, "01_poblacion_nay.xls", sep = "/"), sheet = 3, range = "a8:g2278")
colnames(nay15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
nay15 <- nay15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select

nl15 <- read_excel(paste(int15, "01_poblacion_nl.xls", sep = "/"), sheet = 3, range = "a8:g5602")
colnames(nl15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
nl15 <- nl15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

oax15 <- read_excel(paste(int15, "01_poblacion_oax.xls", sep = "/"), range = "a8:g60575")
colnames(oax15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
oax15 <- oax15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)
## En el caso de Oaxaca le tuve que quitar las otras hojas del excel, no abr?a y pens? que la raz?n ser?a el peso del archivo (8.17mb) al quitarle las otras 3 hojas funcion?

pue15 <- read_excel(paste(int15, "01_poblacion_pue.xls", sep = "/"), sheet = 3, range = "a8:g23345")
colnames(pue15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
pue15 <- pue15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

qro15 <- read_excel(paste(int15, "01_poblacion_qro.xls", sep = "/"), sheet = 3, range = "a8:g2037")
colnames(qro15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
qro15 <- qro15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15, Hombres_15, Mujeres_15)

qroo15 <- read_excel(paste(int15, "01_poblacion_qroo.xls", sep = "/"), sheet = 3, range = "a8:g1191")
colnames(qroo15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
qroo15 <- qroo15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

sin15 <- read_excel(paste(int15, "01_poblacion_sin.xls", sep = "/"), sheet = 3, range = "a8:g2055")
colnames(sin15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
sin15 <- sin15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

slp15 <- read_excel(paste(int15, "01_poblacion_slp.xls", sep = "/"), sheet = 3, range = "a8:g6370")
colnames(slp15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
slp15 <- slp15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

son15 <- read_excel(paste(int15, "01_poblacion_son.xls", sep = "/"), sheet = 3, range = "a8:g7745")
colnames(son15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
son15 <- son15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

tab15 <- read_excel(paste(int15, "01_poblacion_tab.xls", sep = "/"), sheet = 3, range = "a8:g1953")
colnames(tab15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
tab15 <- tab15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

tamps15 <- read_excel(paste(int15, "01_poblacion_tamps.xls", sep = "/"), sheet = 3, range = "a8:g4750")
colnames(tamps15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
tamps15 <- tamps15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

tlax15 <- read_excel(paste(int15, "01_poblacion_tlax.xls", sep = "/"), sheet = 3, range = "a8:g6538")
colnames(tlax15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
tlax15 <- tlax15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

ver15 <- read_excel(paste(int15, "01_poblacion_ver.xls", sep = "/"), sheet = 3, range = "a8:g22882")
colnames(ver15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
ver15 <- ver15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

yuc15 <- read_excel(paste(int15, "01_poblacion_yuc.xls", sep = "/"), sheet = 3, range = "a8:g11440")
colnames(yuc15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
yuc15 <- yuc15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)

zac15 <- read_excel(paste(int15, "01_poblacion_zac.xls", sep = "/"), sheet = 3, range = "a8:g6310")
colnames(zac15) <- c("estado", "municipio", "edad", "estimador", "Total_15", "Hombres_15", "Mujeres_15")
zac15 <- zac15 %>%
  filter(edad == "Total" & estimador == "Valor" & municipio != "Total") %>%
  mutate(
    cve_edo = factor(substr(estado, 1, 2)),
    cve_mun = factor(substr(municipio, 1, 3)), 
    Clave = paste(cve_edo, cve_mun, sep = "")
  ) %>%
  select(Clave, Total_15)#, Hombres_15, Mujeres_15)


censo2015 <- bind_rows(list(ags15, bc15, bcs15, cam15, cdmx15, 
                          chih15, chis15, coah15, col15, dgo15, 
                          gro15, gto15, hgo15, jal15, mex15, 
                          mich15, mor15, nay15, nl15, oax15, 
                          pue15, qro15, qroo15, sin15, slp15, 
                          son15, tab15, tamps15, tlax15, ver15, yuc15, zac15))

censo15 <- censo2015 %>%
  mutate(
    year = 2015,
    muniYear_15 = paste(Clave, year, sep = "_")
  ) %>%
  select(Clave, muniYear_15, Total_15#, Hombres_15, Mujeres_15) 
  )%>%
  filter(muniYear_15 != "NA_2015")

rm(ags15, bc15, bcs15, cam15, cdmx15, 
   chih15, chis15, coah15, col15, dgo15, 
   gro15, gto15, hgo15, jal15, mex15, 
   mich15, mor15, nay15, nl15, oax15, 
   pue15, qro15, qroo15, sin15, slp15, 
   son15, tab15, tamps15, tlax15, ver15, 
   yuc15, zac15)

pob <- full_join(censo80, censo90, by = "Clave")
pob <- full_join(pob, censo95, by = "Clave")
pob <- full_join(pob, censo00, by = "Clave")
pob <- full_join(pob, censo05, by = "Clave")
pob <- full_join(pob, censo10, by = "Clave")
pob <- full_join(pob, censo15, by = "Clave")

pob <- pob %>% 
  mutate(
    tasa80_90 = ((log(Total_90 / Total_80)) / 10),
    muniYear_81 = factor(paste(Clave, "1981", sep = "_")),
    Total_81 = round(Total_80 * (exp(tasa80_90 * 1))),
    
    muniYear_82 = factor(paste(Clave, "1982", sep = "_")),
    Total_82 = round(Total_80 * (exp(tasa80_90 * 2))),
    
    muniYear_83 = factor(paste(Clave, "1983", sep = "_")),
    Total_83 = round(Total_80 * (exp(tasa80_90 * 3))),
    
    muniYear_84 = factor(paste(Clave, "1984", sep = "_")),
    Total_84 = round(Total_80 * (exp(tasa80_90 * 4))),
    
    muniYear_85 = factor(paste(Clave, "1985", sep = "_")),
    Total_85 = round(Total_80 * (exp(tasa80_90 * 5))),
    
    muniYear_86 = factor(paste(Clave, "1986", sep = "_")),
    Total_86 = round(Total_80 * (exp(tasa80_90 * 6))),
    
    muniYear_87 = factor(paste(Clave, "1987", sep = "_")),
    Total_87 = round(Total_80 * (exp(tasa80_90 * 7))),
    
    muniYear_88 = factor(paste(Clave, "1988", sep = "_")),
    Total_88 = round(Total_80 * (exp(tasa80_90 * 8))),
    
    muniYear_89 = factor(paste(Clave, "1989", sep = "_")),
    Total_89 = round(Total_80 * (exp(tasa80_90 * 9))),
    
    tasa90_95 = ((log(Total_95 / Total_90)) / 10),
    muniYear_91 = factor(paste(Clave, "1991", sep = "_")),
    Total_91 = round(Total_90 * (exp(tasa90_95 * 1))),
    
    muniYear_92 = factor(paste(Clave, "1992", sep = "_")),
    Total_92 = round(Total_90 * (exp(tasa90_95 * 2))),
    
    muniYear_93 = factor(paste(Clave, "1993", sep = "_")),
    Total_93 = round(Total_90 * (exp(tasa90_95 * 3))),
    
    muniYear_94 = factor(paste(Clave, "1994", sep = "_")),
    Total_94 = round(Total_90 * (exp(tasa90_95 * 4))),
    
    tasa95_00 = ((log(Total_00 / Total_95)) / 10),
    muniYear_96 = factor(paste(Clave, "1996", sep = "_")),
    Total_96 = round(Total_90 * (exp(tasa95_00 * 1))),
    
    muniYear_97 = factor(paste(Clave, "1997", sep = "_")),
    Total_97 = round(Total_90 * (exp(tasa95_00 * 2))),
    
    muniYear_98 = factor(paste(Clave, "1998", sep = "_")),
    Total_98 = round(Total_90 * (exp(tasa95_00 * 3))),
    
    muniYear_99 = factor(paste(Clave, "1999", sep = "_")),
    Total_99 = round(Total_90 * (exp(tasa95_00 * 4))),
    
    tasa00_05 = ((log(Total_05 / Total_00)) / 10),
    muniYear_01 = factor(paste(Clave, "2001", sep = "_")),
    Total_01 = round(Total_00 * (exp(tasa00_05 * 1))),
    
    muniYear_02 = factor(paste(Clave, "2002", sep = "_")),
    Total_02 = round(Total_00 * (exp(tasa00_05 * 2))),
    
    muniYear_03 = factor(paste(Clave, "2003", sep = "_")),
    Total_03 = round(Total_00 * (exp(tasa00_05 * 3))),
    
    muniYear_04 = factor(paste(Clave, "2004", sep = "_")),
    Total_04 = round(Total_00 * (exp(tasa00_05 * 4))),
    
    tasa05_10 = ((log(Total_10 / Total_05)) / 10),
    muniYear_06 = factor(paste(Clave, "2006", sep = "_")),
    Total_06 = round(Total_05 * (exp(tasa05_10 * 1))),
    
    muniYear_07 = factor(paste(Clave, "2007", sep = "_")),
    Total_07 = round(Total_05 * (exp(tasa05_10 * 2))),
    
    muniYear_08 = factor(paste(Clave, "2008", sep = "_")),
    Total_08 = round(Total_05 * (exp(tasa05_10 * 3))),
    
    muniYear_09 = factor(paste(Clave, "2009", sep = "_")),
    Total_09 = round(Total_05 * (exp(tasa05_10 * 4))),
    
    tasa10_15 = ((log(Total_15 / Total_10)) / 10),
    muniYear_11 = factor(paste(Clave, "2011", sep = "_")),
    Total_11 = round(Total_10 * (exp(tasa10_15 * 1))),
    
    muniYear_12 = factor(paste(Clave, "2012", sep = "_")),
    Total_12 = round(Total_10 * (exp(tasa10_15 * 2))),
    
    muniYear_13 = factor(paste(Clave, "2013", sep = "_")),
    Total_13 = round(Total_10 * (exp(tasa10_15 * 3))),
    
    muniYear_14 = factor(paste(Clave, "2014", sep = "_")),
    Total_14 = round(Total_10 * (exp(tasa10_15 * 4)))
  ) %>% 
  select(-c(tasa80_90, tasa90_95, tasa95_00, tasa00_05, tasa05_10, tasa10_15))

censo80 <- censo80 %>%
  select(muniYear_80, Total_80) %>%
  setNames(c("muniYear", "Pob_Total"))

censo81 <- pob %>%
  select(ends_with("81")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo82 <- pob %>%
  select(ends_with("82")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo83 <- pob %>%
  select(ends_with("83")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo84 <- pob %>%
  select(ends_with("84")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo85 <- pob %>%
  select(ends_with("85")) %>%
  setNames(c("muniYear", "Pob_Total"))

censo86 <- pob %>%
  select(ends_with("86")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo87 <- pob %>%
  select(ends_with("87")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo88 <- pob %>%
  select(ends_with("88")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo89 <- pob %>%
  select(ends_with("89")) %>%
  setNames(c("muniYear", "Pob_Total"))

censo90 <- censo90 %>%
  select(muniYear_90, Total_90) %>%
  setNames(c("muniYear", "Pob_Total"))

censo91 <- pob %>%
  select(ends_with("91")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo92 <- pob %>%
  select(ends_with("92")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo93 <- pob %>%
  select(ends_with("93")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo94 <- pob %>%
  select(ends_with("94")) %>%
  setNames(c("muniYear", "Pob_Total"))

censo95 <- censo95 %>%
  select(muniYear_95, Total_95) %>%
  setNames(c("muniYear", "Pob_Total"))

censo96 <- pob %>%
  select(ends_with("96")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo97 <- pob %>%
  select(ends_with("97")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo98 <- pob %>%
  select(ends_with("98")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo99 <- pob %>%
  select(ends_with("99")) %>%
  setNames(c("muniYear", "Pob_Total"))

censo00 <- censo00 %>%
  select(muniYear_00, Total_00) %>%
  setNames(c("muniYear", "Pob_Total"))

censo01 <- pob %>%
  select(ends_with("01")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo02 <- pob %>%
  select(ends_with("02")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo03 <- pob %>%
  select(ends_with("03")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo04 <- pob %>%
  select(ends_with("04")) %>%
  setNames(c("muniYear", "Pob_Total"))

censo05 <- censo05 %>%
  select(muniYear_05, Total_05) %>%
  setNames(c("muniYear", "Pob_Total"))

censo06 <- pob %>%
  select(ends_with("06")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo07 <- pob %>%
  select(ends_with("07")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo08 <- pob %>%
  select(ends_with("08")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo09 <- pob %>%
  select(ends_with("09")) %>%
  setNames(c("muniYear", "Pob_Total"))

censo10 <- censo10 %>%
  select(muniYear_10, Total_10) %>%
  setNames(c("muniYear", "Pob_Total"))

censo11 <- pob %>%
  select(ends_with("11")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo12 <- pob %>%
  select(ends_with("12")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo13 <- pob %>%
  select(ends_with("13")) %>%
  setNames(c("muniYear", "Pob_Total"))
censo14 <- pob %>%
  select(ends_with("14")) %>%
  setNames(c("muniYear", "Pob_Total"))

censo15 <- censo15 %>%
  select(muniYear_15, Total_15) %>%
  setNames(c("muniYear", "Pob_Total"))

censo <- bind_rows(list(censo80, censo81, censo82, censo83, censo84, 
                        censo85, censo86, censo87, censo88, censo89, 
                        censo90, censo91, censo92, censo93, censo94, 
                        censo95, censo96, censo97, censo98, censo99, 
                        censo00, censo01, censo02, censo03, censo04, 
                        censo05, censo06, censo07, censo08, censo09, 
                        censo10, censo11, censo12, censo13, censo14, censo15))

rm(censo80, censo81, censo82, censo83, censo84, censo85, censo86, censo87, 
   censo88, censo89, censo90, censo91, censo92, censo93, censo94, censo95,
   censo96, censo97, censo98, censo99, censo00, censo01, censo02, censo03, 
   censo04, censo05, censo06, censo07, censo08, censo09, censo10, censo11, 
   censo12, censo13, censo14, censo15, pob, censo2015)

summary(censo)
View(censo[is.na(censo$Pob_Total),])

as.data.frame(censo, row.names = NULL)
write.csv(censo, paste(out, "censo.csv", sep = "/"), row.names = F, fileEncoding ="UTF-8")
