rm(list=ls())
setwd("~")

library(plyr)
library(dplyr)
library(readxl)
library(stringr)

inp = "/Users/Emmanuel RB/Documents/tesis/input"
out = "/Users/Emmanuel RB/Documents/tesis/Datos"

####Elecciones 2014####
##Nayarit
nay <- read.csv(paste(inp, 'tabula-nay14pdf.csv', sep = '/'), header = F, stringsAsFactors = F)
colnames(nay) <- c("Municipio", "PAN", "PRI_PVEM_PNA", "PRD", "PT", "PRS", "MC", "INDEP", "noreg", "nulos")

nay <- nay %>%
  mutate(
    state = as.character("18"),
    muni = as.factor(c("001", "002", "003", "020", "004", "005", "006", "007", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "008", "019")),
    year = 2014,
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_"),
    PAN = str_replace_all(PAN, c(" " = "." , "," = "" )),
    PRI_PVEM_PNA = str_replace_all(PRI_PVEM_PNA, c(" " = "." , "," = "" )),
    PRD = str_replace_all(PRD, c(" " = "." , "," = "" )),
    PT = str_replace_all(PT, c(" " = "." , "," = "" )),
    PRS = str_replace_all(PRS, c(" " = "." , "," = "" )),
    MC = str_replace_all(MC, c(" " = "." , "," = "" )),
    INDEP = str_replace_all(INDEP, c(" " = "." , "," = "" )),
    noreg = str_replace_all(noreg, c(" " = "." , "," = "" )),
    nulos = str_replace_all(nulos, c(" " = "." , "," = "" ))
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI_PVEM_PNA, PRD, PT, PRS, MC, INDEP, noreg, nulos) %>% 
  arrange(muniYear)

nay <- nay %>%
  mutate(
    PAN = as.numeric(PAN), 
    PRI_PVEM_PNA = as.numeric(PRI_PVEM_PNA), 
    PRD = as.numeric(PRD), 
    PT = as.numeric(PT), 
    PRS = as.numeric(PRS), 
    MC = as.numeric(MC), 
    INDEP = as.numeric(INDEP), 
    noreg = as.numeric(noreg), 
    nulos = as.numeric(nulos)
  )

gana <- select(nay, PAN, PRI_PVEM_PNA, PRD, PT, PRS, MC, INDEP, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
nay14 <- bind_cols(nay, Winner2)

rm(nay, Winner2)

####Elecciones 2015####
####BCS####
la_paz <- read_excel(paste(inp, "LA_PAZ.xlsx", sep = "/"), range = "c374:k375")
colnames(la_paz) <- (c("TOTAL", "PAN", "PRI_PVEM_PNA", "PRD_MC_PT", "Morena", "Humanista", "PES", "noreg", "nulos"))

loreto <- read_excel(paste(inp, "loreto.xlsx", sep = "/"), range = "c29:k30")
colnames(loreto) <- (c("TOTAL", "PAN", "PRI_PVEM_PNA", "PRD_MC_PT", "Morena", "Humanista", "PES", "noreg", "nulos"))

mulege <- read_excel(paste(inp, "mulege.xlsx", sep = "/"), range = "c86:k87")
colnames(mulege) <- (c("TOTAL", "PAN", "PRI_PVEM_PNA", "PRD_MC_PT", "Morena", "Humanista", "PES", "noreg", "nulos"))

comondu <- read_excel(paste(inp, "comondu.xlsx", sep = "/"), range = "c111:k112")
colnames(comondu) <- (c("TOTAL", "PAN", "PRI_PVEM_PNA", "PRD_MC_PT", "Morena", "Humanista", "PES", "noreg", "nulos"))

los_cabos <- read_excel(paste(inp, "los_cabos.xlsx", sep = "/"), range = "c324:k325")
colnames(los_cabos) <- (c("TOTAL", "PAN", "PRI_PVEM_PNA", "PRD_MC_PT", "Morena", "Humanista", "PES", "noreg", "nulos"))

bcs15 <- bind_rows(list(la_paz, loreto, comondu, los_cabos, mulege))
bcs15 <- bcs15 %>%
  mutate(
    year = 2015,
    state = as.character("03"),
    muni = as.factor(c("003", "009", "002", "001", "008")),
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_"),
    Winner2 = colnames(bcs15)[apply(bcs15, 1, which.max)]
  ) %>%
  arrange(muniYear) %>%
  select(muniYear, state, year, muni, PAN, PRI_PVEM_PNA, PRD_MC_PT, Morena, Humanista, PES, noreg, nulos, Winner2)
rm(la_paz, loreto, comondu, los_cabos, mulege)

####Campeche####
camp <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 1, range = "e362:y363")
camp <- select(camp, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(camp) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

calkini <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 2, range = "e89:y90")
calkini <- select(calkini, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(calkini) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

carmen <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 3, range = "e299:y300")
carmen <- select(carmen, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(carmen) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

champo <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 4, range = "e125:y126")
champo <- select(champo, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(champo) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

hecel <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 5, range = "e49:y50")
hecel <- select(hecel, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(hecel) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

hopel <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 6, range = "e64:y65")
hopel <- select(hopel, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(hopel) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

pali <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 7, range = "e26:y27")
pali <- select(pali, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(pali) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

tena <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 8, range = "e27:y28")
tena <- select(tena, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(tena) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

escar <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 9, range = "e83:y84")
escar <- select(escar, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(escar) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

cande <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 10, range = "e69:y70")
cande <- select(cande, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(cande) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

calak <- read_excel(paste(inp, "RESULTADOS POR CASILLA - AYUNTAMIENTOS - CANDIDATO.xlsx", sep = "/"), sheet = 11, range = "e50:y51")
calak <- select(calak, X__1, X__3, X__5, X__7, X__9, X__11, X__13, X__15, X__17, X__21)
colnames(calak) <- c("PAN", "PRD", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "nulos")

camp15 <- bind_rows(list(calak, calkini, camp, cande, carmen, champo, escar, hecel, hopel, pali, tena))
rm(calak, calkini, camp, cande, carmen, champo, escar, hecel, hopel, pali, tena)
camp15 <- camp15 %>%
  mutate(
    year = 2015, 
    state = as.character("04"),
    muni = as.factor(c("002", "001", "003", "004", "005", "006", "007", "008", "009", "011", "010")),
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_"),
    Winner2 = colnames(camp15)[apply(camp15, 1, which.max)]
  ) %>%
  select(muniYear, state, year, muni, PAN, PRD, PT, MC, PNA, Morena, Humanista, PES, PRI_PVEM, nulos, Winner2) %>%
  arrange(muniYear)

####Colima####
clave <- read.csv(paste(inp, "tabula_Colima.csv", sep = "/"), fileEncoding = "UTF-8", header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"))
col <- read_excel(paste(inp, "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sep = "/"), sheet = 1, range = "b6:r16")
col <- select(col, -c("SECCIONES", "TOTAL CASILLAS", "LISTA NOMINAL"))
colnames(col) <- c("MUNICIPIO", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "Coalicion", "noreg", "nulos")
col <- col %>%
  mutate(
    PRD = as.numeric(PRD),
    PVEM = as.numeric(PVEM),
    PNA = as.numeric(PNA),
    Humanista = as.numeric(Humanista),
    PES = as.numeric(PES),
    Coalicion = as.numeric(Coalicion),
    state = as.character("06"),
    year = 2015
  ) %>%
  left_join(clave, col, by = "MUNICIPIO")
col$PRI_PVEM_PNA <- ifelse(is.na(col$Coalicion), NA, rowSums(col[c("PRI", "PVEM", "PNA", "Coalicion")], na.rm = T))
col15 <- col %>%
  mutate(
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PVEM, PNA, PRI_PVEM_PNA, PRD, PT, MC, Morena, Humanista, PES, noreg, nulos)

gana <- select(col15, muniYear, PAN, PRI, PVEM, PNA, PRI_PVEM_PNA, PRD, PT, MC, Morena, Humanista, PES, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
col15 <- bind_cols(col15, Winner2)

rm(clave, col, gana, Winner2)

####Chiapas####
clave <- read.csv(paste(inp, "tabula-Chiapas.csv", sep = "/"), header = F, col.names = c("muni", "MUNICIPIO"), colClasses = "factor")
add <- data.frame("muni" = as.factor(c("036", "038", "053", "055", "056", "071", "113", "114", "115", "116", "118", NA, "123", "124", "122")), "MUNICIPIO" = c("LA GRANDEZA", "HUIXTAN","MAZAPA DE MADERO", "METAPA DE DOMINGUEZ", "MITONTIC", "VILLACOMALTITLAN", "ALDAMA", "BENEMERITO DE LAS AMERICAS", "MARAVILLA TENEJAPA", "MARQUES DE COMILLAS", "SAN ANDRES DURAZNAL", "BELISARIO DOMINGUEZ", "EMILIANO ZAPATA", "MEZCALAPA", "EL PARRAL"))
clave <- bind_rows(clave, add)
nom <- read.csv(paste(inp, "tabula-nombres chiapas.csv", sep = "/"), header = F, col.names = "MUNICIPIO")
vot <- read.csv(paste(inp, "tabula-resultados chiapas.csv", sep = "/"), header = F, col.names = c("PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "CU", "Morena", "Humanista", "PES", "Mover", "INDEP1", "noreg", "nulos"))
chis <- bind_cols(list(nom,vot))
chis <- left_join(chis, clave, by = "MUNICIPIO")
chis[is.na(chis$muni),]

chis15 <- chis %>%
  filter(!is.na(muni)) %>%
  mutate(
    PAN = str_replace_all(PAN, c(" " = "." , "," = "" )),
    PRI = str_replace_all(PRI, c(" " = "." , "," = "" )),
    PRD = str_replace_all(PRD, c(" " = "." , "," = "" )),
    PT = str_replace_all(PT, c(" " = "." , "," = "" )),
    PVEM = str_replace_all(PVEM, c(" " = "." , "," = "" )),
    MC = str_replace_all(MC, c(" " = "." , "," = "" )),
    PNA = str_replace_all(PNA, c(" " = "." , "," = "" )),
    CU = str_replace_all(CU, c(" " = "." , "," = "" )),
    Morena = str_replace_all(Morena, c(" " = "." , "," = "" )),
    Humanista = str_replace_all(Humanista, c(" " = "." , "," = "" )),
    PES = str_replace_all(PES, c(" " = "." , "," = "" )),
    Mover = str_replace_all(Mover, c(" " = "." , "," = "" )),
    INDEP1 = str_replace_all(INDEP1, c(" " = "." , "," = "" )),
    noreg = as.numeric(noreg),
    nulos = str_replace_all(nulos, c(" " = "." , "," = "" )),
    state = as.character("07"),
    year = 2015,
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PT, PVEM, MC, PNA, CU, Morena, Humanista, PES, Mover, INDEP1, noreg, nulos)

chis15 <- chis15 %>%
  mutate(
    PAN = as.numeric(PAN),
    PRI = as.numeric(PRI),
    PRD = as.numeric(PRD),
    PT = as.numeric(PT),
    PVEM = as.numeric(PVEM),
    MC = as.numeric(MC),
    PNA = as.numeric(PNA),
    CU = as.numeric(CU),
    Morena = as.numeric(Morena),
    Humanista = as.numeric(Humanista),
    PES = as.numeric(PES),
    Mover = as.numeric(Mover),
    INDEP1 = as.numeric(INDEP1),
    nulos = as.numeric(nulos)
  )

gana <- select(chis15,PAN, PRI, PRD, PT, PVEM, MC, PNA, CU, Morena, Humanista, PES, Mover, INDEP1, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
chis15 <- bind_cols(chis15, Winner2)

chis15 <- filter(chis15, Winner2 != "noreg")
rm(clave, chis, vot, nom, add)

####CDMX####
cdmx <- read_excel(paste(inp, "jd_del_2015.xlsx", sep = "/"), range = "a1:ab17")
colnames(cdmx) <- c("DELEGACIÓN", "SEC", "CAS", "LN", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "INDEP1", "INDEP2", "INDEP3", "INDEP4", "INDEP5", "INDEP6", "INDEP7", "PRI_PVEM", "PRD_PT_PNA", "PRD_PT", "noreg", "nulos", "Total", "Participacion")
clave <- read.csv(paste(inp, "tabula-cdmx.csv", sep = "/"), header = F, col.names = c("muni", "DELEGACIÓN"), colClasses = "factor", fileEncoding = "UTF-8")
add <- data.frame("muni" = as.factor(c("011")), "DELEGACIÓN" = c("Tlahuác"))
clave <- bind_rows(clave, add)
cdmx <- cdmx %>%
  left_join(clave, cdmx, by = "DELEGACIÓN") %>%
  mutate(
    year = 2015,
    state = as.character("09"), 
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_"),
    PT = str_replace_all(PT, " ", ""),
    MC = str_replace_all(MC, " ", ""),
    PNA = str_replace_all(PNA, " ", ""),
    Morena = str_replace_all(Morena, " ", ""),
    Humanista = str_replace_all(Humanista, " ", ""),
    PES = str_replace_all(PES, " ", ""),
    INDEP3 = str_replace_all(INDEP3, " ", ""), 
    INDEP5 = str_replace_all(INDEP5, " ", ""),
    PRI_PVEM = str_replace_all(PRI_PVEM, " ", ""),
    PRD_PT_PNA = str_replace_all(PRD_PT_PNA, " ", ""),
    PRD_PT = str_replace_all(PRD_PT, " ", ""),
    noreg = str_replace_all(noreg, " ", ""),
    nulos = str_replace_all(nulos, " ", "")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PVEM, PT, MC, PNA, Morena, Humanista, PES, INDEP1, INDEP2, INDEP3, INDEP4, INDEP5, INDEP6, INDEP7, PRI_PVEM, PRD_PT_PNA, PRD_PT, noreg, nulos)

cdmx <- cdmx %>%
  mutate(
    PT = as.numeric(PT),
    MC = as.numeric(MC),
    PNA = as.numeric(PNA),
    Morena = as.numeric(Morena), 
    Humanista = as.numeric(Humanista),
    PES = as.numeric(PES), 
    INDEP3 = as.numeric(INDEP3),
    INDEP5 = as.numeric(INDEP5),
    PRI_PVEM = as.numeric(PRI_PVEM),
    PRD_PT_PNA = as.numeric(PRD_PT_PNA),
    PRD_PT = as.numeric(PRD_PT),
    noreg = as.numeric(noreg),
    nulos = as.numeric(nulos)
    )
  
cdmx15 <- cdmx %>%
  mutate(
    S_PRI_PVEM = ifelse(PRI_PVEM != 0, rowSums(cdmx[c("PRI", "PVEM", "PRI_PVEM")]), PRI_PVEM),
    S_PRD_PT_PNA = ifelse(PRD_PT_PNA != 0, rowSums(cdmx[c("PRD", "PT", "PNA", "PRD_PT_PNA")]), PRD_PT_PNA),
    S_PRD_PT = ifelse(PRD_PT != 0, rowSums(cdmx[c("PRD", "PT", "PRD_PT")]), PRD_PT)
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PVEM, PT, MC, PNA, Morena, Humanista, PES, INDEP1, INDEP2, INDEP3, INDEP4, INDEP5, INDEP6, INDEP7, S_PRI_PVEM, S_PRD_PT_PNA, S_PRD_PT, noreg, nulos)
colnames(cdmx15) <- c("muniYear", "state", "year", "muni", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "INDEP1", "INDEP2", "INDEP3", "INDEP4", "INDEP5", "INDEP6", "INDEP7", "PRI_PVEM", "PRD_PT_PNA", "PRD_PT", "noreg", "nulos")
cdmx15$Winner2 <- colnames(cdmx15)[apply(cdmx15, 1, which.max)]
rm(cdmx, clave, add)

####Guanajuato####
gto <- read_excel(paste(inp, "2015-ayuntamiento_municipio_20171006.xlsx", sep = "/"))
colnames(gto) <- c("muni", "UBICACIÓN", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "Humanista", "PES", "INDEP1", "PRI_PVEM_PNA", "PRI_PVEM", "PRI_PNA", "PVEM_PNA", "PNA", "noreg", "NULO", "NOMINAL", "VALIDA", "TOTAL", "% PARTICIPACIÓN")
gto15 <- gto %>%
  mutate(
    muni = as.factor(muni),
    muni = formatC(muni, width = 3, format = "d", flag = "0"),
    state = as.character("11"),
    mpo = paste(state, muni, sep = ""),
    year = 2015,
    muniYear = paste(mpo, year, sep = "_"),
    coal = ifelse(is.na(PRI_PVEM_PNA), NA, rowSums(gto[c("PRI", "PVEM", "PRI_PVEM_PNA", "PRI_PVEM", "PRI_PNA", "PVEM_PNA", "PNA")], na.rm = T))
  ) %>% 
  select(muniYear, state, year, muni, PRI, PVEM, PNA, PAN, PRD, PT, MC, MORENA, Humanista, PES, INDEP1, coal, noreg, NULO)
rm(gto)
colnames(gto15) <- c("muniYear", "state", "year", "muni", "PRI", "PVEM", "PNA", "PAN", "PRD", "PT", "MC", "MORENA", "Humanista", "PES", "INDEP1", "PRI_PVEM_PNA", "noreg", "NULO")

gana <- select(gto15, PRI, PVEM, PNA, PAN, PRD, PT, MC, MORENA, Humanista, PES, INDEP1, PRI_PVEM_PNA, noreg, NULO)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
gto15 <- bind_cols(gto15, Winner2)

rm(got, gana, Winner2)

####Guerrero####
gro <- read_excel(paste(inp, "Resultados de la elección de Ayuntamientos por casilla 2014-2015.xlsx", sep = "/"), range = "a7:z4893")
colnames(gro) <- c("Distrito", "MUNICIPIO", "SECCION", "CASILLA", "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "Morena", "Humanista", "PES", "PPG", "PRI_PVEM", "PRD_PT", "PRI_PVEM_PNA", "PRI_PNA", "PVEM_PNA", "INDEP1", "noreg", "nulos", "TOTALES", "NOMINAL", "Participacion")
clave = read.csv(paste(inp, "tabula-guerrero.csv", sep = "/"), header = F, col.names = c("muni", "MUNICIPIO"), colClasses = "factor")
add <- data.frame("muni" = as.factor(c("005", "024", "026", "031", "043", "065")), "MUNICIPIO" = c("ALPOYECA", "CUALAC", "CUETZALA DEL PROGRESO", "GENERAL CANUTO A. NERI", "METLATONOC", "TLALIXTAQUILLA DE MALDONADO"))
clave <- bind_rows(clave, add)
gro <- gro %>%
  select(MUNICIPIO, PAN, PRI, PRD, PT, PVEM, MC, PNA, Morena, Humanista, PES, PPG, PRI_PVEM, PRD_PT, PRI_PVEM_PNA, PRI_PNA, PVEM_PNA, INDEP1, noreg, nulos)%>%
  filter(substr(MUNICIPIO, 1, 5) == "Total") %>%
  mutate(
    MUNICIPIO  = gsub("Total ", "", MUNICIPIO)
  ) %>%
  left_join(clave, gro, by = "MUNICIPIO")
gro15 <- gro %>% 
  mutate(
    coal_PRD = rowSums(gro[c("PRD", "PT", "PRD_PT")], na.rm = T),
    coal_PRI = rowSums(gro[c("PRI", "PVEM", "PNA", "PRI_PVEM", "PRI_PVEM_PNA", "PRI_PNA", "PVEM_PNA")], na.rm = T), 
    state = as.character("12"),
    mpo = paste(state, muni, sep = ""),
    year = 2015,
    muniYear = paste(mpo, year, sep = "_")
  ) %>% 
  select(muniYear, state, year, muni, PRI, PVEM, PNA, PRD, PT, coal_PRI, coal_PRD, PAN, PT, MC, Morena, Humanista, PES, PPG, INDEP1, noreg, nulos)
colnames(gro15) <- c("muniYear", "state", "year", "muni", "PRI", "PVEM", "PNA", "PRD", "PT", "PRI_PVEM_PNA", "PRD_PT", "PAN", "MC", "Morena", "Humanista", "PES", "PPG", "INDEP1", "noreg", "nulos")

gana <- select(gro15, PRI, PVEM, PNA, PRD, PT, PRI_PVEM_PNA, PRD_PT, PAN, PT, MC, Morena, Humanista, PES, PPG, INDEP1, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
gro15 <- bind_cols(gro15, Winner2)

rm(gro, clave, add, gana, Winner2)

####Jalisco####
jal <- read_excel(paste(inp, "Jalisco_ayttos_15.xlsx", sep = "/"), range = "a1:y126")
colnames(jal) <- c("MUNICIPIO", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM", "PAN_PRD", "JPKA", "JFSP", "JOSM", "JZC", "GCP", "S_PAN_PRD", "S_PRI_PVEM", "noreg", "nulos", "Boletas", "VotosTotales", "VotosValidos")
clave <- read.csv(paste(inp, "tabula-jalisco.csv", sep = "/"), fileEncoding = "UTF-8", header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"))
clave$MUNICIPIO <- toupper(clave$MUNICIPIO)
add <- data.frame("muni" = as.factor(c("026", "079", "045", "048", "057", "071", "098", "080", "081", "097")), "MUNICIPIO" = c("CONCEPCION DE BUENOS AIRES", "GOMEZ FARÍAS", "IXTLAHUACÁN DEL RIO", "JESÚS MARIA", "MANZANILLA DE LA PAZ", "SAN CRISTOBAL DE LA BARRANCA", "SAN PEDRO TLAQUEPAQUE", "SAN SEBASTIAN DEL OESTE", "SANTA MARIA DE LOS ÁNGELES", "TLAJOMULCO DE ZUÑIGA"))
clave <- bind_rows(clave, add)
jal15 <- jal %>% 
  left_join(clave, jal, by = "MUNICIPIO") %>%
  select(MUNICIPIO, muni, PAN, PRI, PRD, PVEM, PT, MC, PNA, Morena, Humanista, PES, JPKA, JFSP, JOSM, JZC, GCP, S_PAN_PRD, S_PRI_PVEM, noreg, nulos) %>%
  mutate(
    PAN = as.numeric(PAN), 
    PRI = as.numeric(PRI), 
    PRD = as.numeric(PRD), 
    PVEM = as.numeric(PVEM),
    PT = as.numeric(PT),
    MC = as.numeric(MC),
    PNA = as.numeric(PNA), 
    Morena = as.numeric(Morena), 
    Humanista = as.numeric(Humanista), 
    PES = as.numeric(PES), 
    JPKA = as.numeric(JPKA), 
    JFSP = as.numeric(JFSP), 
    JOSM = as.numeric(JOSM), 
    JZC = as.numeric(JZC), 
    GCP = as.numeric(GCP), 
    S_PAN_PRD = as.numeric(S_PAN_PRD), 
    S_PRI_PVEM = as.numeric(S_PRI_PVEM), 
    year = 2015,
    state = as.character("14"),
    mpo = as.factor(paste(state, muni, sep = "")),
    muniYear = as.factor(paste(mpo, year, sep = "_"))
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PVEM, S_PAN_PRD, S_PRI_PVEM, PT, MC, PNA, Morena, Humanista, PES, JPKA, JFSP, JOSM, JZC, GCP, noreg, nulos)

colnames(jal15) <- c("muniYear", "state", "year", "muni", "PAN", "PRI", "PRD", "PVEM", "PAN_PRD", "PRI_PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "JPKA", "JFSP", "JOSM", "JZC", "GCP", "noreg", "nulos")

gana <- select(jal15, PAN, PRI, PRD, PVEM, PAN_PRD, PRI_PVEM, PT, MC, PNA, Morena, Humanista, PES, JPKA, JFSP, JOSM, JZC, GCP, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
jal15 <- bind_cols(jal15, Winner2)

rm(jal, clave, add, gana, Winner2)

####EDOMEX####
mex <- read_excel(paste(inp, "Computo_MUNICIPAL_2015.xlsx", sep = "/"), range = "b7:z132")
colnames(mex) <- c("MUNICIPIO", "LISTA_NOMINAL", "TOTAL_CASILLAS", "TOTAL_CASILLAS_CAPTURADAS", "Porcentaje_Captura", "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "Morena", "Humanista", "PES", "PFD", "PRI_PVEM_PNA", "PRI_PVEM", "PRI_PNA", "PVEM_PNA", "PAN_PT", "INDEP1", "INDEP2", "noreg", "nulos")
clave <- read.csv(paste(inp, "tabula-edomex.csv", sep = "/"), header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"))
add <- data.frame("muni" = as.factor(c("001","020", "057", "058", "071", "077", "078", "107", "116")), "MUNICIPIO" = c("ACAMBAY", "COACALCO DE BERRIOZABAL", "NAUCALPAN DE JUAREZ 1", "NEZAHUALCOYOTL 2", "POLOTITLAN", "SAN SIMON DE GUERRERO", "SANTO TOMAS", "TONATICO", "ZACAZONAPAN"))
clave <- bind_rows(clave, add)
mex15 <- mex %>%
  left_join(clave, mex, by = "MUNICIPIO") %>%
  mutate(
    state = as.character("15"), 
    mpo = paste(state, muni, sep = ""),
    year = 2015,
    muniYear = paste(mpo, year, sep = "_"),
    coal_PRI = ifelse(is.na(PRI_PVEM_PNA), NA, rowSums(mex[c("PRI", "PVEM", "PNA", "PRI_PVEM", "PRI_PVEM_PNA", "PRI_PNA", "PVEM_PNA")], na.rm = T)), 
    coal_PAN = ifelse(is.na(PAN_PT), NA, rowSums(mex[c("PAN", "PT", "PAN_PT")], na.rm = T))
  )%>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PT, PVEM, MC, PNA, Morena, Humanista, PES, PFD, coal_PRI, coal_PAN, INDEP1, INDEP2, noreg, nulos)
colnames(mex15) <- c("muniYear", "state", "year", "muni", "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "Morena", "Humanista", "PES", "PFD", "PRI_PVEM_PNA", "PAN_PT", "INDEP1", "INDEP2", "noreg", "nulos")

gana <- select(mex15, PAN, PRI, PRD, PT, PVEM, MC, PNA, Morena, Humanista, PES, PFD, PRI_PVEM_PNA, PAN_PT, INDEP1, INDEP2, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
mex15 <- bind_cols(mex15, Winner2)

rm(mex, add, clave, gana, Winner2)

####Michoacan####
mich <- read_excel(paste(inp, "computos_municipales_2015.xlsx", sep = "/"), range = "d3:bv115")
mich <- select(mich, -c("X__13", "X__14", "X__15", "X__16", "X__17", "X__18", "X__19", "X__20", "X__21", "X__22", "X__23", "X__24", "X__25", "X__26", "X__27", "X__28", "X__29", "X__30", "X__31", "X__32", "X__33", "X__34", "X__35", "X__36", "X__37", "X__38", "X__39", "X__40", "X__41", "X__42", "X__43", "X__44", "X__45"))
colnames(mich) <- c("MUNICIPIO" ,"PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "Morena", "Humanista", "PES", "INDEP", "PAN_PRI_PRD_PNA_Humanista_PES", "PRD_PT_PAN_PNA_Humanista", "PRD_PTPAN_PNA_PES", "PRD_PT_PNA_Humanista", "PRD_PT_PNA_PES", "PAN_PRI_PVEM", "PAN_PRD_PT", "PRD_PT_PNA", "PRD_PT_Humanista", "PRD_PT_PES", "PRD_PNA_PES", "PT_PNA_Humanista", "PT_Humanista_PES", "PAN_PRD", "PAN_PT", "PAN_MC", "PAN_Humanista", "PRI_PVEM", "PRD_PT", "PRD_PNA", "PRD_PES", "PT_MC", "PT_Humanista", "PT_PES", "noreg", "nulos")
clave <- read.csv(paste(inp, "tabula-mich.csv", sep = "/"), fileEncoding = "UTF-8", header = F, col.names = c("muni", "MUNICIPIO"), colClasses = "factor")
clave$MUNICIPIO <- toupper(clave$MUNICIPIO)
add <- data.frame("muni" = as.factor(c("007", "074", "037", "056", "092", "015", "096")), "MUNICIPIO" = c("ÁPORO", "RÉGULES", "HUANÍQUEO", "NAHUÁTZEN", "TIQUICHEO", "COALCOMÁN", "TUMBISCATIO"))
clave <- bind_rows(clave, add)
mich15 <- mich %>%
  left_join(clave, mich, by = "MUNICIPIO") %>%
  mutate(
    year = 2015,
    state = as.character("16"),
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_"),
    PAN_PRI_PRD_PNA_Humanista_PES = as.numeric(PAN_PRI_PRD_PNA_Humanista_PES), 
    PRD_PT_PAN_PNA_Humanista = as.numeric(PRD_PT_PAN_PNA_Humanista), 
    PRD_PTPAN_PNA_PES = as.numeric(PRD_PTPAN_PNA_PES), 
    PRD_PT_PNA_Humanista = as.numeric(PRD_PT_PNA_Humanista), 
    PRD_PT_PNA_PES = as.numeric(PRD_PT_PNA_PES), 
    PAN_PRI_PVEM = as.numeric(PAN_PRI_PVEM), 
    PAN_PRD_PT = as.numeric(PAN_PRD_PT), 
    PRD_PT_PNA = as.numeric(PRD_PT_PNA), 
    PRD_PT_Humanista = as.numeric(PRD_PT_Humanista), 
    PRD_PT_PES = as.numeric(PRD_PT_PES), 
    PRD_PNA_PES = as.numeric(PRD_PNA_PES), 
    PT_PNA_Humanista = as.numeric(PT_PNA_Humanista), 
    PT_Humanista_PES = as.numeric(PT_Humanista_PES), 
    PAN_PRD = as.numeric(PAN_PRD), 
    PAN_PT = as.numeric(PAN_PT), 
    PAN_MC = as.numeric(PAN_MC), 
    PAN_Humanista = as.numeric(PAN_Humanista), 
    PRI_PVEM = as.numeric(PRI_PVEM), 
    PRD_PT = as.numeric(PRD_PT), 
    PRD_PNA = as.numeric(PRD_PNA), 
    PRD_PES = as.numeric(PRD_PES), 
    PT_MC = as.numeric(PT_MC), 
    PT_Humanista = as.numeric(PT_Humanista), 
    PT_PES = as.numeric(PT_PES)
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PT, PVEM, MC, PNA, Morena, Humanista, PES, INDEP, PAN_PRI_PRD_PNA_Humanista_PES, PRD_PT_PAN_PNA_Humanista, PRD_PTPAN_PNA_PES, PRD_PT_PNA_Humanista, PRD_PT_PNA_PES, PAN_PRI_PVEM, PAN_PRD_PT, PRD_PT_PNA, PRD_PT_Humanista, PRD_PT_PES, PRD_PNA_PES, PT_PNA_Humanista, PT_Humanista_PES, PAN_PRD, PAN_PT, PAN_MC, PAN_Humanista, PRI_PVEM, PRD_PT, PRD_PNA, PRD_PES, PT_MC, PT_Humanista, PT_PES, noreg, nulos)

gana <- select(mich15, PAN, PRI, PRD, PT, PVEM, MC, PNA, Morena, Humanista, PES, INDEP, PAN_PRI_PRD_PNA_Humanista_PES, PRD_PT_PAN_PNA_Humanista, PRD_PTPAN_PNA_PES, PRD_PT_PNA_Humanista, PRD_PT_PNA_PES, PAN_PRI_PVEM, PAN_PRD_PT, PRD_PT_PNA, PRD_PT_Humanista, PRD_PT_PES, PRD_PNA_PES, PT_PNA_Humanista, PT_Humanista_PES, PAN_PRD, PAN_PT, PAN_MC, PAN_Humanista, PRI_PVEM, PRD_PT, PRD_PNA, PRD_PES, PT_MC, PT_Humanista, PT_PES, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
mich15 <- bind_cols(mich15, Winner2)

rm(mich, add, clave)

####Morelos####
mor <- read_excel(paste(inp, "Resultados_mor_ayto_15.xlsx", sep = "/"))
clave <- read.csv(paste(inp, "tabula-morelos.csv", sep = "/"), header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"))
add <- data.frame("muni" = as.factor(c("013", "015", "020")), "MUNICIPIO" = c( "JONACATEPEC", "MIACATLÁN", "TEPOZTLÁN"))
clave <- bind_rows(clave, add)
colnames(mor)[17] <- "noreg"
colnames(mor)[18] <- "nulos"
mor15 <- mor %>%
  left_join(clave, mor, by = "MUNICIPIO") %>%
  mutate(
    year = 2015,
    state = as.character("17"),
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PT, PVEM, MC, PNA, PSD, Morena, PES, Humanista, INDEP1, INDEP2, INDEP3, PRI_PVEM_PNA, noreg, nulos)

gana <- select(mor15, PAN, PRI, PRD, PT, PVEM, MC, PNA, PSD, Morena, PES, Humanista, INDEP1, INDEP2, INDEP3, PRI_PVEM_PNA, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
mor15 <- bind_cols(mor15, Winner2)

rm(clave, add, mor)

####Nuevo León####
nl <- read.csv(paste(inp, "ayuntamientos_nl_2015.csv", sep = "/"))
colnames(nl) <- c("Seccion", "Casilla", "Elección" ,"MUNICIPIO", "Distrito", "Estatus", "LN", "Total", "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "PD", "PCC", "Morena", "Humanista", "PES", "INDEP1", "PRI_PVEM_PNA_PD", "PRI_PVEM_PNA", "PRI_PVEM_PD", "PRI_PNA_PD", "PVEM_PNA_PD", "PRI_PVEM", "PRI_PNA", "PRI_PD", "PVEM_PNA", "PVEM_PD", "PNA_PD", "PRD_PT", "nulos" )
clave <- read.csv(paste(inp, "tabula-NL.csv", sep = "/"), header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"), fileEncoding = "UTF-8")
add <- data.frame("muni" = as.factor("010"), "MUNICIPIO" = "El Carmen")
clave = bind_rows(clave, add)
"El Carmen"
nl15 <- nl %>%
  right_join(clave, nl, by = "MUNICIPIO") %>%
  select(- c("Seccion", "Casilla", "Elección" ,"MUNICIPIO", "Distrito", "Estatus", "LN", "Total")) %>%
  group_by(muni) %>%
  summarise_all(sum, na.rm = T) %>%
  ungroup(muni) %>%
  mutate(
    year = 2015, 
    state = as.character("19"), 
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PT, PVEM, MC, PNA, PD, PCC, Morena, Humanista, PES, INDEP1, PRI_PVEM_PNA_PD, PRI_PVEM_PNA, PRI_PVEM_PD, PRI_PNA_PD, PVEM_PNA_PD, PRI_PVEM, PRI_PNA, PRI_PD, PVEM_PNA, PVEM_PD, PNA_PD, PRD_PT, nulos)

gana <- select(nl15, PAN, PRI, PRD, PT, PVEM, MC, PNA, PD, PCC, Morena, Humanista, PES, INDEP1, PRI_PVEM_PNA_PD, PRI_PVEM_PNA, PRI_PVEM_PD, PRI_PNA_PD, PVEM_PNA_PD, PRI_PVEM, PRI_PNA, PRI_PD, PVEM_PNA, PVEM_PD, PNA_PD, PRD_PT, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
nl15 <- bind_cols(nl15, Winner2)

rm(nl, clave, add)

####Queretaro####
qro <- read_excel(paste(inp, "Aytos_2015.xlsx", sep = "/"))
colnames(qro)[12] <- "noreg"
colnames(qro)[13] <- "nulos"
qro <- qro %>%
  mutate(
    PAN = as.numeric(PAN),
    PRI = as.numeric(PRI), 
    PRD = as.numeric(PRD), 
    MC = as.numeric(MC), 
    PNA = as.numeric(PNA), 
    PVEM = as.numeric(PVEM), 
    PES = as.numeric(PES), 
    Morena = as.numeric(Morena), 
    PT = as.numeric(PT), 
    PRI_PT = as.numeric(PRI_PT), 
    noreg = as.numeric(noreg), 
    nulos = as.numeric(nulos), 
    X__1 = as.numeric(X__1), 
    X__2 = as.numeric(X__2), 
    X__3 = as.numeric(X__3), 
    X__4 = as.numeric(X__4), 
    X__5 = as.numeric(X__5), 
    X__6 = as.numeric(X__6), 
    X__7 = as.numeric(X__7), 
    X__8 = as.numeric(X__8)
  )
clave <- read.csv(paste(inp, "tabula-queretaro.csv", sep = "/"), fileEncoding = "UTF-8", header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"))
clave$MUNICIPIO <- toupper(clave$MUNICIPIO)
qro <- left_join(clave, qro, by = "MUNICIPIO")
levels(as.factor(qro$MUNICIPIO))
ay1 <- qro %>%
  filter(MUNICIPIO == "AMEALCO DE BONFIL") %>%
  group_by(muni) %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(14:21))

ay2 <- qro %>%
  filter(MUNICIPIO == "ARROYO SECO") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(13:21))
colnames(ay2) <- c("muni", "PAN", "PRI", "PRD", "PNA", "PVEM", "Morena", "PT", "PRI_PT", "PNA_PVEM", "noreg", "nulos")

ay3 <- qro %>%
  filter(MUNICIPIO == "CADEREYTA DE MONTES") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(16:21))
colnames(ay3) <- c("muni", "PAN",	"PRI", "PRD", "MC", "PNA",	"PVEM",	"PES", "Morena", "Humanista", "PT",	"PRI_PT",	"INDEP1", "noreg",	"nulos")

ay4 <- qro %>%
  filter(MUNICIPIO == "COLÓN") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(13:21))
colnames(ay4) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PVEM",	"PES",	"Morena",	"Humanista",	"PT",	"noreg",	"nulos"
)

ay5 <- qro %>%
  filter(MUNICIPIO == "CORREGIDORA") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(19:21))
colnames(ay5) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"Humanista",	"PT",	"PRI_PNA_PVEM",	"PRI_PNA",	"PRI_PVEM",	"PNA_PVEM",	"PC",	"noreg",	"nulos"
)

ay6 <- qro %>%
  filter(MUNICIPIO == "EL MARQUÉS") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(18:21))
colnames(ay6) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"PT",	"PRI_PNA_PVEM",	"PRI_PNA_PVEM",	"PRI_PVEM",	"PNA_PVEM",	"INDEP2",	"noreg",	"nulos"
)

ay7 <- qro %>%
  filter(MUNICIPIO == "EZEQUIEL MONTES") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(20:21))
colnames(ay7) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"Humanista",	"PT",	"PRI_PVEM_PT",	"PRI_PNA",	"PRI_PT",	"PNA_PT",	"INDEP3",	"INDEP4",	"noreg",	"nulos"
)

ay8 <- qro %>%
  filter(MUNICIPIO == "HUIMILPAN") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(17:21))
colnames(ay8) <- c("muni", "PAN",	"PRI",	"PRD",	"PNA",	"PVEM",	"PES",	"Morena",	"PT",	"PAN_PRD",	"PRI_PNA_PVEM",	"PRI_PNA",	"PRI_PVEM",	"PNA_PVEM",	"noreg",	"nulos"
)

ay9 <- qro %>%
  filter(MUNICIPIO == "JALPAN DE SERRA") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(13:21))
colnames(ay9) <- c("muni", "PAN",	"PRI",	"PRD",	"PNA",	"PVEM",	"PES",	"Morena",	"PT",	"PRI_PT",	"noreg",	"nulos"
)

ay10 <- qro %>%
  filter(MUNICIPIO == "LANDA DE MATAMOROS") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(11:21))
colnames(ay10) <- c("muni", "PAN",	"PRI",	"PNA",	"PVEM",	"Morena",	"PT",	"PRI_PT",	"noreg",	"nulos"
)

ay11 <- qro %>%
  filter(MUNICIPIO == "PEDRO ESCOBEDO") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T)
colnames(ay11) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"PT",	"PRI_PVEM_PT",	"PRI_PNA_PT",	"PNA_PVEM_PT",	"PRI_PNA",	"PRI_PVEM",	"PRI_PT",	"PNA_PVEM",	"PNA_PT",	"PVEM_PT",	"noreg",	"nulos"
)

ay12 <- qro %>%
  filter(MUNICIPIO == "PEÑAMILLER") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(13:21))
colnames(ay12) <- c("muni", "PAN", "PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"Morena",	"PT",	"PRI_PVEM",	"noreg",	"nulos"
)

ay13 <- qro %>%
  filter(MUNICIPIO == "PINAL DE AMOLES") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(14:21))
colnames(ay13) <- c("muni", "PAN", "PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES" ,"Morena",	"PT",	"PRI_PT",	"noreg",	"nulos"
)

ay14 <- qro %>%
  filter(MUNICIPIO == "QUERÉTARO") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(18:21))
colnames(ay14) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"Humanista",	"PT",	"PRI_PNA_PVEM",	"PRI_PNA",	"PRI_PVEM",	"PNA_PVEM",	"noreg",	"nulos"
)

ay15 <- qro %>%
  filter(MUNICIPIO == "SAN JOAQUÍN") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(11:21))
colnames(ay15) <- c("muni", "PAN",	"PRI",	"PRD",	"PNA",	"Morena",	"PR",	"PRI_PT",	"noreg",	"nulos"
)

ay16 <- qro %>%
  filter(MUNICIPIO == "SAN JUAN DEL RÍO") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(18:21))
colnames(ay16) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"Humanista",	"PT",	"PRI_PNA_PVEM",	"PRI_PNA",	"PRI_PVEM",	"PNA_PVEM",	"noreg",	"nulos"
)

ay17 <- qro %>%
  filter(MUNICIPIO == "TEQUISQUIAPAN") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(15:21))
colnames(ay17) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"PT",	"PRI_PT",	"PNA_PVEM",	"noreg",	"nulos"
)

ay18 <- qro %>%
  filter(MUNICIPIO == "TOLIMÁN") %>%
  group_by(muni)  %>%
  select(-MUNICIPIO) %>%
  summarise_all(sum, na.rm = T) %>%
  select(-(16:21))
colnames(ay18) <- c("muni", "PAN",	"PRI",	"PRD",	"MC",	"PNA",	"PVEM",	"PES",	"Morena",	"Humanista",	"PT",	"INDEP5",	"INDEP6",	"noreg",	"nulos"
)

qro15 <- bind_rows(list(ay1, ay2, ay3, ay4, ay5, ay6, ay7, ay8, ay9, ay10, ay11, ay12, ay13, ay14, ay15, ay16, ay17, ay18))
qro15 <- qro15 %>%
  mutate(
    year = 2015,
    state = as.character("22"),
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, MC, PNA, PVEM, PES, Morena, PT, PRI_PT, PNA_PVEM, Humanista, PRI_PNA_PVEM, PRI_PNA, PRI_PVEM, PC, PRI_PVEM_PT, PNA_PT, INDEP1, INDEP2, INDEP3, INDEP4, INDEP5, INDEP6, PAN_PRD, PRI_PNA_PT,PNA_PVEM_PT, PVEM_PT, PR, noreg, nulos)
rm(qro, ay1, ay2, ay3, ay4, ay5, ay6, ay7, ay8, ay9, ay10, ay11, ay12, ay13, ay14, ay15, ay16, ay17, ay18, clave)

gana <- select(qro15, PAN, PRI, PRD, MC, PNA, PVEM, PES, Morena, PT, PRI_PT, PNA_PVEM, Humanista, PRI_PNA_PVEM, PRI_PNA, PRI_PVEM, PC, PRI_PVEM_PT, PNA_PT, INDEP1, INDEP2, INDEP3, INDEP4, INDEP5, INDEP6, PAN_PRD, PRI_PNA_PT,PNA_PVEM_PT, PVEM_PT, PR, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
qro15 <- bind_cols(qro15, Winner2)

####San Luis Potosi####
ayto_slp = "/Users/Emmanuel RB/Documents/tesis/input/slp_15"

ay1 <- read_excel(paste(ayto_slp, "1_cme_ ciudad fernandez(1).xls", sep = "/"), range = "a3:v63")
ay1 <- ay1[-1,]

ay2 <- read_excel(paste(ayto_slp, "1_cme_ ebano(1).xls", sep = "/"), range = "a3:y59")
ay2 <- ay2[-1,]

ay3 <- read_excel(paste(ayto_slp, "1_cme_ guadalcazar(1).xls", sep = "/"), range = "a3:w45")
ay3 <- ay3[-1,]

ay4 <- read_excel(paste(ayto_slp, "1_cme_ san ciro(1).xls", sep = "/"), range = "a3:p23")
ay4 <- ay4[-1,]

ay5 <- read_excel(paste(ayto_slp, "1_cme_ santo domingo(1).xls", sep = "/"), range = "a3:v28")
ay5 <- ay5[-1,]

ay6 <- read_excel(paste(ayto_slp, "1_cme_ tampamolon corona(1).xls", sep = "/"), range = "a3:t24")
ay6 <- ay6[-1,]

ay7 <- read_excel(paste(ayto_slp, "1_cme_ tanquian de escobedo(1).xls", sep = "/"), range = "a3:r23")
ay7 <- ay7[-1,]

ay8 <- read_excel(paste(ayto_slp, "1_cme_ villa hidalgo(1).xls", sep = "/"), range = "a3:w31")
ay8 <- ay8[-1,]

ay9 <- read_excel(paste(ayto_slp, "1_cme_ villa juarez(1).xls", sep = "/"), range = "a3:t24")
ay9 <- ay9[-1,]

ay10 <- read_excel(paste(ayto_slp, "1_cme_ahualulco(1).xls", sep = "/"), range = "a3:z31")
ay10 <- ay10[-1,]

ay11 <- read_excel(paste(ayto_slp, "1_cme_alaquines(1).xls", sep = "/"), range = "a3:t23")
ay11 <- ay11[-1,]

ay12 <- read_excel(paste(ayto_slp, "1_cme_aquismon(1).xls", sep = "/"), range = "a3:v62")
ay12 <- ay12[-1,]

ay13 <- read_excel(paste(ayto_slp, "1_cme_armadillo(1).xls", sep = "/"), range = "a3:r15")
ay13 <- ay13[-1,]

ay14 <- read_excel(paste(ayto_slp, "1_cme_arriaga(1).xls", sep = "/"), range = "a3:x30")
ay14 <- ay14[-1,]

ay15 <- read_excel(paste(ayto_slp, "1_cme_axt(1).xls", sep = "/"), range = "a3:y45")
ay15 <- ay15[-1,]

ay16 <- read_excel(paste(ayto_slp, "1_cme_cardenas(1).xls", sep = "/"), range = "a3:aa33")
ay16 <- ay16[-1,]

ay17 <- read_excel(paste(ayto_slp, "1_cme_catorce(1).xls", sep = "/"), range = "a3:s24")
ay17 <- ay17[-1,]

ay18 <- read_excel(paste(ayto_slp, "1_cme_cedral(1).xls", sep = "/"), range = "a3:s32")
ay18 <- ay18[-1,]

ay19 <- read_excel(paste(ayto_slp, "1_cme_cerritos(1).xls", sep = "/"), range = "a3:v43")
ay19 <- ay19[-1,]

ay20 <- read_excel(paste(ayto_slp, "1_cme_charcas(1).xls", sep = "/"), range = "a3:u37")
ay20 <- ay20[-1,]

ay21 <- read_excel(paste(ayto_slp, "1_cme_ciudad del maiz(1).xls", sep = "/"), range = "a3:x53")
ay21 <- ay21[-1,]

ay22 <- read_excel(paste(ayto_slp, "1_cme_coxcatlan(1).xls", sep = "/"), range = "a3:v27")
ay22 <- ay22[-1,]

ay23 <- read_excel(paste(ayto_slp, "1_cme_el naranjo(1).xls", sep = "/"), range = "a3:x32")
ay23 <- ay23[-1,]

ay24 <- read_excel(paste(ayto_slp, "1_cme_huehuetlan(1).xls", sep = "/"), range = "a3:v26")
ay24 <- ay24[-1,]

ay25 <- read_excel(paste(ayto_slp, "1_cme_lagunillas(1).xls", sep = "/"), range = "a3:r17")
ay25 <- ay25[-1,]

ay26 <- read_excel(paste(ayto_slp, "1_cme_matehuala(1).xls", sep = "/"), range = "a3:v131")
ay26 <- ay26[-1,]

ay27 <- read_excel(paste(ayto_slp, "1_cme_matlapa(1).xls", sep = "/"), range = "a3:v41")
ay27 <- ay27[-1,]

ay28 <- read_excel(paste(ayto_slp, "1_cme_mexquitic.xls", sep = "/"), range = "a3:y79")
ay28 <- ay28[-1,]

ay29 <- read_excel(paste(ayto_slp, "1_cme_moctezuma(1).xls", sep = "/"), range = "a3:u34")
ay29 <- ay29[-1,]

ay30 <- read_excel(paste(ayto_slp, "1_cme_rayon(1).xls", sep = "/"), range = "a3:q31")
ay30 <- ay30[-1,]

ay31 <- read_excel(paste(ayto_slp, "1_cme_rioverde(1).xls", sep = "/"), range = "a3:y149")
ay31 <- ay31[-1,]

ay32 <- read_excel(paste(ayto_slp, "1_cme_salinas(1).xls", sep = "/"), range = "a3:y44")
ay32 <- ay32[-1,]

ay33 <- read_excel(paste(ayto_slp, "1_cme_san antonio(1).xls", sep = "/"), range = "a3:s17")
ay33 <- ay33[-1,]

ay34 <- read_excel(paste(ayto_slp, "1_cme_san martin chalchicuautla(1).xls", sep = "/"), range = "a3:x35")
ay34 <- ay34[-1,]

ay35 <- read_excel(paste(ayto_slp, "1_cme_san nicolas tolentino(1).xls", sep = "/"), range = "a3:x16")
ay35 <- ay35[-1,]

ay36 <- read_excel(paste(ayto_slp, "1_cme_san vicente(1).xls", sep = "/"), range = "a3:r30")
ay36 <- ay36[-1,]

ay37 <- read_excel(paste(ayto_slp, "1_cme_sanpedro(1).xls", sep = "/"), range = "a3:y13")
ay37 <- ay37[-1,]

ay38 <- read_excel(paste(ayto_slp, "1_cme_santa catarina(1).xls", sep = "/"), range = "a3:s23")
ay38 <- ay38[-1,]

ay39 <- read_excel(paste(ayto_slp, "1_cme_santa maria del rio(1).xls", sep = "/"), range = "a3:v61")
ay39 <- ay39[-1,]

ay40 <- read_excel(paste(ayto_slp, "1_cme_slp(1).xls", sep = "/"), range = "a3:z958")
ay40 <- ay40[-1,]

ay41 <- read_excel(paste(ayto_slp, "1_cme_sol(1).xls", sep = "/"), range = "a3:ab305")
ay41 <- ay41[-1,]

ay42 <- read_excel(paste(ayto_slp, "1_cme_tamasopo(1).xls", sep = "/"), range = "a3:w44")
ay42 <- ay42[-1,]

ay43 <- read_excel(paste(ayto_slp, "1_cme_tamazunchale(1).xls", sep = "/"), range = "a3:w124")
ay43 <- ay43[-1,]

ay44 <- read_excel(paste(ayto_slp, "1_cme_tampacan(1).xls", sep = "/"), range = "a3:x28")
ay44 <- ay44[-1,]

ay45 <- read_excel(paste(ayto_slp, "1_cme_tamuin(1).xls", sep = "/"), range = "a3:ae57")
ay45 <- ay45[-1,]

ay46 <- read_excel(paste(ayto_slp, "1_cme_tancanhuitz(1).xls", sep = "/"), range = "a3:s30")
ay46 <- ay46[-1,]

ay47 <- read_excel(paste(ayto_slp, "1_cme_tanlajas(1).xls", sep = "/"), range = "a3:z29")
ay47 <- ay47[-1,]

ay48 <- read_excel(paste(ayto_slp, "1_cme_tierra nueva(1).xls", sep = "/"), range = "a3:v18")
ay48 <- ay48[-1,]

ay49 <- read_excel(paste(ayto_slp, "1_cme_val(1).xls", sep = "/"), range = "a3:z223")
ay49 <- ay49[-1,]

ay50 <- read_excel(paste(ayto_slp, "1_cme_vanegas(1).xls", sep = "/"), range = "a3:p18")
ay50 <- ay50[-1,]

ay51 <- read_excel(paste(ayto_slp, "1_cme_venado(1).xls", sep = "/"), range = "a3:v29")
ay51 <- ay51[-1,]

ay52 <- read_excel(paste(ayto_slp, "1_cme_villa de arista(1).xls", sep = "/"), range = "a3:s26")
ay52 <- ay52[-1,]

ay53 <- read_excel(paste(ayto_slp, "1_cme_villa de la paz(1).xls", sep = "/"), range = "a3:r13")
ay53 <- ay53[-1,]

ay54 <- read_excel(paste(ayto_slp, "1_cme_villa de reyes(1).xls", sep = "/"), range = "a3:w64")
ay54 <- ay54[-1,]

ay55 <- read_excel(paste(ayto_slp, "1_cme_villa guadalupe(1).xls", sep = "/"), range = "a3:r22")
ay55 <- ay55[-1,]

ay56 <- read_excel(paste(ayto_slp, "1_cme_xilitla(1).xls", sep = "/"), range = "a3:w74")
ay56 <- ay56[-1,]

ay57 <- read_excel(paste(ayto_slp, "1_cme_zaragoza(1).xls", sep = "/"), range = "a3:aa40")
ay57 <- ay57[-1,]

ay58 <- read_excel(paste(ayto_slp, "1_cme_villa de ramos PAW.xls", sep = "/"), range = "a3:w55")
ay58 <- ay58[-1,]

slp <- list(ay1, ay2, ay3, ay4, ay5, ay6, ay7, ay8, ay9, ay10, ay11, ay12, ay13, ay14, ay15, ay16, ay17, ay18, ay19, ay20, ay21, ay22, ay23, ay24, ay25, ay26, ay27, ay28, ay29, ay30, ay31, ay32, ay33, ay34, ay35, ay36, ay37, ay38, ay39, ay40, ay41, ay42, ay43, ay44, ay45, ay46, ay47, ay49 , ay50, ay51, ay52, ay53, ay54, ay55, ay56, ay57, ay58)
slp <- rbind.fill(slp)
names(slp) <- str_replace_all(names(slp), c(" " = "." , "," = "" ))  
slp <- slp %>%
  select(-c("Dto.Local", "codigo", "Municipio", "Seccion", "Tipo", "Lista.Nominal", "VOTACION.VALIDA.EMITIDA", "VOTACION.EMITIDA")) %>%
  mutate_all(funs(as.numeric)) %>%
  group_by(No.Mpio) %>%
  summarise_all(sum, na.rm = T)	%>%
  select(-c(6, 15, 16, 22, 23, 26, 27, 30, 32, 34, 36, 37, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 61, 62, 63, 65, 67, 68, 69))
colnames(slp) <- c("muni", "PAN", "PRI", "PRD", "PMC", "PVEM", "PCP", "PNA", "Morena", "Humanista", "noreg", "nulos", "PRD_PMC", "PT", "PES", "INDEP", "PRI_PVEM_PNA", "PRD_PCP", "PAN_PT_PMC", "PRI_PNA", "PAN_PT", "PRD_PVEM_PCP", "PAN_PRD_PT", "PRI_PVEM", "PRD_PT_PMC", "PAN_PMC", "PRD_PT_PNA", "PAN_PRD_PT_PVEM_PMC_PNA", "PRD_PT_PCP", "PT_PMC", "PRD_PT", "PRD_PT_PCP_PMC", "PAN_PCP", "PRD_PT_PMC_PNA", "PAN_PVEM_PNA", "PCP_PMC_PNA", "PAN_PRD_PT_PMC", "PRD_PCP_PMC", "PMC_PNA", "PVEM_PNA")

slp15 <- slp %>%
  mutate(
    muni = as.factor(formatC(muni, width = 3, format="d", flag="0")),
    year = 2015,
    state = as.character("24"),
    mpo = paste(state, muni, sep = ""), 
    muniYear = paste(mpo, year, sep = "_")
      ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PMC, PVEM, PCP, PNA, Morena, Humanista, PRD_PMC, PT, PES, INDEP, PRI_PVEM_PNA, PRD_PCP, PAN_PT_PMC, PRI_PNA, PAN_PT, PRD_PVEM_PCP, PAN_PRD_PT, PRI_PVEM, PRD_PT_PMC, PAN_PMC, PRD_PT_PNA, PAN_PRD_PT_PVEM_PMC_PNA, PRD_PT_PCP, PT_PMC, PRD_PT, PRD_PT_PCP_PMC, PAN_PCP, PRD_PT_PMC_PNA, PAN_PVEM_PNA, PCP_PMC_PNA, PAN_PRD_PT_PMC, PRD_PCP_PMC, PMC_PNA, PVEM_PNA, noreg, nulos)

rm(ay1, ay2, ay3, ay4, ay5, ay6, ay7, ay8, ay9, ay10, ay11, ay12, ay13, ay14, ay15, ay16, ay17, ay18, ay19, ay20, ay21, ay22, ay23, ay24, ay25, ay26, ay27, ay28, ay29, ay30, ay31, ay32, ay33, ay34, ay35, ay36, ay37, ay38, ay39, ay40, ay41, ay42, ay43, ay44, ay45, ay46, ay47, ay48, ay49 , ay50, ay51, ay52, ay53, ay54, ay55, ay56, ay57, ay58, slp)

gana <- select(slp15, PAN, PRI, PRD, PMC, PVEM, PCP, PNA, Morena, Humanista, PRD_PMC, PT, PES, INDEP, PRI_PVEM_PNA, PRD_PCP, PAN_PT_PMC, PRI_PNA, PAN_PT, PRD_PVEM_PCP, PAN_PRD_PT, PRI_PVEM, PRD_PT_PMC, PAN_PMC, PRD_PT_PNA, PAN_PRD_PT_PVEM_PMC_PNA, PRD_PT_PCP, PT_PMC, PRD_PT, PRD_PT_PCP_PMC, PAN_PCP, PRD_PT_PMC_PNA, PAN_PVEM_PNA, PCP_PMC_PNA, PAN_PRD_PT_PMC, PRD_PCP_PMC, PMC_PNA, PVEM_PNA, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
slp15 <- bind_cols(slp15, Winner2)

####Sonora####
son <- read_excel (paste(inp, "ComputoMpalAyuntamiento2015_global.xlsx", sep = "/"), range = "a2:w74")
colnames(son) <- c("MUNICIPIO", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM_PNA", "PRI_PVEM", "PRI_PNA", "PVEM_PNA", "UXE", "UF", "INDEP1", "INDEP2", "noreg", "nulos", "TOTAL", "S_PRI_PVEM_PNA")
clave <- read.csv(paste(inp, "tabula-son.csv", sep = "/"), fileEncoding = "UTF-8", header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"))
clave$MUNICIPIO <- toupper(clave$MUNICIPIO)
add <- data.frame("muni" = as.factor(c("003", "070")), "MUNICIPIO" = c("ÁLAMOS", "GRAL. PLUTARCO ELÍAS CALLES"))
clave <- bind_rows(clave, add)
son <- right_join(clave, son, by = "MUNICIPIO")
son15 <- son %>%
  mutate(
    year = 2015,
    state = as.character("26"),
    mpo = as.factor(paste(state, muni, sep = "")),
    muniYear = paste(mpo, year, sep = "_")
    ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PVEM, PT, MC, PNA, Morena, Humanista, PES, S_PRI_PVEM_PNA, PRI_PVEM, PRI_PNA, PVEM_PNA, UXE, UF, INDEP1, INDEP2, noreg, nulos)
colnames(son15) <- c("muniYear", "state", "year", "muni", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "PRI_PVEM_PNA", "PRI_PVEM", "PRI_PNA", "PVEM_PNA", "UXE", "UF", "INDEP1", "INDEP2", "noreg", "nulos")

gana <- select(son15, PAN, PRI, PRD, PVEM, PT, MC, PNA, Morena, Humanista, PES, PRI_PVEM_PNA, PRI_PVEM, PRI_PNA, PVEM_PNA, UXE, UF, INDEP1, INDEP2, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
son15 <- bind_cols(son15, Winner2)

rm(son, clave, add)

####Tabasco####
tab <- read_excel(paste(inp, "estadistica_municipal_concentrado_2015.xls", sep = "/"), range = "a9:q25")
colnames(tab) <- c("MUNICIPIO", "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "Morena", "Humanista", "PES", "INDEP", "PRD_PVEM", "S_PRD_PVEM", "noreg", "nulos", "TOTAL")
clave <- read.csv(paste(inp, "tabula-tabasco.csv", sep = "/"), fileEncoding = "UTF-8", header = F, colClasses = "factor", col.names = c("muni", "MUNICIPIO"))
clave$MUNICIPIO <- toupper(clave$MUNICIPIO)
tab <- left_join(tab, clave, by = "MUNICIPIO")
tab15 <- tab %>%
  mutate(
    year = 2015,
    state = as.character("27"),
    mpo = paste(state, muni, sep = ""),
    muniYear = paste(mpo, year, sep = "_")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PT, PVEM, MC, PNA, Morena, Humanista, PES, INDEP, S_PRD_PVEM, noreg, nulos)
colnames(tab15) <- c("muniYear", "state", "year", "muni", "PAN", "PRI", "PRD", "PT", "PVEM", "MC", "PNA", "Morena", "Humanista", "PES", "INDEP", "PRD_PVEM", "noreg", "nulos")
tab15$Winner2 <- colnames(tab15)[apply(tab15, 1, which.max)]
rm(tab, clave)

####Yucatan####
yuc <- read_excel(paste(inp, "RESULTADOS-POR-CASILLAS-DE-REGIDORES-2015.xlsx", sep = "/"), range = "a2:t2531")
clave <- read.csv(paste(inp, "tabula-yuc.csv", sep = "/"), fileEncoding = "UTF-8", header = F, colClasses = "factor", col.names = c("muni", "Municipio"))
clave$Municipio <- toupper(clave$Municipio)
yuc15 <- yuc %>%
  left_join(clave, yuc, by = "Municipio") %>%
  select(-c("Municipio", "Distrito Electoral Local", "Sección", "Tipo de casilla", "TOTAL")) %>%
  group_by(muni) %>%
  summarise_all(sum, na.rm = T) %>%
  mutate(
    year = 2015, 
    state = as.character("31"),
    mpo = paste(state, muni, sep = ""), 
    muniYear = paste(mpo, year, sep = "_")
  ) %>%
  select(muniYear, state, year, muni, PAN, PRI, PRD, PVEM, PT, MC, PANAL, MORENA, PH, PES, CC1, CC2, IND, CNR, VN)
colnames(yuc15) <- c("muniYear", "state", "year", "muni", "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "PNA", "Morena", "Humanista", "PES", "CC1", "CC2", "INDEP", "noreg", "nulos")

gana <- select(yuc15, PAN, PRI, PRD, PVEM, PT, MC, PNA, Morena, Humanista, PES, CC1, CC2, INDEP, noreg, nulos)
Winner2 <- colnames(gana)[apply(gana, 1, which.max)]

Winner2 <- as.data.frame(Winner2)
yuc15 <- bind_cols(yuc15, Winner2)
rm(yuc, clave, gana, Winner2)

####Juntando la actualizacion####
act <- bind_rows(list(nay14, bcs15, camp15, cdmx15, chis15, col15, gro15, gto15, jal15, mex15, mich15, mor15, nl15, qro15, slp15, son15, tab15, yuc15)) 
suma <- act %>% select(-c("state", "year", "muni", "Winner2")) 
suma <- suma %>% 
  mutate(total = rowSums(suma[2:105], na.rm = T)) 

sum_pan <- suma %>% select(muniYear, starts_with("PAN")) 
sum_pan <- sum_pan %>% 
  mutate(
    s_PAN_PRD = rowSums(sum_pan[c("PAN_PRD", "PAN_PRI_PRD_PNA_Humanista_PES", "PAN_PRD_PT", "PAN_PRD_PT_PVEM_PMC_PNA", "PAN_PRD_PT_PMC")], na.rm = T),
    s_PAN = rowSums(sum_pan[c("PAN", "PAN_PT", "PAN_PRI_PVEM", "PAN_MC", "PAN_Humanista", "PAN_PT_PMC", "PAN_PMC", "PAN_PCP", "PAN_PVEM_PNA")], na.rm = T)
  ) %>% 
  select(muniYear, s_PAN_PRD, s_PAN)
colnames(sum_pan)[2] <- "PAN_PRD"
colnames(sum_pan)[3] <- "PAN"

sum_pri <- suma %>% select(muniYear, starts_with("PRI")) 
sum_pri <- sum_pri %>% 
  mutate(s_PRI = rowSums(sum_pri[c("PRI_PVEM_PNA", "PRI_PVEM", "PRI", "PRI_PVEM_PNA_PD", "PRI_PVEM_PD","PRI_PNA_PD", "PRI_PNA", "PRI_PD", "PRI_PT", "PRI_PNA_PVEM", "PRI_PVEM_PT")], na.rm = T)) %>% 
  select(muniYear, s_PRI)
colnames(sum_pri)[2] <- "PRI"

sums <- full_join(sum_pan, sum_pri, by = "muniYear")
#rm(sum_pan, sum_pri)
suma <- suma %>% 
  select(muniYear, total) %>% full_join(sums)

act <- act %>% 
  select(muniYear, state, muni, year, Winner2, PAN, PRI, PRD, nulos, noreg)

act <- bind_cols(act, suma)
act <- act %>% 
  mutate(
    s_PAN = rowSums(act[c("PAN", "PAN1")], na.rm= T),
    s_PRI = rowSums(act[c("PRI", "PRI1")], na.rm = T),
    muni = paste(state, muni, sep = "")
  ) %>% 
  select(-c("PAN", "PAN1", "PRI", "PRI1", "muniYear1"))
colnames(act)[11] <- "PAN"
colnames(act)[12] <- "PRI"
####Data Lucardi####
data <- read.csv(paste(inp, "Municipal elections Mexico 1980-2013.csv", sep="/"))
names(data) <- str_replace_all(names(data), "[.]", "_")

data <- data %>% 
  mutate(
    muniYear = factor(substr(muniYear, 2, 11)),
    muni = factor(substr(muni, 2, 6)),
    state = ifelse(state=="AGS", "01", ifelse( state=="BC", "02", ifelse(state== "BCS", "03", ifelse(state=="CAM", "04", ifelse(state=="CHUA", "08", ifelse(state== "CHIA","07", ifelse(state=="CLA", "05", ifelse(state=="CMA", "06", ifelse(state=="DF", "09", ifelse(state=="DGO", "10", ifelse(state=="EDOMEX", "15", ifelse(state=="GTO", "11", ifelse(state== "GRO", "12", ifelse(state== "HID", "13", ifelse(state=="JAL", "14", ifelse(state== "MICH","16", ifelse(state=="MOR","17", ifelse(state=="NAY", "18", ifelse(state=="OAX", "20", ifelse(state=="NL", "19", ifelse(state== "PUE", "21", ifelse(state=="QUER", "22", ifelse(state=="QROO", "23", ifelse(state=="SLP", "24", ifelse(state== "SIN", "25", ifelse(state=="SON", "26", ifelse(state=="TAB", "27", ifelse(state=="TAM", "28", ifelse(state=="TLAX", "29", ifelse(state=="VER", "30", ifelse(state=="YUC", "31", "32" )))))))))))))))))))))))))))))))
  ) %>% select(muniYear, state, muni, year, Winner2, PAN, PRI, PRD, PAN_PRD, nulos, noreg, total)
data <- bind_rows(data, act)

data = arrange(data,muni)

data$Winner2 = str_replace_all(data$Winner2, "Nval" , "PNA")
data$Winner2 = str_replace_all(data$Winner2, "[.]", "_")
data$Winner2 = str_replace_all(data$Winner2, 'Conv' , "MC")
data$Winner2 = ifelse(data$Winner2 == "Independiente1", "INDEP1", data$Winner2)
data$Winner2 = ifelse(data$Winner2 == "INDEP1", "INDEP", data$Winner2)

####Gubernaturas####
gobs <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTAJ22OR9JCNfRdQFeHUO1szhw-tje4sjtMCMf3m9DA5mKjoWMNCWi4Pr3SPeO1TEb5wvvVZArw6ecq/pub?gid=1686758032&single=true&output=csv"
gob <- read.csv(gobs, stringsAsFactors = F)
gob$INEGI <- formatC(gob$INEGI, width = 2, format = "d", flag = "0")

elec <- 1973:2019
gober <- data.frame()
pb = txtProgressBar(min=1, max=length(elec), style=3)
for (x in 1:length(elec)) {
  tempo <- gob %>% select(INEGI, ends_with(as.character(elec[x]))) %>% mutate(year = elec[x])
  names(tempo) <- c("INEGI", "win", "year")
  gober <- bind_rows(gober, tempo)
  setTxtProgressBar(pb, x)
  rm(tempo)
}
colnames(gober)[2] <- "win_state"
gober <- gober %>% mutate(
  gub = paste(INEGI, year, sep = "_"),
  wintop_state = sapply(strsplit(win_state, "_"), "[", 1)
  )

data <- data %>% 
  mutate(gub = paste(state, year, sep = "_")
  ) %>% inner_join(gober, by = "gub") %>% select(-c(gub, INEGI, year.y))
colnames(data)[4] <- "year"

####Subset de partidos####
levels(as.factor(data$Winner2))     

data <- data %>%
  mutate(
    win = sapply(strsplit(Winner2, "_"), "[", 1),
    win_top = #ifelse(win == "PVEM", "PVEM",
                     #ifelse(win == "PT", "PT",
                            ifelse(win == "PRI", "PRI",
                                   #ifelse(win == "PNA", "PNA",
                                          ifelse(win == "PAN", "PAN",
                                                 #ifelse(win == "MC", "MC", 
                                                        ifelse(win =="PRD", "PRD","Otros")))#))))    
    ) %>%
  group_by(muni) %>%
  mutate(
    prevyear = lag(year, n = 1),
    difY = ( year - prevyear),
    inc = lag(win, n = 1),
    cont = ifelse(inc == win, 1, 0),
    alt = ifelse(inc != win, 1, 0),
    inc_top = #ifelse(inc == "PVEM", "PVEM",
                     #ifelse(inc == "PT", "PT",
                            ifelse(inc == "PRI", "PRI",
                                   #ifelse(inc == "PNA", "PNA",
                                          ifelse(inc == "PAN", "PAN",
                                                 #ifelse(inc == "MC", "MC", 
                                                        ifelse(inc == "PRD", "PRD", "Otros")))#))))
    #firstPRIdefeat = ifelse(win_top != "PRI", year, NA)
    #firstPRIdefeat = ifelse(win_top != "PRI", year, NA)
  ) %>% 
  ungroup(muni)
####Concordancia y otras variables####
data <- data %>% 
  mutate(
    conco = ifelse(win_top == wintop_state, 1, 0),
    PRI_ofi = ifelse(win == "PRI" & wintop_state == "PRI", 1, 0),
    PAN_ofi = ifelse(win == "PAN" & wintop_state == "PAN", 1, 0),
    PRD_ofi = ifelse(win == "PRD" & wintop_state == "PRD", 1, 0),
    PAN_PRD_ofi = ifelse(substr(Winner2, 1, 7) == "PAN_PRD" & substr(win_state, 1, 7) == "PAN_PRD", 1, 0),
    PRI_opo = ifelse(win == "PRI" & wintop_state != "PRI", 1, 0),
    PAN_opo = ifelse(win == "PAN" & wintop_state != "PAN", 1, 0),
    PRD_opo= ifelse(win == "PRD" & wintop_state != "PRD", 1, 0),
    PAN_PRD_opo = ifelse(substr(Winner2, 1, 7) == "PAN_PRD" & substr(win_state, 1, 7) != "PAN_PRD", 1, 0),
    edo_year = paste(state, year, sep = "")
  )

levels(as.factor(data$Winner2))
levels(as.factor(data$win))
summary(data)
####Exportando a csv####
as.data.frame(data, row.names = NULL)
write.csv(data, paste(out, "data.csv", sep = "/"), row.names = F, fileEncoding ="UTF-8")