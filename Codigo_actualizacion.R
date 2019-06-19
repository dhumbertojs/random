rm(llist = ls())
setwd("~")

library(dplyr)

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"

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
#as.data.frame(data, row.names = NULL)
#write.csv(data, paste(out, "data.csv", sep = "/"), row.names = F, fileEncoding ="UTF-8")