rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)
library(stargazer)
library(lfe)
library(lme4)
library(beepr)

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"
out <- "/home/dhjs/Documentos/R_projects/tesis/presentacion"


# Data wrangling ----------------------------------------------------------

data <- read.csv(paste(inp, "data.csv", sep = "/"))
#27,292 observaciones

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    prevyear = lag(year, n = 1),
    difY = year - prevyear,
    
    inc = lag(Winner2, n = 1),      
    inc = sapply(strsplit(as.character(inc), "_"), "[", 1),
    
    PAN_ofi = ifelse(lag(inc, n = 1) == "PAN", 1, 0),
    PRI_ofi = ifelse(lag(inc, n = 1) == "PRI", 1, 0),
    PRD_ofi = ifelse(lag(inc, n = 1) == "PRD", 1, 0)
  ) %>% 
  ungroup()
#Aprox 4,000 NA

data <- data %>% 
  mutate(
    inc_top = ifelse(inc == "PRI", "PRI",
                     ifelse(inc == "PAN", "PAN",
                            ifelse(inc == "PRD", "PRD", 
                                   ifelse(is.na(inc), NA, "Otros")))),
    PAN.s = PAN/total,
    PRI.s = PRI/total,
    PRD.s = PRD/total,
    PAN_PRD.s = PAN_PRD/total,
    
    inc.share = ifelse(inc_top == "PAN", PAN.s,
                       ifelse(inc_top == "PRI", PRI.s,
                              ifelse(inc_top == "PRD", PRD.s, 
                                     ifelse(inc_top == "PAN_PRD", PAN_PRD.s, NA)))),
    inc.share = (inc.share * 100)
  ) %>% 
  select(-muniYear1)

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    inc.share.lag = lag(inc.share, n = 1),
    inc.ch = inc.share - inc.share.lag
  ) %>% 
  ungroup()

bp <- read.csv(paste(inp, "Bienes_publicos.csv", sep = "/"))
colnames(bp)[4] <- "tot_del"
bp <- select(bp, -c(year, muni))
bp <- bp %>% select(-(13:32))
##Si se utilizan las lag variables hay que agrupar por año y hacer un ifelse para calcular el promedio, será conveniente agrupar por año y estado ??

data <- left_join(data, bp, by = "muniYear")

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    prevyear = lag(year, n = 1),
    difY = year - prevyear
  ) %>% 
  ungroup()

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    l.del = lag(tasa_tot_del, n = 1),
    l.hom = lag(tasa_hom, n = 1),
    l.agua = lag(tasa_agua, n = 1),
    l.elec = lag(tasa_elec, n = 1),
    l.dren = lag(tasa_dren, n = 1),
    
    ch.del = ifelse(!is.na(tasa_tot_del) & !is.na(l.del), ((tasa_tot_del - l.del)/l.del) * 100, NA),
    ch.hom = ifelse(!is.na(tasa_hom) & !is.na(l.hom), ((tasa_hom - l.hom)/l.hom) * 100, NA),
    ch.agua = ifelse(!is.na(tasa_agua) & !is.na(l.agua), ((tasa_agua - l.agua)/l.agua) * 100, NA),
    ch.elec = ifelse(!is.na(tasa_elec) & !is.na(l.elec), ((tasa_elec - l.elec)/l.elec) * 100, NA),
    ch.dren = ifelse(!is.na(tasa_dren) & !is.na(l.dren), ((tasa_dren - l.dren)/l.dren) * 100, NA),
    
    conco = ifelse(as.character(win_top) == as.character(wintop_state), 1, 0)
  )

summary(data)


data <- data %>% 
  filter(
    !is.infinite(ch.del) & !is.infinite(ch.hom) & !is.infinite(ch.agua) & !is.infinite(ch.elec) & !is.infinite(ch.dren)
  )

mar <- read.csv(paste(inp, "Marginación.csv", sep = "/"))

data <- left_join(data, mar, by = "muniYear")

beep(2) 

# Modelos -----------------------------------------------------------------
##de las poco más de 25K observaciones, 24K no son leídas porque tienen NA
#24712 observations deleted due to missingness


#Modelo lineal
t1 <- lm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco, data)
t2 <- lm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco, data)
t3 <- lm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco, data)
t4 <- lm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco, data)
t5 <- lm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco, data)

t6 <- glm(alt ~ ch.agua + log(Pob_Total) + IM + conco, data, family = binomial(link = "probit"))
t7 <- glm(alt ~ ch.elec + log(Pob_Total) + IM + conco, data, family = binomial(link = "probit"))
t8 <- glm(alt ~ ch.dren + log(Pob_Total) + IM + conco, data, family = binomial(link = "probit"))
t9 <- glm(alt ~ ch.del + log(Pob_Total) + IM + conco, data, family = binomial(link = "probit"))
t10 <- glm(alt ~ ch.hom + log(Pob_Total) + IM + conco, data, family = binomial(link = "probit"))

stargazer(t1, t2, t3, t4, t5, x6, x7, x8, x9, x10, type = "html", out = paste(out, "todos.html", sep = "/"), flip = T)

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data)
u2 <- felm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data)
u3 <- felm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data)
u4 <- felm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data)
u5 <- felm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data)

u6 <- glmer(alt ~ ch.agua + log(Pob_Total) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u7 <- glmer(alt ~ ch.elec + log(Pob_Total) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u8 <- glmer(alt ~ ch.dren + log(Pob_Total) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u9 <- glmer(alt ~ ch.del + log(Pob_Total) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u10 <- glmer(alt ~ ch.hom + log(Pob_Total) + IM + conco + (1 | edo_year), data, model = binomial("probit"))

stargazer(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, type = "html", out = paste(out, "todos_FE.html", sep = "/"), flip = T)

