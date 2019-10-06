rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)
library(tidyr)
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
  select(-muniYear1) %>% 
  filter(inc_top != "Otros")

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    inc.share.lag = lag(inc.share, n = 1),
    inc.ch = inc.share - inc.share.lag
  ) %>% 
  ungroup()

clave <- data %>% 
  select(muniYear) %>% 
  mutate(try = 1:nrow(data))
##debe haber datos para 17,021 municipios-año electoral
##Revisar como estoy haciendo los calculos

bp <- read.csv(paste(inp, "Bienes_publicos.csv", sep = "/"))
colnames(bp)[4] <- "tot_del"
bp <- select(bp, -year, -muni)
#bp <- bp %>% select(-(14:38))

# bp <- left_join(bp, clave, by = "muniYear")
# bp <- bp %>% 
#   arrange(muniYear) %>% 
#   fill(try)

#Y si observo cambio porcentual respecto al promedio? 
# bp <- bp %>% 
#   group_by(try) %>% 
#   mutate(
#     mn.agua = mean(tasa_agua, na.rm = T),
#     #md.agua = median(tasa_agua, na.rm = T),
#     
#     mn.dren = mean(tasa_dren, na.rm = T),
#     #md.dren = median(tasa_dren, na.rm = T),
#     
#     mn.elec = mean(tasa_elec, na.rm = T),
#     #md.elec = median(tasa_elec, na.rm = T),
#     
#     mn.del = mean(tasa_tot_del, na.rm = T),
#     #md.del = median(tasa_tot_del, na.rm = T),
#     
#     mn.hom = mean(tasa_hom, na.rm = T),
#     #md.hom = median(tasa_hom, na.rm = T)
#   ) %>% 
#   ungroup() %>% 
#   select(-muni)

data <- inner_join(data, bp, by = "muniYear")
#17,021 observaciones con datos electorales y de bienes

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    prevyear = lag(year, n = 1),
    difY = year - prevyear,
    conco = ifelse(as.character(win_top) == as.character(wintop_state), 1, 0)
  ) %>% 
  ungroup()

# data <- data %>% 
#   group_by(muni) %>% 
#   mutate(
#     l.del = lag(tasa_tot_del, n = 1),
#     l.hom = lag(tasa_hom, n = 1),
#     l.agua = lag(tasa_agua, n = 1),
#     l.elec = lag(tasa_elec, n = 1),
#     l.dren = lag(tasa_dren, n = 1),
#     
#     ch.del = ifelse(!is.na(tasa_tot_del) & !is.na(l.del), ((tasa_tot_del - l.del)/l.del) * 100, NA),
#     ch.hom = ifelse(!is.na(tasa_hom) & !is.na(l.hom), ((tasa_hom - l.hom)/l.hom) * 100, NA),
#     ch.agua = ifelse(!is.na(tasa_agua) & !is.na(l.agua), ((tasa_agua - l.agua)/l.agua) * 100, NA),
#     ch.elec = ifelse(!is.na(tasa_elec) & !is.na(l.elec), ((tasa_elec - l.elec)/l.elec) * 100, NA),
#     ch.dren = ifelse(!is.na(tasa_dren) & !is.na(l.dren), ((tasa_dren - l.dren)/l.dren) * 100, NA)
#   )


# data <- data %>% 
#   mutate(
#     ch.del = ifelse(!is.na(tasa_tot_del) & !is.na(mn.del), ((tasa_tot_del - mn.del)/mn.del) * 100, NA),
#     ch.hom = ifelse(!is.na(tasa_hom) & !is.na(mn.hom), ((tasa_hom - mn.hom)/mn.hom) * 100, NA),
#     ch.agua = ifelse(!is.na(tasa_agua) & !is.na(mn.agua), ((tasa_agua - mn.agua)/mn.agua) * 100, NA),
#     ch.elec = ifelse(!is.na(tasa_elec) & !is.na(mn.elec), ((tasa_elec - mn.elec)/mn.elec) * 100, NA),
#     ch.dren = ifelse(!is.na(tasa_dren) & !is.na(mn.dren), ((tasa_dren - mn.dren)/mn.dren) * 100, NA)
#   )

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
    ch.dren = ifelse(!is.na(tasa_dren) & !is.na(l.dren), ((tasa_dren - l.dren)/l.dren) * 100, NA)
  )

summary(data)


data <- data %>% 
  filter(
    !is.infinite(ch.del) & !is.infinite(ch.hom) & !is.infinite(ch.agua) & !is.infinite(ch.elec) & !is.infinite(ch.dren)
  )

mar <- read.csv(paste(inp, "Marginación.csv", sep = "/"), stringsAsFactors = F)

data <- left_join(data, mar, by = "muniYear")

beep(2) 

# Modelos -----------------------------------------------------------------

#Modelo lineal
#summary(lm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco, data)) ##Ahora dice que se eliminaron 15,611 
#de las 25,086 que se eliminaban antes..
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

stargazer(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, type = "html", out = paste(out, "todos.html", sep = "/"), flip = T)

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

#PAN####
PAN1 <- lm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN")
PAN2 <- lm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN")
PAN3 <- lm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN")
PAN4 <- lm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN")
PAN5 <- lm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN")

PAN6 <- glm(alt ~ ch.agua + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN7 <- glm(alt ~ ch.elec + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN8 <- glm(alt ~ ch.dren + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN9 <- glm(alt ~ ch.del + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN10 <- glm(alt ~ ch.hom + log(Pob_Total) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))

stargazer(PAN1, PAN2, PAN3, PAN4, PAN5, PAN6, PAN7, PAN8, PAN9, PAN10, type = "html", out = paste(out, "PAN.html", sep = "/"), flip = T)

#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe2 <- felm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe3 <- felm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe4 <- felm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe5 <- felm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")

panfe6 <- glmer(alt ~ ch.agua + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe7 <- glmer(alt ~ ch.elec + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe8 <- glmer(alt ~ ch.dren + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe9 <- glmer(alt ~ ch.del + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe10 <- glmer(alt ~ ch.hom + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))

stargazer(panfe1, panfe2, panfe3, panfe4, panfe5, panfe6, panfe7, panfe8, panfe9, panfe10, type = "html", out = paste(out, "PAN_FE.html", sep = "/"), flip = T)

#PRI####
pri1 <- lm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI")
pri2 <- lm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI")
pri3 <- lm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI")
pri4 <- lm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI")
pri5 <- lm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI")

pri6 <- glm(alt ~ ch.agua + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri7 <- glm(alt ~ ch.elec + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri8 <- glm(alt ~ ch.dren + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri9 <- glm(alt ~ ch.del + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri10 <- glm(alt ~ ch.hom + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))

stargazer(pri1, pri2, pri3, pri4, pri5, pri6, pri7, pri8, pri9, pri10, type = "html", out = paste(out, "PRI.html", sep = "/"), flip = T)

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife2 <- felm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife3 <- felm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife4 <- felm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife5 <- felm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")

prife6 <- glmer(alt ~ ch.agua + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife7 <- glmer(alt ~ ch.elec + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife8 <- glmer(alt ~ ch.dren + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife9 <- glmer(alt ~ ch.del + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife10 <- glmer(alt ~ ch.hom + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))

stargazer(prife1, prife2, prife3, prife4, prife5, prife6, prife7, prife8, prife9, prife10, type = "html", out = paste(out, "PRI_FE.html", sep = "/"), flip = T)

#PRD####
prd1 <- lm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD")
prd2 <- lm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD")
prd3 <- lm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD")
prd4 <- lm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD")
prd5 <- lm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD")

prd6 <- glm(alt ~ ch.agua + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd7 <- glm(alt ~ ch.elec + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd8 <- glm(alt ~ ch.dren + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd9 <- glm(alt ~ ch.del + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd10 <- glm(alt ~ ch.hom + log(Pob_Total) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))

stargazer(prd1, prd2, prd3, prd4, prd5, prd6, prd7, prd8, prd9, prd10, type = "html", out = paste(out, "PRD.html", sep = "/"), flip = T)

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe2 <- felm(inc.ch ~ ch.elec + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe3 <- felm(inc.ch ~ ch.dren + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe4 <- felm(inc.ch ~ ch.del + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe5 <- felm(inc.ch ~ ch.hom + log(Pob_Total) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")

prdfe6 <- glmer(alt ~ ch.agua + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe7 <- glmer(alt ~ ch.elec + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe8 <- glmer(alt ~ ch.dren + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe9 <- glmer(alt ~ ch.del + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe10 <- glmer(alt ~ ch.hom + log(Pob_Total) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))

stargazer(prdfe1, prdfe2, prdfe3, prdfe4, prdfe5, prdfe6, prdfe7, prdfe8, prdfe9, prdfe10, type = "html", out = paste(out, "PRD_FE.html", sep = "/"), flip = T)
beep(8)