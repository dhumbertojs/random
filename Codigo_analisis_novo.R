rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"

data <- read.csv(paste(inp, "data.csv", sep = "/"))
data <- data %>% 
  mutate(
    PAN.s = PAN/total,
    PRI.s = PRI/total,
    PRD.s = PRD/total,
    PAN_PRD.s = PAN_PRD/total,
    
    inc.share = ifelse(win_top == "PAN", PAN.s,
                       ifelse(win_top == "PRI", PRI.s,
                              ifelse(win_top == "PRD", PRD.s, 
                                     ifelse(win_top == "PAN_PRD", PAN_PRD.s, NA))))
  ) %>% 
  select(-muniYear1)

bp <- read.csv(paste(inp, "Bienes_publicos.csv", sep = "/"))
colnames(bp)[4] <- "tot_del"
bp <- select(bp, -c(year, muni))
bp <- bp %>% select(-(13:32))
##Si se utilizan las lag variables hay que agrupar por año y hacer un ifelse para calcular el promedio, será conveniente agrupar por año y estado ??

data <- data %>% 
  inner_join(bp, by = "muniYear")
  

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    prevyear = lag(year, n = 1),
    difY = year - prevyear
    ) %>% 
  ungroup()

summary(data)

p1 <- ggplot(data %>% filter(!is.na(alt)) , aes(x = as.factor(year), y = inc.share, col = factor(alt))) + 
  geom_boxplot()
p1

p2 <- ggplot(data, aes(x = as.factor(year), y = tasa_tot_del, col = as.factor(alt))) +
  geom_boxplot()
p2

p3 <- ggplot(data, aes(x = as.factor(year), y = tasa_hom, col = as.factor(alt))) +
  geom_boxplot()
p3

p4 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = tasa_agua, col = as.factor(alt))) +
  geom_boxplot()
p4

p5 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = tasa_elec, col = as.factor(alt))) +
  geom_boxplot()
p5

##hay que poner los cambios como porcentajes

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

d1 <- ggplot(data %>% filter(!is.na(alt)) , aes(x = as.factor(year), y = inc.share, col = factor(alt))) + 
  geom_boxplot()
d1

#Graficas de bienes públicos####
#Boxplot####
d2 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.agua, col = as.factor(alt))) +
  geom_boxplot()
d2

d3 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.elec, col = as.factor(alt))) +
  geom_boxplot()
d3

d4 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.dren, col = as.factor(alt))) +
  geom_boxplot()
d4

d5 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.del, col = as.factor(alt))) +
  geom_boxplot()
d5

d6 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.hom, col = as.factor(alt))) +
  geom_boxplot()
d6
#Scatterplot####
#agua
a7 <- ggplot(data %>% filter(!is.na(alt) & #ch.agua <= 100 & 
                              #ch.agua < quantile(ch.agua, .6, na.rm = T)  & 
                              #ch.agua < mean(ch.agua, na.rm = T) & 
                               #ch.agua < quantile(ch.agua, .25, na.rm = T) & 
                              ch.agua >= -25 & ch.agua <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
a7 #pue que funcione

a7a <- ggplot(data %>% filter(!is.na(alt) & ch.agua >= -25 & ch.agua <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
a7a

a71 <- ggplot(data %>% filter(!is.na(alt) & ch.agua >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
a71

a71a <- ggplot(data %>% filter(!is.na(alt) & ch.agua >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
a71a

a72 <- ggplot(data %>% filter(!is.na(alt) & ch.agua <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
a72 ## estás tienen muucho mayor pendiente
#puede ser por menor dispersión entre los datos

a72a <- ggplot(data %>% filter(!is.na(alt) & ch.agua <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
a72a

#Elec
e7 <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
e7 

e7a <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
e7a

e71 <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
e71

e71a <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
e71a

e72 <- ggplot(data %>% filter(!is.na(alt) & ch.elec <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
e72 

e72a <- ggplot(data %>% filter(!is.na(alt) & ch.elec <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
e72a

#dren
d7 <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
d7 

d7a <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
d7a

d71 <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
d71

d71a <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
d71a

d72 <- ggplot(data %>% filter(!is.na(alt) & ch.dren <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
d72 

d72a <- ggplot(data %>% filter(!is.na(alt) & ch.dren <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
d72a

#del
t7 <- ggplot(data %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
t7 

t7a <- ggplot(data %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
t7a

t71 <- ggplot(data %>% filter(!is.na(alt) & ch.del >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
t71

t71a <- ggplot(data %>% filter(!is.na(alt) & ch.del >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
t71a

t72 <- ggplot(data %>% filter(!is.na(alt) & ch.del <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
t72 

t72a <- ggplot(data %>% filter(!is.na(alt) & ch.del <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
t72a

#hom
h7 <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
h7 

h7a <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
h7a

h71 <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
h71

h71a <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
h71a

h72 <- ggplot(data %>% filter(!is.na(alt) & ch.hom <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ win_top)
h72 

h72a <- ggplot(data %>% filter(!is.na(alt) & ch.hom <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
h72a
