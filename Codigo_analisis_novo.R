rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"
out <- "/home/dhjs/Documentos/R_projects/tesis/out"

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
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p1

p2 <- ggplot(data, aes(x = as.factor(year), y = tasa_tot_del, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p2

p3 <- ggplot(data, aes(x = as.factor(year), y = tasa_hom, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p3

p4 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = tasa_agua, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p4

p5 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = tasa_elec, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
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
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
d1
ggsave("box_inc_share.png", path = out, dpi = 300)

#Graficas de bienes públicos####
party <- c("PAN" = "#153588", "PRI" = "#E13A27", "PRD" = "#F6D626")

#pb = txtProgressBar(min=1, max=length(fin), style=3)
#serv <- c("ch.agua", "ch.elec", "ch.dren", "ch.del", "ch.hom")

#Boxplot####
d2 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.agua, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
d2
ggsave("box_agua.png", path = out, dpi = 300)

d3 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.elec, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
d3
ggsave("box_elec.png", path = out, dpi = 300)

d4 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.dren, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
d4
ggsave("box_dren.png", path = out, dpi = 300)

d5 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.del, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
d5
ggsave("box_delitos.png", path = out, dpi = 300)

d6 <- ggplot(data %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.hom, col = as.factor(alt))) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
d6
ggsave("box_hom.png", path = out, dpi = 300)
#Scatterplot####
#agua
a7 <- ggplot(data %>% filter(!is.na(alt) & #ch.agua <= 100 & 
                              #ch.agua < quantile(ch.agua, .6, na.rm = T)  & 
                              #ch.agua < mean(ch.agua, na.rm = T) & 
                               #ch.agua < quantile(ch.agua, .25, na.rm = T) & 
                              ch.agua >= -25 & ch.agua <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F)
  facet_grid(. ~ win_top)
a7 #pue que funcione
ggsave("s_agua.png", path = out, dpi = 300)

a7a <- ggplot(data %>% filter(!is.na(alt) & ch.agua >= -25 & ch.agua <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
a7a
ggsave("s_agua_alt.png", path = out, dpi = 300)

a71 <- ggplot(data %>% filter(!is.na(alt) & ch.agua >= 25 & ch.agua <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
a71
ggsave("s_agua_mayor.png", path = out, dpi = 300)

a71a <- ggplot(data %>% filter(!is.na(alt) & ch.agua >= 25 & ch.agua <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
a71a
ggsave("s_agua_mayor_alt.png", path = out, dpi = 300)

a72 <- ggplot(data %>% filter(!is.na(alt) & ch.agua > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
a72
ggsave("s_agua_may_100.png", path = out, dpi = 300)

a72a <- ggplot(data %>% filter(!is.na(alt) & ch.agua > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
a72a
ggsave("s_agua_may_100_alt.png", path = out, dpi = 300)

a73 <- ggplot(data %>% filter(!is.na(alt) & ch.agua <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
a73 ## estás tienen muucho mayor pendiente
#puede ser por menor dispersión entre los datos
ggsave("s_agua_menor.png", path = out, dpi = 300)

a73a <- ggplot(data %>% filter(!is.na(alt) & ch.agua <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.agua, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) + 
  theme_dark()
a73a
ggsave("s_agua_menor_alt.png", path = out, dpi = 300)

#Elec
e7 <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
e7 
ggsave("s_elec.png", path = out, dpi = 300)

e7a <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
e7a
ggsave("s_elec_alt.png", path = out, dpi = 300)

e71 <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= 25 & ch.elec <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
e71
ggsave("s_elec_mayor.png", path = out, dpi = 300)

e71a <- ggplot(data %>% filter(!is.na(alt) & ch.elec >= 25 & ch.elec <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
e71a
ggsave("s_elec_mayor_alt.png", path = out, dpi = 300)

e72 <- ggplot(data %>% filter(!is.na(alt) & ch.elec > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
e72
ggsave("s_elec_may_100.png", path = out, dpi = 300)

e72a <- ggplot(data %>% filter(!is.na(alt) & ch.elec > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
e72a
ggsave("s_elec_may_100_alt.png", path = out, dpi = 300)

e73 <- ggplot(data %>% filter(!is.na(alt) & ch.elec <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
e73 
ggsave("s_elec_menor.png", path = out, dpi = 300)

e73a <- ggplot(data %>% filter(!is.na(alt) & ch.elec <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.elec, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
e73a
ggsave("s_elec_menor_alt.png", path = out, dpi = 300)

#dren
d7 <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
d7 
ggsave("s_dren.png", path = out, dpi = 300)

d7a <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
d7a
ggsave("s_dren_alt.png", path = out, dpi = 300)

d71 <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= 25 & ch.dren <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
d71
ggsave("s_dren_mayor.png", path = out, dpi = 300)

d71a <- ggplot(data %>% filter(!is.na(alt) & ch.dren >= 25 & ch.dren <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
d71a
ggsave("s_dren_mayor_alt.png", path = out, dpi = 300)

d72 <- ggplot(data %>% filter(!is.na(alt) & ch.dren > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
d72
ggsave("s_dren_may_100.png", path = out, dpi = 300)

d72a <- ggplot(data %>% filter(!is.na(alt) & ch.dren > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
d72a
ggsave("s_dren_may_100_alt.png", path = out, dpi = 300)

d73 <- ggplot(data %>% filter(!is.na(alt) & ch.dren <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
d73 
ggsave("s_dren_menor.png", path = out, dpi = 300)

d73a <- ggplot(data %>% filter(!is.na(alt) & ch.dren <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.dren, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
d73a
ggsave("s_dren_menor_alt.png", path = out, dpi = 300)

#del
t7 <- ggplot(data %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ win_top)
t7 
ggsave("s_delitos.png", path = out, dpi = 300)

t7a <- ggplot(data %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) + 
  theme_dark()
t7a
ggsave("s_delitos_alt.png", path = out, dpi = 300)

t71 <- ggplot(data %>% filter(!is.na(alt) & ch.del >= 25 & ch.del <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
t71
ggsave("s_delitos_mayor.png", path = out, dpi = 300)

t71a <- ggplot(data %>% filter(!is.na(alt) & ch.del >= 25 & ch.del <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
t71a
ggsave("s_delitos_mayor_alt.png", path = out, dpi = 300)

t72 <- ggplot(data %>% filter(!is.na(alt) & ch.del > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
t72
ggsave("s_delitos_may_100.png", path = out, dpi = 300)

t72a <- ggplot(data %>% filter(!is.na(alt) & ch.del > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
t72a
ggsave("s_delitos_may_100_alt.png", path = out, dpi = 300)

t73 <- ggplot(data %>% filter(!is.na(alt) & ch.del <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
t73 
ggsave("s_delitos_menor.png", path = out, dpi = 300)

t73a <- ggplot(data %>% filter(!is.na(alt) & ch.del <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.del, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
t73a
ggsave("s_delitos_menor_alt.png", path = out, dpi = 300)

#hom
h7 <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
h7 
ggsave("s_hom.png", path = out, dpi = 300)

h7a <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
h7a
ggsave("s_hom_alt.png", path = out, dpi = 300)

h71 <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= 25 & ch.hom <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
h71
ggsave("s_hom_mayor.png", path = out, dpi = 300)

h71a <- ggplot(data %>% filter(!is.na(alt) & ch.hom >= 25 & ch.hom <= 100 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
h71a
ggsave("s_hom_mayor_alt.png", path = out, dpi = 300)

h72 <- ggplot(data %>% filter(!is.na(alt) & ch.hom > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
h72
ggsave("s_hom_may_100.png", path = out, dpi = 300)

h72a <- ggplot(data %>% filter(!is.na(alt) & ch.hom > 100 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
h72a
ggsave("s_hom_may_100_alt.png", path = out, dpi = 300)

h73 <- ggplot(data %>% filter(!is.na(alt) & ch.hom <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(alt))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(. ~ win_top)
h73 
ggsave("s_hom_menor.png", path = out, dpi = 300)

h73a <- ggplot(data %>% filter(!is.na(alt) & ch.hom <= -25 & win_top != "Otros"), aes(x = inc.share, y = ch.hom, col = as.factor(win_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_fill_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt) +
  theme_dark()
h73a
ggsave("s_hom_menor_alt.png", path = out, dpi = 300)