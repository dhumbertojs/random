  rm(list = ls())
  setwd("~")
  
  library(dplyr)
  library(ggplot2)
  library(stargazer)
  library(lfe)
  library(beepr)
  
  inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"
  out <- "/home/dhjs/Documentos/R_projects/tesis/out"
  
  data <- read.csv(paste(inp, "data.csv", sep = "/"))
  
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
      ch.dren = ifelse(!is.na(tasa_dren) & !is.na(l.dren), ((tasa_dren - l.dren)/l.dren) * 100, NA),
      
      conco = ifelse(as.character(win_top) == as.character(wintop_state), 1, 0)
    )
  
  summary(data)
  
datos <- data %>% 
    filter(
      !is.infinite(ch.del) & !is.infinite(ch.hom) & !is.infinite(ch.agua) & !is.infinite(ch.elec) & !is.infinite(ch.dren)
    )
beep(2)  

  d1 <- ggplot(datos %>% filter(!is.na(alt)) , aes(x = as.factor(year), y = inc.share, col = factor(alt))) + 
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  d1
  ggsave("box_inc_share.png", path = out, dpi = 300)
  
  #Graficas de bienes públicos####
  #pb = txtProgressBar(min=1, max=length(fin), style=3)
  #serv <- c("ch.agua", "ch.elec", "ch.dren", "ch.del", "ch.hom")
  
  #Boxplot####
  d2 <- ggplot(datos %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.agua, col = as.factor(alt))) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  d2
  ggsave("box_agua.png", path = out, dpi = 300)
  
  d3 <- ggplot(datos %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.elec, col = as.factor(alt))) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  d3
  ggsave("box_elec.png", path = out, dpi = 300)
  
  d4 <- ggplot(datos %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.dren, col = as.factor(alt))) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  d4
  ggsave("box_dren.png", path = out, dpi = 300)
  
  d5 <- ggplot(datos %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.del, col = as.factor(alt))) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  d5
  ggsave("box_delitos.png", path = out, dpi = 300)
  
  d6 <- ggplot(datos %>% filter(!is.na(alt)), aes(x = as.factor(year), y = ch.hom, col = as.factor(alt))) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  d6
  ggsave("box_hom.png", path = out, dpi = 300)
  #Scatterplot####
alt <- "/home/dhjs/Documentos/R_projects/tesis/out/alt"
  
path = partidos <- "/home/dhjs/Documentos/R_projects/tesis/out/party"
  
ap <- "/home/dhjs/Documentos/R_projects/tesis/out/alt+party"
  
  #Alternancia####
  #Agua
  a7 <- ggplot(datos %>% filter(!is.na(alt) & #ch.agua <= 100 & 
                                #ch.agua < yuantile(ch.agua, .6, na.rm = T)  & 
                                #ch.agua < mean(ch.agua, na.rm = T) & 
                                 #ch.agua < yuantile(ch.agua, .25, na.rm = T) & 
                                ch.agua >= -25 & ch.agua <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_agua.png", path = alt, dpi = 300)
  
  a71 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 25 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_agua_mayor.png", path = alt, dpi = 300)
  
  a72 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_agua_may_100.png", path = alt, dpi = 300)
  
  a73 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_agua_menor.png", path = alt, dpi = 300)
  
  a74 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 0 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_agua_pos.png", path = alt, dpi = 300)
  
  a75 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua <= 0 & ch.agua >= -100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_agua_neg.png", path = alt, dpi = 300)
  
  a76 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -100 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_agua_int.png", path = alt, dpi = 300)
  
  #Electricidad
  e7 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_elec.png", path = alt, dpi = 300)
  
  e71 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 25 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_elec_mayor.png", path = alt, dpi = 300)
  
  e72 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_elec_may_100.png", path = alt, dpi = 300)
  
  e73 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_elec_menor.png", path = alt, dpi = 300)
  
  e74 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 0 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_elec_pos.png", path = alt, dpi = 300)
  
  e75 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec <= 0 & ch.elec >= -100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_elec_neg.png", path = alt, dpi = 300)
  
  e76 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -100 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_elec_int.png", path = alt, dpi = 300)
  
  #drenaje
  d7 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_dren.png", path = alt, dpi = 300)
  
  d71 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 25 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_dren_mayor.png", path = alt, dpi = 300)
  
  d72 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_dren_may_100.png", path = alt, dpi = 300)
  
  d73 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_dren_menor.png", path = alt, dpi = 300)
  
  d74 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 0 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_dren_posr.png", path = alt, dpi = 300)
  
  d75 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_dren_neg.png", path = alt, dpi = 300)
  
  d76 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_dren_int.png", path = alt, dpi = 300)
  
  #delitos
  t7 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos.png", path = alt, dpi = 300)
  
  t71 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 25 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos_mayor.png", path = alt, dpi = 300)
  
  t72 <- ggplot(datos %>% filter(!is.na(alt) & ch.del > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos_may_100.png", path = alt, dpi = 300)
  
  t73 <- ggplot(datos %>% filter(!is.na(alt) & ch.del <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos_menor.png", path = alt, dpi = 300)
  
  t74 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 0 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos_pos.png", path = alt, dpi = 300)
  
  t75 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos_gen.png", path = alt, dpi = 300)
  
  t76 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos_int.png", path = alt, dpi = 300)
  
  #homicidios
  h7 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_hom.png", path = alt, dpi = 300)
  
  h71 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 25 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_hom_mayor.png", path = alt, dpi = 300)
  
  h72 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_hom_may_100.png", path = alt, dpi = 300)
  
  h73 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_hom_menor.png", path = alt, dpi = 300)
  
  h74 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 0 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_hom_pos.png", path = alt, dpi = 300)
  
  h75 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -100 & ch.hom <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_hom_neg.png", path = alt, dpi = 300)
  
  h76 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 25 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(alt))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
  ggsave("s_hom_int.png", path = alt, dpi = 300)
  
  beep(2)
  #Partidos por alternancia####
partx <- c("PAN" = "#153588", "PRI" = "#E13A27", "PRD" = "#F6D626")
    #Agua
  a7a <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -25 & ch.agua <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    facet_grid(. ~ alt) +
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_alt.png", path = ap, dpi = 300)
  
  a71a <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 25 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_agua_mayor_alt.png", path = ap, dpi = 300)
  
  a72a <- ggplot(datos %>% filter(!is.na(alt) & ch.agua > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_agua_may_100_alt.png", path = ap, dpi = 300)
  
  a73a <- ggplot(datos %>% filter(!is.na(alt) & ch.agua <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) + 
    theme_dark()
  ggsave("s_agua_menor_alt.png", path = ap, dpi = 300)
  
  a74a <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 0 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_agua_mayor_alt_pos.png", path = ap, dpi = 300)
  
  a75a <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -100 & ch.agua <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_agua_mayor_alt_neg.png", path = ap, dpi = 300)
  
  a76a <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -100 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_agua_mayor_alt_int.png", path = ap, dpi = 300)
  
  #Electricidad
  e7a <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_elec_alt.png", path = ap, dpi = 300)
  
  e71a <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 25 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_elec_mayor_alt.png", path = ap, dpi = 300)
  
  e72a <- ggplot(datos %>% filter(!is.na(alt) & ch.elec > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_elec_may_100_alt.png", path = ap, dpi = 300)
  
  e73a <- ggplot(datos %>% filter(!is.na(alt) & ch.elec <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_elec_menor_alt.png", path = ap, dpi = 300)
  
  e74a <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 0 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_elec_mayor_alt_pos.png", path = ap, dpi = 300)
  
  e75a <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -100 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_elec_mayor_alt.png", path = ap, dpi = 300)
  
  e76a <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 25 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_elec_mayor_alt.png", path = ap, dpi = 300)
  
  #drenaje
  d7a <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_dren_alt.png", path = ap, dpi = 300)
  
  d71a <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 25 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_dren_mayor_alt.png", path = ap, dpi = 300)
  
  d72a <- ggplot(datos %>% filter(!is.na(alt) & ch.dren > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_dren_may_100_alt.png", path = ap, dpi = 300)
  
  d73a <- ggplot(datos %>% filter(!is.na(alt) & ch.dren > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) +
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_dren_menor.png", path = ap, dpi = 300)
  
  d74a <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 0 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_dren_mayor_alt_pos.png", path = ap, dpi = 300)
  
  d75a <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_dren_mayor_alt_neg.png", path = ap, dpi = 300)
  
  d76a <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_dren_mayor_alt_int.png", path = ap, dpi = 300)
  
  #delitos
  t7a <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) + 
    theme_dark()
  ggsave("s_delitos_alt.png", path = ap, dpi = 300)
  
  t71a <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 25 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_delitos_mayor_alt.png", path = ap, dpi = 300)
  
  t72a <- ggplot(datos %>% filter(!is.na(alt) & ch.del > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_delitos_may_100_alt.png", path = ap, dpi = 300)
  
  t73a <- ggplot(datos %>% filter(!is.na(alt) & ch.del <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_delitos_menor_alt.png", path = ap, dpi = 300)
  
  t74a <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 0 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_delitos_mayor_alt_pos.png", path = ap, dpi = 300)
  
  t75a <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_delitos_mayor_alt_neg.png", path = ap, dpi = 300)
  
  t76a <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_delitos_mayor_alt_int.png", path = ap, dpi = 300)
  
  #homicidios
  h7a <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_hom_alt.png", path = ap, dpi = 300)
  
  h71a <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 25 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_hom_mayor_alt.png", path = ap, dpi = 300)
  
  h72a <- ggplot(datos %>% filter(!is.na(alt) & ch.hom > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_hom_may_100_alt.png", path = ap, dpi = 300)
  
  h73a <- ggplot(datos %>% filter(!is.na(alt) & ch.hom <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_hom_menor_alt.png", path = ap, dpi = 300)
  
  h74a <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 0 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_hom_mayor_alt_pos.png", path = ap, dpi = 300)
  
  h75a <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -100 & ch.hom <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_hom_mayor_alt_neg.png", path = ap, dpi = 300)
  
  
  h76a <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -100 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    facet_grid(. ~ alt) +
    theme_dark()
  ggsave("s_hom_mayor_alt_int.png", path = ap, dpi = 300)
  beep(2)
  #Partidos####
  #Agua
  a7b <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -25 & ch.agua <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_partx.png", path = partidos, dpi = 300)
  
  a71b <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 25 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_mayor_partx.png", path = partidos, dpi = 300)
  
  a72b <- ggplot(datos %>% filter(!is.na(alt) & ch.agua > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_may_100_partx.png", path = partidos, dpi = 300)
  
  a73b <- ggplot(datos %>% filter(!is.na(alt) & ch.agua <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_menor_partx.png", path = partidos, dpi = 300)

  a74b <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 0 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_mayor_partx_pos.png", path = partidos, dpi = 300)
  
  a75b <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -100 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_mayor_partx_neg.png", path = partidos, dpi = 300)
  
  a76b <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -100 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_agua_mayor_partx_int.png", path = partidos, dpi = 300)
    
  #Electricidad
  
  e7b <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_elec_partx.png", path = partidos, dpi = 300)
  
  e71b <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 25 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_elec_mayor_partx.png", path = partidos, dpi = 300)

  e72b <- ggplot(datos %>% filter(!is.na(alt) & ch.elec > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_elec_may_100_partx.png", path = partidos, dpi = 300)
  
  e73b <- ggplot(datos %>% filter(!is.na(alt) & ch.elec <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_elec_menor_partx.png", path = partidos, dpi = 300)
  
  e74b <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 0 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_elec_mayor_partx_pos.png", path = partidos, dpi = 300)
  
  e75b <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -100 & ch.elec <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_elec_mayor_partx_neg.png", path = partidos, dpi = 300)
  
  e76b <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -100 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_elec_mayor_partx_int.png", path = partidos, dpi = 300)
  
  #drenaje
  d7b <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_dren_partx.png", path = partidos, dpi = 300)
  
  d71b <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 25 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_dren_mayor_partx.png", path = partidos, dpi = 300)
  
  d72b <- ggplot(datos %>% filter(!is.na(alt) & ch.dren > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_dren_may_100_partx.png", path = partidos, dpi = 300)
  
  d73b <- ggplot(datos %>% filter(!is.na(alt) & ch.dren <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_dren_menor_partx.png", path = partidos, dpi = 300)
  
  d74b <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 0 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_dren_mayor_partx_pos.png", path = partidos, dpi = 300)
  
  d75b <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_dren_mayor_partx_neg.png", path = partidos, dpi = 300)
  
  d76b <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren, col = as.factor(inc_top))) +
    geom_point(aes(alpha = 0.2)) +
    geom_smooth(method = "lm", se = F) + 
    scale_colour_manual(values = partx, name = "Partido") +
    theme_dark()
  ggsave("s_dren_mayor_partx_int.png", path = partidos, dpi = 300)
  
  #delitos
t7b <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_delitos_partx.png", path = partidos, dpi = 300)

t71b <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 25 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_delitos_mayor_partx.png", path = partidos, dpi = 300)

t72b <- ggplot(datos %>% filter(!is.na(alt) & ch.del > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_delitos_may_100_partx.png", path = partidos, dpi = 300)

t73b <- ggplot(datos %>% filter(!is.na(alt) & ch.del <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_delitos_menor_partx.png", path = partidos, dpi = 300)

t74b <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 0 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_delitos_mayor_partx_pos.png", path = partidos, dpi = 300)

t75b <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_delitos_mayor_partx_neg.png", path = partidos, dpi = 300)

t76b <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_delitos_mayor_partx_int.png", path = partidos, dpi = 300)

#homicidios
h7b <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_hom_partx.png", path = partidos, dpi = 300)

h71b <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 25 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_hom_mayor_partx.png", path = partidos, dpi = 300)

h72b <- ggplot(datos %>% filter(!is.na(alt) & ch.hom > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_hom_may_100_partx.png", path = partidos, dpi = 300)

h73b <- ggplot(datos %>% filter(!is.na(alt) & ch.hom <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_hom_menor_partx.png", path = partidos, dpi = 300)

h74b <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 0 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_hom_mayor_partx_pos.png", path = partidos, dpi = 300)

h75b <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -100 & ch.hom <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_hom_mayor_partx_neg.png", path = partidos, dpi = 300)

h76b <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -100 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom, col = as.factor(inc_top))) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + 
  scale_colour_manual(values = partx, name = "Partido") +
  theme_dark()
ggsave("s_hom_mayor_partx_int.png", path = partidos, dpi = 300)
beep(2)

#Normal####
nom <- "/home/dhjs/Documentos/R_projects/tesis/out/normal"
#Agua
na7 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -25 & ch.agua <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_agua_n.png", path = nom, dpi = 300)

na71 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 25 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_agua_mayor_n.png", path = nom, dpi = 300)

na72 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_agua_may_100_n.png", path = nom, dpi = 300)

na73 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_agua_menor_n.png", path = nom, dpi = 300)

na74 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= 0 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_agua_mayor_n_pos.png", path = nom, dpi = 300)

na75 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -100 & ch.agua <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_agua_mayor_n_neg.png", path = nom, dpi = 300)

na76 <- ggplot(datos %>% filter(!is.na(alt) & ch.agua >= -100 & ch.agua <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.agua)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_agua_mayor_n_int.png", path = nom, dpi = 300)

#Electricidad
ne7 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -25 & ch.elec <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_elec_n.png", path = nom, dpi = 300)

ne71 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 25 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_elec_mayor_n.png", path = nom, dpi = 300)

ne72 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_elec_may_100_n.png", path = nom, dpi = 300)

ne73 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_elec_menor_n.png", path = nom, dpi = 300)

ne74 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= 0 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_elec_mayor_n_pos.png", path = nom, dpi = 300)

ne75 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -100 & ch.elec <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_elec_mayor_n_neg.png", path = nom, dpi = 300)

ne76 <- ggplot(datos %>% filter(!is.na(alt) & ch.elec >= -100 & ch.elec <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.elec)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_elec_mayor_n_int.png", path = nom, dpi = 300)

#drenaje
nd7 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -25 & ch.dren <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_dren_n.png", path = nom, dpi = 300)

nd71 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 25 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_dren_mayor_n.png", path = nom, dpi = 300)

nd72 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_dren_may_100_n.png", path = nom, dpi = 300)

nd73 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_dren_menor_n.png", path = nom, dpi = 300)

nd74 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= 0 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_dren_mayor_n_pos.png", path = nom, dpi = 300)

nd75 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_dren_mayor_n_neg.png", path = nom, dpi = 300)

nd76 <- ggplot(datos %>% filter(!is.na(alt) & ch.dren >= -100 & ch.dren <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.dren)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_dren_mayor_n_int.png", path = nom, dpi = 300)

#delitos
nt7 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -25 & ch.del <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) + #+ facet_grid(. ~ inc_top)
  ggsave("s_delitos_n.png", path = nom, dpi = 300)

nbt71 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 25 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_delitos_mayor_n.png", path = nom, dpi = 300)

nbt72 <- ggplot(datos %>% filter(!is.na(alt) & ch.del > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_delitos_may_100_n.png", path = nom, dpi = 300)

nt73 <- ggplot(datos %>% filter(!is.na(alt) & ch.del <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_delitos_menor_n.png", path = nom, dpi = 300)

nbt74 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= 0 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_delitos_mayor_n_pos.png", path = nom, dpi = 300)

nbt75 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_delitos_mayor_n_neg.png", path = nom, dpi = 300)

nbt76 <- ggplot(datos %>% filter(!is.na(alt) & ch.del >= -100 & ch.del <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.del)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_delitos_mayor_n_int.png", path = nom, dpi = 300)

#homicidios
nh7 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -25 & ch.hom <= 25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_hom_n.png", path = nom, dpi = 300)

nh71 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 25 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_hom_mayor_n.png", path = nom, dpi = 300)

nh72 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom > 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_hom_may_100_n.png", path = nom, dpi = 300)

nh73 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom <= -25 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_hom_menor_n.png", path = nom, dpi = 300)

nh74 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= 0 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_hom_mayor_n_pos.png", path = nom, dpi = 300)

nh75 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -100 & ch.hom <= 0 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_hom_mayor_n_neg.png", path = nom, dpi = 300)

nh76 <- ggplot(datos %>% filter(!is.na(alt) & ch.hom >= -100 & ch.hom <= 100 & inc_top != "Otros"), aes(y = inc.ch, x = ch.hom)) +
  geom_point(aes(alpha = 0.2)) +
  geom_smooth(method = "lm", se = F) #+ facet_grid(. ~ inc_top)
ggsave("s_hom_mayor_n_int.png", path = nom, dpi = 300)
beep(2)

#Modelos####
t1 <- lm(inc.ch ~ ch.agua, datos)
t2 <- lm(inc.ch ~ ch.elec, datos)
t3 <- lm(inc.ch ~ ch.dren, datos)
t4 <- lm(inc.ch ~ ch.hom, datos)
t5 <- lm(inc.ch ~ ch.del, datos)
stargazer(t1, t2, t3, t4, t5, type = "html", out = paste(out, "try.html", sep = "/"), flip = T)

m1 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del, datos)
m2 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt, datos)
m3 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco, datos)
m4 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + conco, datos)
m5 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + PAN_ofi + PRI_ofi + PRD_ofi, datos)
m6 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco + PAN_ofi + PRI_ofi + PRD_ofi, datos)
m7 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi, datos)
m8 <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi + alt + conco, datos)
stargazer(m1, m2, m3, m4, m5, m6, m7, m8, type = "html", out = paste(out, "table.html", sep = "/"), flip = T)

m1a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
m2a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
m3a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
m4a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + conco, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
m5a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
m6a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
m7a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
m8a <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi + alt + conco, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
stargazer(m1a, m2a, m3a, m4a, m5a, m6a, m7a, m8a, type = "html", out = paste(out, "table I.html", sep = "/"), flip = T)

m1b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
m2b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
m3b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
m4b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + conco, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
m5b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
m6b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
m7b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
m8b <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi + alt + conco, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
stargazer(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b, type = "html", out = paste(out, "table II.html", sep = "/"), flip = T)

 # m1c <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del, subset(datos, ch.agua >= -100 & ch.agua <= 0 & ch.elec >= -100 & ch.elec <= 0 & ch.dren >= -100 & ch.dren <= 0 & ch.hom >= -100 & ch.hom <= 0 & ch.del >= -100 & ch.del <= 0))
 # m2c <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt, subset(datos, ch.agua >= -100 & ch.agua <= 0 & ch.elec >= -100 & ch.elec <= 0 & ch.dren >= -100 & ch.dren <= 0 & ch.hom >= -100 & ch.hom <= 0 & ch.del >= -100 & ch.del <= 0))
 # m3c <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= -100 & ch.agua <= 0 & ch.elec >= -100 & ch.elec <= 0 & ch.dren >= -100 & ch.dren <= 0 & ch.hom >= -100 & ch.hom <= 0 & ch.del >= -100 & ch.del <= 0))
 # m4c <- lm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi + alt, subset(datos, ch.agua >= -100 & ch.agua <= 0 & ch.elec >= -100 & ch.elec <= 0 & ch.dren >= -100 & ch.dren <= 0 & ch.hom >= -100 & ch.hom <= 0 & ch.del >= -100 & ch.del <= 0))
 # stargazer(m1c, m2c, m3c, m4c, type = "html", out = paste(out, "table IV.html", sep = "/"), flip = T)

#Modelos solo agua y homicidios####
# n1 <- lm(inc.ch ~ ch.agua + ch.hom, datos)
# n2 <- lm(inc.ch ~ ch.agua + ch.hom + alt, datos)
# n3 <- lm(inc.ch ~ ch.agua + ch.hom + PAN_ofi + PRI_ofi + PRD_ofi, datos)
# n4 <- lm(inc.ch ~ ch.agua + ch.hom + PAN_ofi + PRI_ofi + PRD_ofi + alt, datos)
# stargazer(n1, n2, n3, n4, type = "html", out = paste(out, "table_AH.html", sep = "/"), flip = T)
# 
# n1a <- lm(inc.ch ~ ch.agua + ch.hom, subset(datos, ch.agua >= 0 & ch.hom >= 0))
# n2a <- lm(inc.ch ~ ch.agua + ch.hom + alt, subset(datos, ch.agua >= 0 & ch.hom >= 0))
# n3a <- lm(inc.ch ~ ch.agua + ch.hom + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.hom >= 0))
# n4a <- lm(inc.ch ~ ch.agua + ch.hom + PAN_ofi + PRI_ofi + PRD_ofi + alt, subset(datos, ch.agua >= 0 & ch.hom >= 0))
# stargazer(n1a, n2a, n3a, n4a, type = "html", out = paste(out, "table_AH I.html", sep = "/"), flip = T)
# 
# n1b <- lm(inc.ch ~ ch.agua + ch.hom, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.hom >= 0 & ch.hom <= 100))
# n2b <- lm(inc.ch ~ ch.agua + ch.hom + alt, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.hom >= 0 & ch.hom <= 100))
# n3b <- lm(inc.ch ~ ch.agua + ch.hom + PAN_ofi + PRI_ofi + PRD_ofi, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.hom >= 0 & ch.hom <= 100))
# n4b <- lm(inc.ch ~ ch.agua + ch.hom + PAN_ofi + PRI_ofi + PRD_ofi + alt, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.hom >= 0 & ch.hom <= 100))
# stargazer(n1b, n2b, n3b, n4b, type = "html", out = paste(out, "table_AH II.html", sep = "/"), flip = T)
beep(2)
#Modelos con efectos fijos####

f1 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del | 0 | 0 | edo_year, datos)
f2 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt | 0 | 0 | edo_year, datos)
f3 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco | 0 | 0 | edo_year, datos)
f4 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + conco | 0 | 0 | edo_year, datos)
f5 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, datos)
f6 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, datos)
f7 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, datos)
f8 <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi + alt + conco | 0 | 0 | edo_year, datos)
stargazer(f1, f2, f3, f4, f5, f6, f7, f8, type = "html", out = paste(out, "table FE.html", sep = "/"), flip = T)

f1a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
f2a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
f3a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
f4a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + conco | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
f5a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
f6a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
f7a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
f8a <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi + alt + conco | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.elec >= 0 & ch.dren >= 0 & ch.hom >= 0 & ch.del >= 0))
stargazer(f1a, f2a, f3a, f4a, f5a, f6a, f7a, f8a, type = "html", out = paste(out, "table I FE.html", sep = "/"), flip = T)

f1b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
f2b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
f3b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
f4b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + conco | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
f5b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
f6b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + alt + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
f7b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + conco + PAN_ofi + PRI_ofi + PRD_ofi | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
f8b <- felm(inc.ch ~ ch.agua + ch.elec + ch.dren + ch.hom + ch.del + PAN_ofi + PRI_ofi + PRD_ofi + alt + conco | 0 | 0 | edo_year, subset(datos, ch.agua >= 0 & ch.agua <= 100 & ch.elec >= 0 & ch.elec <= 100 & ch.dren >= 0 & ch.dren <= 100 & ch.hom >= 0 & ch.hom <= 100 & ch.del >= 0 & ch.del <= 100))
stargazer(f1b, f2b, f3b, f4b, f5b, f6b, f7b, f8b, type = "html", out = paste(out, "table II FE.html", sep = "/"), flip = T)
beep(8)
