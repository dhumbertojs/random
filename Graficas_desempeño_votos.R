rm(list=ls())
setwd("~")

inp = "/Users/Emmanuel RB/Documents/tesis/Datos"
out = "/Users/Emmanuel RB/Documents/tesis/out"
####Paquetes####
library(dplyr)
library(ggplot2)
require(scales)
####Datos####
del <- read.csv(paste(inp, "delitos_b.csv", sep = "/"))
ser <- read.csv(paste(inp, "servicios_b.csv", sep = "/"))

del <- del %>% 
  group_by(muni) %>% 
  mutate(
    PAN_lag = lag(PAN, n=1, order_by = muniYear),
    PRI_lag = lag(PRI, n=1, order_by = muniYear),
    PRD_lag = lag(PRD, n=1, order_by = muniYear),
    PAN_PRD_lag = lag(PAN_PRD, n=1, order_by = muniYear),
    
    PAN_perf = PAN - PAN_lag, 
    PRI_perf = PRI - PRI_lag,
    PRD_perf = PRD - PRD_lag, 
    PAN_PRD_perf = PAN_PRD - PAN_PRD_lag
  ) %>% 
  ungroup(muni)

ser <- ser %>% 
  group_by(muni) %>% 
  mutate(
    PAN_lag = lag(PAN, n=1, order_by = muniYear),
    PRI_lag = lag(PRI, n=1, order_by = muniYear),
    PRD_lag = lag(PRD, n=1, order_by = muniYear),
    PAN_PRD_lag = lag(PAN_PRD, n=1, order_by = muniYear),
    
    PAN_perf = PAN - PAN_lag, 
    PRI_perf = PRI - PRI_lag,
    PRD_perf = PRD - PRD_lag, 
    PAN_PRD_perf = PAN_PRD - PAN_PRD_lag
  ) %>% 
  ungroup(muni)

####Gráficas nuevas####
#scatter delitos por partido####

delPAN1 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_total , 0.9, na.rm = T)), aes(x = PAN_perf, y = change_total, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Total de delitos", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

delPAN2 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_hom , 0.9, na.rm = T)), aes(x = PAN_perf, y = change_hom, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Homicidios", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(delPAN1, delPAN2)
annotate_figure(zscat_del, top = text_grob("Tipo de delito y desempeño electoral del PAN (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sdel_PAN.png", path = out, dpi = 320)

delPRI1 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_total , 0.9, na.rm = T)), aes(x = PRI_perf, y = change_total, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Total de delitos", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

delPRI2 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_hom , 0.9, na.rm = T)), aes(x = PRI_perf, y = change_hom, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Homicidios", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(delPRI1, delPRI2)
annotate_figure(zscat_del, top = text_grob("Tipo de delito y desempeño electoral del PRI (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sdel_PRI.png", path = out, dpi = 320)

delPRD1 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_total , 0.9, na.rm = T)), aes(x = PRD_perf, y = change_total, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Total de delitos", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

delPRD2 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_hom , 0.9, na.rm = T)), aes(x = PRD_perf, y = change_hom, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Homicidios", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(delPRD1, delPRD2)
annotate_figure(zscat_del, top = text_grob("Tipo de delito y desempeño electoral del PRD (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sdel_PRD.png", path = out, dpi = 320)

delPAN_PRD1 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_total , 0.9, na.rm = T)), aes(x = PAN_PRD_perf, y = change_total, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Total de delitos", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

delPAN_PRD2 <- ggplot(data = subset(del, !is.na(alt) & win_top != "Otros" & quantile(change_hom , 0.9, na.rm = T)), aes(x = PAN_PRD_perf, y = change_hom, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Homicidios", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(delPAN_PRD1, delPAN_PRD2)
annotate_figure(zscat_del, top = text_grob("Tipo de delito y desempeño electoral del PAN_PRD (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sdel_PAN_PRD.png", path = out, dpi = 320)

#scatter servicios por partido####

serPAN1 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_agua , 0.9, na.rm = T)), aes(x = PAN_perf, y = change_agua, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Agua", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPAN2 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_elec , 0.9, na.rm = T)), aes(x = PAN_perf, y = change_elec, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Electricidad", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPAN3 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_dren , 0.9, na.rm = T)), aes(x = PAN_perf, y = change_dren, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Drenaje", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(serPAN1, serPAN2, serPAN3)
annotate_figure(zscat_del, top = text_grob("Tipo de servicio y desempeño electoral del PAN (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sser_PAN.png", path = out, dpi = 320)

serPRI1 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_agua , 0.9, na.rm = T)), aes(x = PRI_perf, y = change_agua, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Agua", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPRI2 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_elec , 0.9, na.rm = T)), aes(x = PRI_perf, y = change_elec, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Electricidad", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPRI3 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_dren , 0.9, na.rm = T)), aes(x = PRI_perf, y = change_dren, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Drenaje", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(serPRI1, serPRI2, serPRI3)
annotate_figure(zscat_del, top = text_grob("Tipo de servicio y desempeño electoral del PRI (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sser_PRI.png", path = out, dpi = 320)

serPRD1 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_agua , 0.9, na.rm = T)), aes(x = PRD_perf, y = change_agua, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Agua", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPRD2 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_elec , 0.9, na.rm = T)), aes(x = PRD_perf, y = change_elec, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Electricidad", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPRD3 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_dren , 0.9, na.rm = T)), aes(x = PRD_perf, y = change_dren, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Drenaje", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(serPRD1, serPRD2, serPRD3)
annotate_figure(zscat_del, top = text_grob("Tipo de servicio y desempeño electoral del PRD (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sser_PRD.png", path = out, dpi = 320)

serPAN_PRD1 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_agua , 0.9, na.rm = T)), aes(x = PAN_PRD_perf, y = change_agua, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Agua", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPAN_PRD2 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_elec , 0.9, na.rm = T)), aes(x = PAN_PRD_perf, y = change_elec, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Electricidad", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

serPAN_PRD3 <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros" & quantile(change_dren , 0.9, na.rm = T)), aes(x = PAN_PRD_perf, y = change_dren, col = factor(alt))) +
  geom_point(alpha = 0.4) + ylim(-200, 200) + scale_x_continuous(labels = comma) +
  labs(y = "Drenaje", x = "Desempeño electoral", col = "Alt") +
  geom_smooth(method = lm, se= F)

zscat_del <- ggarrange(serPAN_PRD1, serPAN_PRD2, serPAN_PRD3)
annotate_figure(zscat_del, top = text_grob("Tipo de servicio y desempeño electoral del PAN_PRD (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("sser_PAN_PRD.png", path = out, dpi = 320)

#este es otro tema####
try <- ggplot(data = subset(ser, !is.na(alt) & win_top != "Otros"), aes(x = PAN_perf, y = change_dren#, col = factor(alt)
                                                                        )) +
  geom_point(alpha = 0.4) + ylim(-200, 200) +
  labs(y = "", x = "", col = "Alt") +
  geom_smooth()#(method = lm, se= F)

