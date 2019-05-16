rm(list=ls())
setwd("~")

inp = "/Users/Emmanuel RB/Documents/tesis/Datos"
out = "/Users/Emmanuel RB/Documents/tesis/out"
####Paquetes####
library(dplyr)
library(ggplot2)
library(ggpubr)
#library (stargazer)
#library (lfe)
#library (lme4)
#library (miceadds)
#outlier.shape=NA dentro del geom boxplot para quitarle los outliers

####Datos####
data <- read.csv(paste(inp, "data.csv", sep = "/"))

censo <- read.csv(paste(inp, "censo.csv", sep = "/"))

del <- read.csv(paste(inp, "delitos.csv", sep = "/"))
colnames(del)[2] <- "Total_delitos"
del <- del %>%
  select(c("muniYear", "Total_delitos", "hom"))


edu <- read.csv(paste(inp, "educacion.csv", sep = "/"))
edu <- edu %>%
  select(-c("Clave", "year"))

viv <- read.csv(paste(inp, "viv.csv", sep = "/"))
viv <- viv %>%
  select(-c("muni", "year"))

delitos <- inner_join(data, del, by = "muniYear")
delitos <- left_join(delitos, edu, by = "muniYear")
delitos <- left_join(delitos, censo, by = "muniYear")
del_b <- delitos

servicios <- inner_join(data, viv, by = "muniYear")
servicios <- left_join(servicios, edu, by = "muniYear")
servicios <- left_join(servicios, censo, by = "muniYear")
serv_b <- servicios

summary(delitos)
summary(servicios)

####Performance de delitos####
##Los datos poblacionales están mal, ligeramente, pero en el agregado no sirven 
#delitos <- delitos %>%
#  group_by(muni) %>% 
#  mutate(
#    totalrate = (total/Pob_Total) * 100000,
#    daniorate = (danio/Pob_Total) * 100000,
#    delsexrate = (delsex/Pob_Total) * 100000,
#    homrate = (hom/Pob_Total) * 100000,
#    lesrate = (les/Pob_Total) * 100000,
#    roborate = (robo/Pob_Total) * 100000,
#    otrosrate = (otros/Pob_Total) * 100000,
    
#    alfab_rate = (mayor6_leer_esc/Pob_Total) * 100000,
#    asist_rate = (may5_asistencia/Pob_Total) * 100000,
#    nivel_rate = (may5_nivel/Pob_Total) * 100000,
#    prof_rate = (may18_prof/Pob_Total) * 100000,
#    posg_rate = (may18_posg/Pob_Total) * 100000,
#    indig_rate = (may5_indigena/Pob_Total) * 100000,
    
#    totalrate_lag = lag(totalrate, n=1, order_by = muniYear),
#    daniorate_lag = lag(daniorate, n=1, order_by = muniYear),
#    delsexrate_lag = lag(delsexrate, n=1, order_by = muniYear),
#    homrate_lag = lag(homrate, n=1, order_by = muniYear),
#    lesrate_lag = lag(lesrate, n=1, order_by = muniYear),
#    roborate_lag = lag(roborate, n=1, order_by = muniYear),
#    otrosrate_lag = lag(otrosrate, n=1, order_by = muniYear),
    
#    alfab_rate_lag = lag(alfab_rate, n=1, order_by = muniYear),
#    asist_rate_lag = lag(asist_rate, n=1, order_by = muniYear),
#    nivel_rate_lag = lag(nivel_rate, n=1, order_by = muniYear),
#    prof_rate_lag = lag(prof_rate, n=1, order_by = muniYear),
#    posg_rate_lag = lag(posg_rate, n=1, order_by = muniYear),
#    may15_escol_mean_lag = lag(may15_escol_mean, n=1, order_by = muniYear),
#    indig_rate_lag = lag(indig_rate, n=1, order_by = muniYear),
    
#    perftotrate = totalrate - totalrate_lag,
#    perfdaniorate = daniorate - daniorate_lag,
#    perfdelsexrate = delsexrate - delsexrate_lag,
#    perfhomrate = homrate - homrate_lag,
#    perflesrate = lesrate - lesrate_lag,
#    perfroborate = roborate - roborate_lag,
#    perfotrosrate = otrosrate - otrosrate_lag,
    
#    perfalfab_rate = alfab_rate - alfab_rate_lag,
#    perfasist_rate = asist_rate - asist_rate_lag,
#    perfnivel_rate = nivel_rate - nivel_rate_lag,
#    perfprof_rate = prof_rate - prof_rate_lag,
#    perfposg_rate = posg_rate - posg_rate_lag,
#    perfmay15_escol_mean = may15_escol_mean - may15_escol_mean_lag,
#    perfindig_rate = indig_rate - indig_rate_lag
#  )
####Performance servicios####
#servicios <- servicios %>%
#  group_by(muni) %>% 
#  mutate(
#    tomas_agua_pop = (tomas_dom_agua/Pob_Total) * 100000, 
#    tomas_elec_pop = (tomas_elec/Pob_Total) * 100000, 
#    jardines_pop = (jardines/Pob_Total) * 100000, 
#    parques_pop = (parques/Pob_Total) * 100000,
#    drenaje_pop = (sist_dren/Pob_Total) * 100000,
    
#    alfab_rate = (mayor6_leer_esc/Pob_Total) * 100000,
#    asist_rate = (may5_asistencia/Pob_Total) * 100000,
#    nivel_rate = (may5_nivel/Pob_Total) * 100000,
#    prof_rate = (may18_prof/Pob_Total) * 100000,
#    posg_rate = (may18_posg/Pob_Total) * 100000,
#    indig_rate = (may5_indigena/Pob_Total) * 100000,
    
#    tomas_agua_pop_lag = lag(tomas_agua_pop, n=1, order_by = muniYear),
#    tomas_elec_pop_lag = lag(tomas_elec_pop, n=1, order_by = muniYear),
#    jardines_pop_lag = lag(jardines_pop, n=1, order_by = muniYear),
#    parques_pop_lag = lag(parques_pop, n=1, order_by = muniYear),
#    drenaje_pop_lag = lag(drenaje_pop, n=1, order_by = muniYear),
    
#    alfab_rate_lag = lag(alfab_rate, n=1, order_by = muniYear),
#    asist_rate_lag = lag(asist_rate, n=1, order_by = muniYear),
#    nivel_rate_lag = lag(nivel_rate, n=1, order_by = muniYear),
#    prof_rate_lag = lag(prof_rate, n=1, order_by = muniYear),
#    posg_rate_lag = lag(posg_rate, n=1, order_by = muniYear),
#    may15_escol_mean_lag = lag(may15_escol_mean, n=1, order_by = muniYear),
#    indig_rate_lag = lag(indig_rate, n=1, order_by = muniYear),
    
#    perf_agua = tomas_agua_pop - tomas_agua_pop_lag,
#    perf_elec =  tomas_elec_pop - tomas_elec_pop_lag, 
#    perf_jardines =  jardines_pop - jardines_pop_lag, 
#    perf_parques =  parques_pop - parques_pop_lag, 
#    perf_drenaje =  drenaje_pop - drenaje_pop_lag, 
    
#    perfalfab_rate =  alfab_rate - alfab_rate_lag, 
#    perfasist_rate =  asist_rate - asist_rate_lag, 
#    perfnivel_rate =  nivel_rate - nivel_rate_lag, 
#    perfprof_rate =  prof_rate - prof_rate_lag, 
#    perfposg_rate =  posg_rate - posg_rate_lag, 
#    perfmay15_escol_mean =  may15_escol_mean - may15_escol_mean_lag, 
#    perfindig_rate =  indig_rate - indig_rate_lag
#  )

#summary(servicios)

####Exploracion performance####
tab_alt <- with(data, table(win, alt))

mean(data$alt, na.rm = T)
mean(data$cont, na.rm = T)
#promedio de alternancia por partido politico
with(data, tapply(alt, as.factor(win), mean, na.rm = T))

#with(data = subset(servicios, !is.na(alt)), qplot(perf_agua, fill = as.factor(alt), geom = "density", position = "fill", na.rm = T))

#palt_del <- ggplot(data = subset(delitos, !is.na(win) & !is.na(alt)), aes(x = year, y = win)) +
#  geom_jitter(alpha = 0.2, position = "jitter", shape = 1) +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  facet_grid(. ~ alt)

#palt_serv <- ggplot(data = subset(servicios, !is.na(win) & !is.na(alt)), aes(x = year, y = win)) +
#  geom_jitter(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  facet_grid(. ~ alt)
#ggsave("alt_yearXparty.png", path = out, dpi = 320)


#ddel <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = alt)) +
#  geom_density() +
#  facet_grid(. ~ alt)

#dserv <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = alt)) +
#  geom_density() +
#  facet_grid(. ~ alt)
#ggsave("densidad_de_alternancia.png", path = out, dpi = 320)

#ggplot(data = subset(servicios, !is.na(alt))) +
#  geom_density(aes(x = alt)) #+ geom_density(aes(x = alt == 0), fill = "darkred", alpha = 0.6) #+ scale_x_log10() 
#####Graficas delitos####
##Densidad de la distribución de los delitos: aumenta 
#pdt <- ggplot(delitos) +
  #geom_density(aes(x = totalrate), fill = "darkblue", alpha = 0.6) +
#  geom_density(aes(x = perftotrate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Total", y = "")

#pdd <- ggplot(delitos) +
  #geom_density(aes(x = daniorate), fill = "darkblue", alpha = 0.6) + ##esta es la que menos aumenta
#  geom_density(aes(x = perfdaniorate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Danio", y = "")

#pds <- ggplot(delitos) +
  #geom_density(aes(x = delsexrate), fill = "darkblue", alpha = 0.6) +
#  geom_density( aes(x = perfdelsexrate), fill = "darkred", alpha = 0.6) + 
#  labs(x = "Delitos sexuales", y = "")

#pdh <- ggplot(delitos) +
  #geom_density(aes(x = homrate), fill = "darkblue", alpha = 0.6) +
#  geom_density(aes(x = perfhomrate), fill = "darkred", alpha = 0.6) + 
#  labs(x = "Homicidios", y = "")

#pdl <- ggplot(delitos) +
  #geom_density(aes(x = lesrate), fill = "darkblue", alpha = 0.6) + 
#  geom_density(aes(x = perflesrate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Lesiones", y = "")

#pdr <- ggplot(delitos) +
  #geom_density(aes(x = roborate), fill = "darkblue", alpha = 0.6) +
#  geom_density(aes(x = perfroborate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Robo", y = "")

#pdo <- ggplot(delitos) +
  #geom_density(aes(x = otrosrate), fill = "darkblue", alpha = 0.6) +
#  geom_density(aes(x = perfotrosrate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Otros", y = "")

#den_del <- ggarrange(pdt, pdd, pds, pdh, pdl, pdr, pdo, label.y = "", align = "h")
#annotate_figure(den_del, top = text_grob("Densidad por tipo de denuncia", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("densidadXdelitos.png", path = out, dpi = 320)

##scatter de delitos
#sd1 <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = year, y = perftotrate, col = factor(alt))) +
#  geom_point(alpha = 0.2) +#, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Total", col = "Alt")

#sd2 <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = year, y = perfdaniorate, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Danio", col = "Alt")

#sd3 <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = year, y = perfdelsexrate, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Delitos sexuales", col = "Alt")

#sd4 <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = year, y = perfhomrate, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Homicidios", col = "Alt")

#sd5 <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = year, y = perflesrate, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Lesiones", col = "Alt")

#sd6 <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = year, y = perfroborate, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Robo", col = "Alt")

#sd7 <- ggplot(data = subset(delitos, !is.na(alt)), aes(x = year, y = perfotrosrate, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Otros", col = "Alt")

#scat_del <- ggarrange(sd1,sd2,sd3,sd4,sd5,sd6,sd7)#, ncol = 2,label.x = "year")
#annotate_figure(scat_del, top = text_grob("Dispersion por tipo de denuncia (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("scatterXdelitos.png", path = out, dpi = 320)

####Graficas servicios####
##Densidad en servicios
#pda <- ggplot(servicios) +
  #geom_density(aes(x = tomas_agua_pop), fill = "darkblue", alpha = 0.6) + 
#  geom_density(aes(x = perf_agua), fill = "darkred", alpha = 0.6) + #+ scale_x_log10() 
#  labs(x = "Agua")

#pde <- ggplot(servicios) +
  #geom_density(aes(x = tomas_elec_pop), fill = "darkblue", alpha = 0.6) + 
#  geom_density(aes(x = perf_elec), fill = "darkred", alpha = 0.6) + #+ scale_x_log10() 
#  labs(x = "Electricidad")

#pdj <- ggplot(servicios) +
  #geom_density(aes(x = jardines_pop), fill = "darkblue", alpha = 0.6) +
#  geom_density(aes(x = perf_jardines), fill = "darkred", alpha = 0.6) + #+  scale_x_log10() 
#  labs(x = "Jardines")

#pdp <- ggplot(servicios) +
  #geom_density(aes(x = parques_pop), fill = "darkblue", alpha = 0.6) +
#  geom_density(aes(x = perf_parques), fill = "darkred", alpha = 0.6) + #+  scale_x_log10() 
#  labs(x = "Parques")

#pddr <- ggplot(servicios) +
  #geom_density(aes(x = drenaje_pop)) +
#  geom_density(aes(x = perf_drenaje), fill = "darkred", alpha = 0.6) + #+  scale_x_log10()
#  labs(x = "Drenaje")

#den_serv <- ggarrange(pda, pde, pdj,pdp, pddr)
#annotate_figure(den_serv, top = text_grob("Densidad por tipo de servicio", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("densidadXservicios.png", path = out, dpi = 320)

##Scatters de servicios

#ss1 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perf_agua)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Agua", colour = "Alt")

#ss2 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perf_elec)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Electricidad", colour = "Alt")

#ss3 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perf_jardines)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Jardines", colour = "Alt")

#ss4 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perf_parques)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Parques", colour = "Alt")

#ss5 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perf_drenaje)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Drenaje", colour = "Alt")

#scat_serv <- ggarrange(ss1, ss2, ss3, ss4, ss5)
#annotate_figure(scat_serv, top = text_grob("Dispersion por tipo de servicio (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("scatterXservicios.png", path = out, dpi = 320)

##Este tipo de graficas se verian bien con pocos partidos, y podría ser clara
#ggplot(servicios, aes(x=perf_agua, y=alt, col = win)) + scale_y_continuous(breaks=seq(from=0, to=1, by=1))  +  geom_vline(xintercept = 0) + geom_point(size=3, alpha=0.2)

####Graficas educacion####
##densidad

#pdeds1 <- ggplot(servicios) +
#  geom_density(aes(x = perfalfab_rate), fill = "darkred", alpha = 0.6) + 
#  labs(x = "Alfabetizada (> 6 anios)")

#pdeds2 <- ggplot(servicios) +
#  geom_density(aes(x = perfasist_rate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Asistencia escolar (> 5 anios)")

#pdeds3 <- ggplot(servicios) +
#  geom_density(aes(x = perfnivel_rate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Nivel de escolaridad (> 5 anios)")

#pdeds4 <- ggplot(servicios) +
#  geom_density(aes(x = perfprof_rate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Nivel profesional (> 18 anios)")

#pdeds5 <- ggplot(servicios) +
#  geom_density(aes(x = perfposg_rate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Nivel posgrado (> 18 anios)")

#pdeds6 <- ggplot(servicios) +
#  geom_density(aes(x = perfmay15_escol_mean), fill = "darkred", alpha = 0.6) +
#  labs(x = "Escolaridad promedio (> 6 anios)")

#pdeds7 <- ggplot(servicios) +
#  geom_density(aes(x = perfindig_rate), fill = "darkred", alpha = 0.6) +
#  labs(x = "Lengua indigena (> 5 anios)")

#den_eds <- ggarrange(pdeds1, pdeds2, pdeds3, pdeds4, pdeds5, pdeds6, pdeds7)
#annotate_figure(den_eds, top = text_grob("Densidad por caracteristicas educativas y culturales", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("densidadXeducacion.png", path = out, dpi = 320)

##Scatters de servicios

#seds1 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perfalfab_rate)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Alfabetizada \n (> 6 anios)", colour = "Alt")

#seds2 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perfasist_rate)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Asistencia \n (> 5 anios)", colour = "Alt")

#seds3 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perfnivel_rate)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Nivel \n (> 5 anios)", colour = "Alt")

#seds4 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perfprof_rate)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Profesional \n (> 18 anios)", colour = "Alt")

#seds5 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perfposg_rate)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Posgrado \n (> 18 anios)", colour = "Alt")

#seds6 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perfmay15_escol_mean)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Escolaridad \n (> 6 anios)", colour = "Alt")

#seds7 <- ggplot(data = subset(servicios, !is.na(alt)), aes(x = year, y = perfindig_rate)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Lengua indigena \n (> 5 anios)", colour = "Alt")

#scat_ed <- ggarrange(seds1, seds2, seds3, seds4, seds5, seds6, seds7)
#annotate_figure(scat_ed, top = text_grob("Dispersion por caracteristicas educativas y culturales (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("scatterXeducacion.png", path = out, dpi = 320)

####Boxplot performance####
#Delitos

#bpd1 <- ggplot(data = subset(delitos, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perftotrate)) +
#  labs(x = "Alt", y = "Total", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_total_del.png", path = out, dpi = 320)

#bpd2 <- ggplot(data = subset(delitos, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfdaniorate)) +
#  labs(x = "Alt", y = "Danio", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_danio_del.png", path = out, dpi = 320)

#bpd3 <- ggplot(data = subset(delitos, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfdelsexrate)) +
#  labs(x = "Alt", y = "Del Sex", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_delsex_del.png", path = out, dpi = 320)

#bpd4 <- ggplot(data = subset(delitos, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfhomrate)) +
#  labs(x = "Alt", y = "Homicidio", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_total_del.png", path = out, dpi = 320)

#bpd5 <- ggplot(data = subset(delitos, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perflesrate)) +
#  labs(x = "Alt", y = "Lesiones", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_lesiones_del.png", path = out, dpi = 320)

#bpd6 <- ggplot(data = subset(delitos, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfroborate)) +
#  labs(x = "Alt", y = "Robo", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_robo_del.png", path = out, dpi = 320)

#bpd7 <- ggplot(data = subset(delitos, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfotrosrate)) +
#  labs(x = "Alt", y = "Otros", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_otros_del.png", path = out, dpi = 320)

#bp_del <- ggarrange(bpd1, bpd2, bpd3, bpd4, bpd5, bpd6, bpd7)
#annotate_figure(bp_del,top = text_grob("Alt por tipo de delito y partido (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("boxplot_alt_parti_delitos.png", path = out, dpi = 320)

#bps1 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perf_agua)) +
#  labs(x = "Alt", y = "Agua", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_agua_ser.png", path = out, dpi = 320)

#bps2 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perf_elec)) +
#  labs(x = "Alt", y = "Electricidad", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_electricidad_ser.png", path = out, dpi = 320)

#bps3 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perf_jardines)) +
#  labs(x = "Alt", y = "Jardines", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_jardines_ser.png", path = out, dpi = 320)

#bps4 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perf_parques)) +
#  labs(x = "Alt", y = "Parques", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_parques_ser.png", path = out, dpi = 320)

#bps5 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perf_drenaje)) +
#  labs(x = "Alt", y = "Drenaje", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_drenaje_ser.png", path = out, dpi = 320)

#bp_serv <- ggarrange(bps1, bps2, bps3, bps4, bps5)
#annotate_figure(bp_del,top = text_grob("Alt por tipo de servicio y partido (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("boxplot_alt_parti_servicios.png", path = out, dpi = 320)

#bped1 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfalfab_rate)) +
#  labs(x = "Alt", y = "Alfabetizacion", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_alfabetizacion_edu.png", path = out, dpi = 320)

#bped2 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfasist_rate)) +
#  labs(x = "Alt", y = "Asistencia", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_asistencia_edu.png", path = out, dpi = 320)

#bped3 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfnivel_rate)) +
#  labs(x = "Alt", y = "Nivel", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_nivel_edu.png", path = out, dpi = 320)

#bped4 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfprof_rate)) +
#  labs(x = "Alt", y = "Profesionistas", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_profesionistas_edu.png", path = out, dpi = 320)

#bped5 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfposg_rate)) +
#  labs(x = "Alt", y = "Posgrado", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_posgrado_edu.png", path = out, dpi = 320)

#bped6 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfmay15_escol_mean)) +
#  labs(x = "Alt", y = "Escol prom", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_escolaridad_promedio_edu.png", path = out, dpi = 320)

#bped7 <- ggplot(data = subset(servicios, !is.na(alt) & !is.na(inc_top))) +
#  geom_boxplot(aes(x = factor(alt), y = perfindig_rate)) +
#  labs(x = "Alt", y = "Lengua indigena", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("bp_lengua_indigena_edu.png", path = out, dpi = 320)

#bp_del <- ggarrange(bped1, bped2, bped3, bped4, bped5, bped6, bped7)
#annotate_figure(bp_del,top = text_grob("Boxplot por caracteristicas educativas y culturales (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
#ggsave("boxplot_alt_parti_educacion.png", path = out, dpi = 320)

####Delitos cambio porcentual####
del_b <- del_b %>%
  group_by(muni) %>% 
  mutate(
    total_lag = lag(Total_delitos, n=1, order_by = muniYear),
    #danio_lag = lag(danio, n=1, order_by = muniYear),
    #delsex_lag = lag(delsex, n=1, order_by = muniYear),
    hom_lag = lag(hom, n=1, order_by = muniYear),
    #les_lag = lag(les, n=1, order_by = muniYear),
    #robo_lag = lag(robo, n=1, order_by = muniYear),
    #otros_lag = lag(otros, n=1, order_by = muniYear),
    
    mayor6_leer_esc_lag = lag(mayor6_leer_esc, n=1, order_by = muniYear),
    may5_asistencia_lag = lag(may5_asistencia, n=1, order_by = muniYear),
    #may5_nivel_lag = lag(may5_nivel, n=1, order_by = muniYear),
    #may18_prof_lag = lag(may18_prof, n=1, order_by = muniYear),
    #may18_posg_lag = lag(may18_posg, n=1, order_by = muniYear),
    #may15_escol_mean_lag = lag(may15_escol_mean, n=1, order_by = muniYear),
    #may5_indigena_lag = lag(may5_indigena, n=1, order_by = muniYear),
    
    perftot = ((Total_delitos - total_lag)/total_lag) * 100, 
    #perfdanio = ((danio - danio_lag)/danio_lag) * 100, 
    #perfdelsex = ((delsex - delsex_lag)/delsex_lag) * 100, 
    perfhom = ((hom - hom_lag)/hom_lag) * 100, 
    #perfles = ((les - les_lag)/les_lag) * 100, 
    #perfrobo = ((robo - robo_lag)/robo_lag) * 100, 
    #perfotros = ((otros - otros_lag)/otros_lag) * 100, 
    
    perftot_lag = lag(perftot, n=1, order_by = muniYear),
    perfhom_lag = lag(perfhom, n=1, order_by = muniYear),
    
    change_total = perftot - perftot_lag,
    change_hom = perfhom - perfhom_lag,
    
    perfmayor6_leer_esc = ((mayor6_leer_esc - mayor6_leer_esc_lag)/mayor6_leer_esc_lag) * 100, 
    perfmay5_asistencia = ((may5_asistencia - may5_asistencia_lag)/may5_asistencia_lag) * 100, 
    #perfmay5_nivel = ((may5_nivel - may5_nivel_lag)/may5_nivel_lag) * 100, 
    #perfmay18_prof = ((may18_prof - may18_prof_lag)/may18_prof_lag) * 100, 
    #perfmay18_posg = ((may18_posg - may18_posg_lag)/may18_posg_lag) * 100, 
    #perfmay15_escol_mean = ((may15_escol_mean - may15_escol_mean_lag)/may15_escol_mean_lag) * 100, 
    #perfmay5_indigena = ((may5_indigena - may5_indigena_lag)/may5_indigena_lag) * 100
    
    perfmayor6_leer_esc_lag = lag(perfmayor6_leer_esc, n=1, order_by = muniYear), 
    perfmay5_asistencia_lag = lag(perfmay5_asistencia, n=1, order_by = muniYear),
    
    change_alfab = perfmayor6_leer_esc - perfmayor6_leer_esc_lag,
    change_asist = perfmay5_asistencia - perfmay5_asistencia_lag
  )

as.data.frame(del_b, row.names = NULL)
write.csv(del_b, paste(inp, "delitos_b.csv", sep = "/"), row.names = F, fileEncoding ="UTF-8")

#Servicios cambio porcentual####
serv_b <- serv_b %>%
  group_by(muni) %>% 
  mutate(
    tomas_dom_agua_lag = lag(tomas_dom_agua, n=1, order_by = muniYear),
    tomas_elec_lag = lag(tomas_elec, n=1, order_by = muniYear),
    #jardines_lag = lag(jardines, n=1, order_by = muniYear),
    #parques_lag = lag(parques, n=1, order_by = muniYear),
    sist_dren_lag = lag(sist_dren, n=1, order_by = muniYear),
    
    mayor6_leer_esc_lag = lag(mayor6_leer_esc, n=1, order_by = muniYear),
    may5_asistencia_lag = lag(may5_asistencia, n=1, order_by = muniYear),
    #may5_nivel_lag = lag(may5_nivel, n=1, order_by = muniYear),
    #may18_prof_lag = lag(may18_prof, n=1, order_by = muniYear),
    #may18_posg_lag = lag(may18_posg, n=1, order_by = muniYear),
    #may15_escol_mean_lag = lag(may15_escol_mean, n=1, order_by = muniYear),
    #may5_indigena_lag = lag(may5_indigena, n=1, order_by = muniYear),
    
    perf_agua =((tomas_dom_agua - tomas_dom_agua_lag)/tomas_dom_agua_lag) * 100, 
    perf_elec =((tomas_elec - tomas_elec_lag)/tomas_elec_lag) * 100, 
    #perf_jardines =((jardines - jardines_lag)/jardines_lag) * 100, 
    #perf_parques =((parques - parques_lag)/parques_lag) * 100, 
    perf_sist_dren =((sist_dren - sist_dren_lag)/sist_dren_lag) * 100, 
    
    perf_agua_lag = lag(perf_agua, n=1, order_by = muniYear),
    perf_elec_lag = lag(perf_elec, n=1, order_by = muniYear),
    perf_sist_dren_lag = lag(perf_sist_dren, n=1, order_by = muniYear),
    
    change_agua = perf_agua - perf_agua_lag,
    change_elec = perf_elec - perf_elec_lag,
    change_dren = perf_sist_dren - perf_sist_dren_lag,
    
    perfmayor6_leer_esc =((mayor6_leer_esc - mayor6_leer_esc_lag)/mayor6_leer_esc_lag) * 100, 
    perfmay5_asistencia =((may5_asistencia - may5_asistencia_lag)/may5_asistencia_lag) * 100, 
    #perfmay5_nivel =((may5_nivel - may5_nivel_lag)/may5_nivel_lag) * 100, 
    #perfmay18_prof =((may18_prof - may18_prof_lag)/may18_prof_lag) * 100, 
    #perfmay18_posg =((may18_posg - may18_posg_lag)/may18_posg_lag) * 100, 
    #perfmay15_escol_mean =((may15_escol_mean - may15_escol_mean_lag)/may15_escol_mean_lag) * 100, 
    #perfmay5_indigena =((may5_indigena - may5_indigena_lag)/may5_indigena_lag) * 100
    
    perfmayor6_leer_esc_lag = lag(perfmayor6_leer_esc, n=1, order_by = muniYear), 
    perfmay5_asistencia_lag = lag(perfmay5_asistencia, n=1, order_by = muniYear),
    
    change_alfab = perfmayor6_leer_esc - perfmayor6_leer_esc_lag,
    change_asist = perfmay5_asistencia - perfmay5_asistencia_lag
  )

as.data.frame(serv_b, row.names = NULL)
write.csv(serv_b, paste(inp, "servicios_b.csv", sep = "/"), row.names = F, fileEncoding ="UTF-8")
####Graficas delitos cambio %####
##Densidad de la distribución de los delitos: aumenta 
zpdt <- ggplot(del_b) +
  geom_density(aes(x = perftot), fill = "darkred", alpha = 0.6) +
  labs(x = "Total_delitos", y = "")

#zpdd <- ggplot(del_b) +
  #geom_density(aes(x = perfdanio), fill = "darkred", alpha = 0.6) +
  #labs(x = "Danio", y = "")

#zpds <- ggplot(del_b) +
  #geom_density( aes(x = perfdelsex), fill = "darkred", alpha = 0.6) + 
  #labs(x = "del_b sexuales", y = "")

zpdh <- ggplot(del_b) +
  geom_density(aes(x = perfhom), fill = "darkred", alpha = 0.6) + 
  labs(x = "Homicidios", y = "")

#zpdl <- ggplot(del_b) +
  #geom_density(aes(x = perfles), fill = "darkred", alpha = 0.6) +
  #labs(x = "Lesiones", y = "")

#zpdr <- ggplot(del_b) +
  #geom_density(aes(x = perfrobo), fill = "darkred", alpha = 0.6) +
  #labs(x = "Robo", y = "")

#zpdo <- ggplot(del_b) +
  #geom_density(aes(x = perfotros), fill = "darkred", alpha = 0.6) +
  #labs(x = "Otros", y = "")

zden_del <- ggarrange(zpdt, #zpdd, zpds, 
                      zpdh, #zpdl, zpdr, zpdo, 
                      label.y = "", align = "h")
annotate_figure(zden_del, top = text_grob("Densidad por tipo de denuncia", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("zdensidadXdel_b.png", path = out, dpi = 320)

##scatter de del_b
zsd1 <- ggplot(data = subset(del_b, !is.na(alt) & win_top != "Otros"), aes(x = year, y = perftot, col = factor(alt))) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Total_delitos", col = "Alt")

#zsd2 <- ggplot(data = subset(del_b, !is.na(alt)), aes(x = year, y = perfdanio, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Danio", col = "Alt")

#zsd3 <- ggplot(data = subset(del_b, !is.na(alt)), aes(x = year, y = perfdelsex, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "del_b sexuales", col = "Alt")

zsd4 <- ggplot(data = subset(del_b, !is.na(alt) & win_top != "Otros"), aes(x = year, y = perfhom, col = factor(alt))) +
  geom_point(alpha = 0.2, position = "jitter") +
  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Homicidios", col = "Alt")

#zsd5 <- ggplot(data = subset(del_b, !is.na(alt)), aes(x = year, y = perfles, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Lesiones", col = "Alt")

#zsd6 <- ggplot(data = subset(del_b, !is.na(alt)), aes(x = year, y = perfrobo, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Robo", col = "Alt")

#zsd7 <- ggplot(data = subset(del_b, !is.na(alt)), aes(x = year, y = perfotros, col = factor(alt))) +
#  geom_point(alpha = 0.2, position = "jitter") +
#  scale_x_continuous(breaks = seq(from = 1994, to = 2010, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Otros", col = "Alt")

zscat_del <- ggarrange(zsd1,#zsd2,zsd3,
                       zsd4#,zsd5,zsd6,zsd7
                       )
annotate_figure(zscat_del, top = text_grob("Dispersion por tipo de denuncia (1994-2010)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("zscatterXdel_b.png", path = out, dpi = 320)

####Graficas servicios cambio %####
##Densidad en serv_b

xpda <- ggplot(serv_b) +
  geom_density(aes(x = perf_agua), fill = "darkred", alpha = 0.6) +
  labs(x = "Agua")

xpde <- ggplot(serv_b) +
  geom_density(aes(x = perf_elec), fill = "darkred", alpha = 0.6) +
  labs(x = "Electricidad")

#xpdj <- ggplot(serv_b) +
#  geom_density(aes(x = perf_jardines), fill = "darkred", alpha = 0.6) +
#  labs(x = "Jardines")

#xpdp <- ggplot(serv_b) +
#  geom_density(aes(x = perf_parques), fill = "darkred", alpha = 0.6) + 
#  labs(x = "Parques")

xpddr <- ggplot(serv_b) +
  geom_density(aes(x = perf_sist_dren), fill = "darkred", alpha = 0.6) +
  labs(x = "Drenaje")

xden_serv <- ggarrange(xpda, xpde, #xpdj, xpdp, 
                       xpddr)
annotate_figure(xden_serv, top = text_grob("Densidad por tipo de servicio", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("xdensidadXserv_b.png", path = out, dpi = 320)

##Scatters de serv_b
xss1 <- ggplot(data = subset(serv_b, !is.na(alt) & win_top != "Otros"), aes(x = year, y = perf_agua)) +
  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Agua", colour = "Alt")

xss2 <- ggplot(data = subset(serv_b, !is.na(alt) & win_top != "Otros"), aes(x = year, y = perf_elec)) +
  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Electricidad", colour = "Alt")

#xss3 <- ggplot(data = subset(serv_b, !is.na(alt)), aes(x = year, y = perf_jardines)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Jardines", colour = "Alt")

#xss4 <- ggplot(data = subset(serv_b, !is.na(alt)), aes(x = year, y = perf_parques)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Parques", colour = "Alt")

xss5 <- ggplot(data = subset(serv_b, !is.na(alt) & win_top != "Otros"), aes(x = year, y = perf_sist_dren)) +
  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Drenaje", colour = "Alt")

xscat_serv <- ggarrange(xss1, xss2, #xss3, xss4, 
                        xss5)
annotate_figure(xscat_serv, top = text_grob("Dispersion por tipo de servicio (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("xscatterXserv_b.png", path = out, dpi = 320)

####Graficas educacion cambio %####
##densidad

ypdeds1 <- ggplot(serv_b) +
  geom_density(aes(x = perfmayor6_leer_esc), fill = "darkred", alpha = 0.6) + 
  labs(x = "Alfabetizada (> 6 anios)")

ypdeds2 <- ggplot(serv_b) +
  geom_density(aes(x = perfmay5_asistencia), fill = "darkred", alpha = 0.6) +
  labs(x = "Asistencia escolar (> 5 anios)")

#ypdeds3 <- ggplot(serv_b) +
#  geom_density(aes(x = perfmay5_nivel), fill = "darkred", alpha = 0.6) +
#  labs(x = "Nivel de escolaridad (> 5 anios)")

#ypdeds4 <- ggplot(serv_b) +
#  geom_density(aes(x = perfmay18_prof), fill = "darkred", alpha = 0.6) +
#  labs(x = "Nivel may18_profesional (> 18 anios)")

#ypdeds5 <- ggplot(serv_b) +
#  geom_density(aes(x = perfmay18_posg), fill = "darkred", alpha = 0.6) +
#  labs(x = "Nivel may18_posgrado (> 18 anios)")

#ypdeds6 <- ggplot(serv_b) +
#  geom_density(aes(x = perfmay15_escol_mean), fill = "darkred", alpha = 0.6) +
#  labs(x = "Escolaridad promedio (> 6 anios)")

#ypdeds7 <- ggplot(serv_b) +
#  geom_density(aes(x = perfmay5_indigena), fill = "darkred", alpha = 0.6) +
#  labs(x = "Lengua may5_indigenaena (> 5 anios)")

yden_eds <- ggarrange(ypdeds1, ypdeds2#, ypdeds3, ypdeds4, ypdeds5, ypdeds6, ypdeds7
                      )
annotate_figure(yden_eds, top = text_grob("Densidad por caracteristicas educativas y culturales", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("ydensidadXeducacion.png", path = out, dpi = 320)

##Scatters de educacion b

yseds1 <- ggplot(data = subset(serv_b, !is.na(alt) & win_top != "Otros"), aes(x = year, y = perfmayor6_leer_esc)) +
  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Alfabetizada \n (> 6 anios)", colour = "Alt")

yseds2 <- ggplot(data = subset(serv_b, !is.na(alt) & win_top != "Otros"), aes(x = year, y = perfmay5_asistencia)) +
  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Asistencia \n (> 5 anios)", colour = "Alt")

#yseds3 <- ggplot(data = subset(serv_b, !is.na(alt)), aes(x = year, y = perfmay5_nivel)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Nivel \n (> 5 anios)", colour = "Alt")

#yseds4 <- ggplot(data = subset(serv_b, !is.na(alt)), aes(x = year, y = perfmay18_prof)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Profesional \n (> 18 anios)", colour = "Alt")

#yseds5 <- ggplot(data = subset(serv_b, !is.na(alt)), aes(x = year, y = perfmay18_posg)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Posgrado \n (> 18 anios)", colour = "Alt")

#yseds6 <- ggplot(data = subset(serv_b, !is.na(alt)), aes(x = year, y = perfmay15_escol_mean)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Escolaridad \n (> 6 anios)", colour = "Alt")

#yseds7 <- ggplot(data = subset(serv_b, !is.na(alt)), aes(x = year, y = perfmay5_indigena)) +
#  geom_point(aes(colour = factor(alt)), alpha = 0.2, position = "jitter") + 
#  scale_x_continuous(breaks = seq(from = 1994, to = 2015, by = 1)) + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(y = "Lengua may5_indigenaena \n (> 5 anios)", colour = "Alt")

yscat_ed <- ggarrange(yseds1, yseds2#, yseds3, yseds4, yseds5, yseds6, yseds7
                      )
annotate_figure(yscat_ed, top = text_grob("Dispersion por caracteristicas educativas y culturales (1994-2015)", color = "black", face = "bold", size = 14), bottom = text_grob("Diferencial entre el año electoral t-1 y t", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("yscatterXeducacion.png", path = out, dpi = 320)

####Boxplot cambio %####
#Box delitos####
#del_b
#zbpd1a <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perftot < 120 & perftot > -100)) +  geom_boxplot(aes(x = factor(alt), y = perftot)) +  scale_y_continuous(breaks = seq(-100 , 120, by = 10)) +  labs(x = "Alt", y = "Total_delitos", title = "Alternancia y cambio porcentual de bienes publicos", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t")ggsave("zbp_td.png", path = out, dpi = 320)

#try1 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perftot < 130 & perftot != -100)) +  geom_boxplot(aes(x = factor(alt), y = perftot)) +  scale_y_continuous(breaks = seq(-100 , 120, by = 10)) +  labs(x = "Alt", y = "Total_delitos", title = "Alternancia y cambio en bienes por estado", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +  facet_grid(win_top ~ state)

zbpd1 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perftot < 200 & perftot != -100 & win_top != "Otros")) +#& perftot <= quantile(perftot, 0.85, na.rm = T))) + #Esto nos dice que aprox el 15% de las observaciones son outliers
  geom_boxplot(aes(x = factor(alt), y = perftot)) +
  scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
  labs(x = "Alt", y = "Total_delitos", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("zbp_total_del.png", path = out, dpi = 320)

#zbpd2 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perfdanio < 200 & perfdanio != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfdanio)) +
#  scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
#  labs(x = "Alt", y = "Danio", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("zbp_danio_del.png", path = out, dpi = 320)

#zbpd3 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perfdelsex < 200 & perfdelsex != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfdelsex)) +
#  scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
#  labs(x = "Alt", y = "Del Sex", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("zbp_delsex_del.png", path = out, dpi = 320)

zbpd4 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perfhom < 200 & perfhom != -100 & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = perfhom)) +
  scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
  labs(x = "Alt", y = "Homicidio", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("zbp_hom_del.png", path = out, dpi = 320)

#zbpd5 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perfles < 200 & perfles != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfles)) +
#  scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
#  labs(x = "Alt", y = "Lesiones", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("zbp_lesiones_del.png", path = out, dpi = 320)

#zbpd6 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perfrobo < 200 & perfrobo != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfrobo)) +
#  scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
#  labs(x = "Alt", y = "Robo", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("zbp_robo_del.png", path = out, dpi = 320)

#zbpd7 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & perfotros < 200 & perfotros != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfotros)) +
#  scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
#  labs(x = "Alt", y = "Otros", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("zbp_otros_del.png", path = out, dpi = 320)

#*boxplot change delitos####
del1 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) #& perftot < 200 & perftot != -100 
                             & win_top != "Otros")) +#& perftot <= quantile(perftot, 0.85, na.rm = T))) + #Esto nos dice que aprox el 15% de las observaciones son outliers
  geom_boxplot(aes(x = factor(alt), y = change_total)) +
  #scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
  labs(x = "Alt", y = "Total_delitos", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("del_total.png", path = out, dpi = 320)

del2 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) & #perfhom < 200 & perfhom != -100 & 
                               win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_hom)) +
  #scale_y_continuous(breaks = seq(-100 , 200, by = 10)) +
  labs(x = "Alt", y = "Homicidio", title = "Alternancia y cambio en bienes por partido", subtitle = "Delitos registrados ante el MP", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("del_hom.png", path = out, dpi = 320)

#Box servicios####
xbps1 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perf_agua <= quantile(perf_agua, 0.9, na.rm = T) & perf_agua != -100 & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = perf_agua)) +
  scale_y_continuous(breaks = seq(-100 , 50, by = 10)) +
  labs(x = "Alt", y = "Agua", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("xbp_agua_ser.png", path = out, dpi = 320)

xbps2 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perf_elec <= quantile(perf_elec, 0.9, na.rm = T) & perf_elec != -100 & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = perf_elec)) +
  scale_y_continuous(breaks = seq(-100 , 50, by = 10)) +
  labs(x = "Alt", y = "Electricidad", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("xbp_electricidad_ser.png", path = out, dpi = 320)

#xbps3 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perf_jardines < 50 & perf_jardines != -100)) +#<= quantile(perf_jardines, 0.9, na.rm = T) 
#  geom_boxplot(aes(x = factor(alt), y = perf_jardines)) +
#  scale_y_continuous(breaks = seq(-100 , 50, by = 10)) +
#  labs(x = "Alt", y = "Jardines", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("xbp_jardines_ser.png", path = out, dpi = 320)

#xbps4 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perf_parques < 50 & perf_parques != -100 & win_top != "Otros")) +
#  geom_boxplot(aes(x = factor(alt), y = perf_parques)) +
#  scale_y_continuous(breaks = seq(-100 , 50, by = 10)) +
#  labs(x = "Alt", y = "Parques", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("xbp_parques_ser.png", path = out, dpi = 320)

xbps5 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perf_sist_dren <= quantile(perf_sist_dren, 0.9, na.rm = T) & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = perf_sist_dren)) +
  labs(x = "Alt", y = "Drenaje", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("xbp_sist_dren_ser.png", path = out, dpi = 320)

#*boxplot change servicios####
serv1 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) #& perf_agua <= quantile(perf_agua, 0.9, na.rm = T) & perf_agua != -100 
                              & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_agua)) +
  #scale_y_continuous(breaks = seq(-100 , 50, by = 10)) +
  labs(x = "Alt", y = "Agua", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("serv_agua.png", path = out, dpi = 320)

serv2 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) #& perf_elec <= quantile(perf_elec, 0.9, na.rm = T) & perf_elec != -100 
                              & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_elec)) +
  #scale_y_continuous(breaks = seq(-100 , 50, by = 10)) +
  labs(x = "Alt", y = "Electricidad", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("serv_elec.png", path = out, dpi = 320)

serv3 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) #& perf_sist_dren <= quantile(perf_sist_dren, 0.9, na.rm = T) 
                              & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_dren)) +
  labs(x = "Alt", y = "Drenaje", title = "Alternancia y cambio en bienes por partido", subtitle = "Servicios publicos", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("serv_dren.png", path = out, dpi = 320)

#Box educacion####
ybped1 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perfmayor6_leer_esc <= quantile(perfmayor6_leer_esc, 0.9, na.rm = T) & perfmayor6_leer_esc != -100 & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = perfmayor6_leer_esc)) +
  labs(x = "Alt", y = "Alfabetizacion", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("ybp_mayor6_leer_escetizacion_edu.png", path = out, dpi = 320)

ybped2 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perfmay5_asistencia <= quantile(perfmay5_asistencia, 0.9, na.rm = T) & perfmay5_asistencia != -100 & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = perfmay5_asistencia)) +
  labs(x = "Alt", y = "Asistencia", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("ybp_may5_asistenciaencia_edu.png", path = out, dpi = 320)

#ybped3 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perfmay5_nivel <= quantile(perfmay5_nivel, 0.9, na.rm = T) & perfmay5_nivel != -100 & win_top != "Otros")) +
#  geom_boxplot(aes(x = factor(alt), y = perfmay5_nivel)) +
#  labs(x = "Alt", y = "Nivel", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("ybp_may5_nivel_edu.png", path = out, dpi = 320)

#ybped4 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perfmay18_prof <= quantile(perfmay18_prof, 0.9, na.rm = T) & perfmay18_prof != -100)) +
#  geom_boxplot(aes(x = factor(alt), y = perfmay18_prof)) +
#  labs(x = "Alt", y = "Profesionistas", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("ybp_may18_profesionistas_edu.png", path = out, dpi = 320)

#ybped5 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perfmay18_posg <= quantile(perfmay18_posg, 0.9, na.rm = T) & perfmay18_posg != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfmay18_posg)) +
#  labs(x = "Alt", y = "Posgrado", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("ybp_may18_posgrado_edu.png", path = out, dpi = 320)

#ybped6 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perfmay15_escol_mean <= quantile(perfmay15_escol_mean, 0.9, na.rm = T) & perfmay15_escol_mean != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfmay15_escol_mean)) +
#  labs(x = "Alt", y = "Escol prom", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("ybp_escolaridad_promedio_edu.png", path = out, dpi = 320)

#ybped7 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) & perfmay5_indigena <= quantile(perfmay5_indigena, 0.9, na.rm = T) & perfmay5_indigena != -100) & win_top != "Otros") +
#  geom_boxplot(aes(x = factor(alt), y = perfmay5_indigena)) +
#  labs(x = "Alt", y = "Lengua may5_indigenaena", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
#  facet_grid(. ~ inc_top)
#ggsave("ybp_lengua_may5_indigenaena_edu.png", path = out, dpi = 320)

#*boxplot change educacion####
edudel1 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) #& perfmayor6_leer_esc <= quantile(perfmayor6_leer_esc, 0.9, na.rm = T) & perfmayor6_leer_esc != -100 
                                & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_alfab)) +
  labs(x = "Alt", y = "Alfabetizacion", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("edudel_1.png", path = out, dpi = 320)

edudel2 <- ggplot(data = subset(del_b, !is.na(alt) & !is.na(inc_top) #& perfmay5_asistencia <= quantile(perfmay5_asistencia, 0.9, na.rm = T) & perfmay5_asistencia != -100 
                                & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_asist)) +
  labs(x = "Alt", y = "Asistencia", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("edudel_2.png", path = out, dpi = 320)

eduser1 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) #& perfmayor6_leer_esc <= quantile(perfmayor6_leer_esc, 0.9, na.rm = T) & perfmayor6_leer_esc != -100 
                                & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_alfab)) +
  labs(x = "Alt", y = "Alfabetizacion", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("eduser_1.png", path = out, dpi = 320)

eduser2 <- ggplot(data = subset(serv_b, !is.na(alt) & !is.na(inc_top) #& perfmay5_asistencia <= quantile(perfmay5_asistencia, 0.9, na.rm = T) & perfmay5_asistencia != -100 
                                & win_top != "Otros")) +
  geom_boxplot(aes(x = factor(alt), y = change_asist)) +
  labs(x = "Alt", y = "Asistencia", title = "Alternancia y cambio en bienes por partido", subtitle = "Educacion", caption = "Diferencial entre el año electoral t-1 y t") +
  facet_grid(. ~ inc_top)
ggsave("eduser_2.png", path = out, dpi = 320)
