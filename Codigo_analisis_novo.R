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
  )

bp <- read.csv(paste(inp, "Bienes_publicos.csv", sep = "/"))
colnames(bp)[4] <- "tot_del"
bp <- select(bp, -c(year, muni))

data <- data %>% 
  full_join(bp, by = "muniYear")

p1 <- ggplot(data %>% filter(!is.na(alt)) , aes(x = as.factor(year), y = inc.share, col = factor(alt))) + 
  geom_boxplot()
p1
#p2 <- ggplot(data, aes(x = ))