rm(list = ls())
setwd("~")

inp <- "/home/dhjs/Documentos/R_projects/tesis/Datos"

del <- read.csv(paste(inp, "delitos.csv", sep = "/"), stringsAsFactors = F)
del <- select(del, -c(muni, year))
ser <- read.csv(paste(inp, "viv.csv", sep = "/"), stringsAsFactors = F)

bp <- inner_join(del, ser, by = "muniYear")
bp <- bp %>% 
  select("muniYear", "muni", "year", "total", "danio", "delsex", "hom", "les", "robo", "otros", "tomas_dom_agua", "tomas_elec", "sist_dren") %>% 
  mutate(
    muni = formatC(muni, width=5, format="d", flag = "0") 
  )
