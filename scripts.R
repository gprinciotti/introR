# LIMPANDO WORKSPACE
rm(list = ls())

# PACOTES NECESSÁRIOS
source("packages.R")
CallLibraries(c("data.table", "stargazer", "dplyr", "ggplot2", "ggcorrplot", "oaxaca"))

# CHAMANDO SCRIPTS
source("0_abertura.R")
source("1_limpeza.R")
source("2_descritivas.R")
source("3_estatisticas.R")
