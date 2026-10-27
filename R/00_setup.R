# Instalação de pacotes e funções auxiliares globais
#
# Projeto : Impacto da crise de 2016 sobre renda e desemprego
#           Comparação PNAD Contínua - 3º trimestre de 2013 vs 3º trimestre de 2016
# Amostra : Chefes de domicílio, 21-50 anos, área urbana metropolitana


# Instalação automática de pacotes ausentes ----
pkgs <- c("PNADcIBGE", "dplyr", "survey", "gt")

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(PNADcIBGE)
library(dplyr)
library(survey)
library(gt)

# Função utilizada para evitar erros com PSU solitária em estratos pequenos (ajusta a variância)
options(survey.lonely.psu = "adjust")

# Pasta onde as tabelas são salvas
dir.create("outputs", showWarnings = FALSE)


# Funções auxiliares que serão reutilizadas em todos os scripts ----

# Converte qualquer vetor (fator, character) para numérico sem avisos
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Retorna o primeiro nome de coluna disponível entre os candidatos fornecidos
# (serve para lidar com as diferentes versões do dicionário oficial da PNAD Contínua)
pick_first <- function(nm, candidates) {
  cands <- intersect(candidates, nm)
  if (length(cands)) cands[1] else NA_character_
}

# Converte p-valor em asteriscos de significância estatística
# Padrão: * p<0,05 | ** p<0,01 | *** p<0,001
stars <- function(p) {
  if (is.na(p))    ""
  else if (p < .001) "***"
  else if (p < .01)  "**"
  else if (p < .05)  "*"
  else               ""
}
