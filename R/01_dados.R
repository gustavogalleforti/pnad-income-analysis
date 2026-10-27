# Download dos microdados da PNAD Contínua, filtros amostrais e
# construção das variáveis analíticas

source("R/00_setup.R")


# Variáveis que serão baixadas ----
# Aqui, apenas as colunas necessárias serão solicitadas ao get_pnadc() para
# economizar memória e tempo de download.
#
# Dicionário resumido:
#   UF       - Unidade da Federação
#   UPA      - Unidade Primária de Amostragem (PSU do design)
#   V1008    - Número do domicílio na UPA
#   V1014    - Número do painel
#   V1022    - Situação do domicílio (1=Urbana, 2=Rural)
#   V1023    - Classificação da área (1=Capital, 2=Resto RM/RIDE, 3=Resto UF)
#   V2005    - Condição no domicílio (1=Pessoa de referência / chefe)
#   V2007    - Sexo (1=Homem, 2=Mulher)
#   V2009    - Idade do morador
#   V2010    - Cor ou raça (1=Branca, 2=Preta, 3=Amarela, 4=Parda, 5=Indígena)
#   VD4002   - Condição de ocupação (1=Ocupado, 2=Desocupado)
#   VD4019   - Rendimento mensal efetivo do trabalho principal (R$)
#   VD4020   - Rendimento mensal efetivo de todos os trabalhos (R$)
#   VD3004   - Nível de instrução (2-3=Fundamental, 4-5=Médio, >=6=Superior)
#   V3002A   - Anos de estudo (usado como fallback se VD3004 ausente)
#   V1027    - Peso amostral básico
#   V1028    - Peso de calibração final (obrigatório no svydesign)
#   V1033    - Peso para estimativas de pessoas
#   Estrato  - Estrato de amostragem (obrigatório no svydesign)

vars_min <- c(
  "UF", "UPA", "V1008", "V1014", "V1022", "V1023",
  "V2007", "V2009", "V2010", "V2005",
  "VD4002", "VD4019", "VD4020",
  "VD3004", "V3002A",
  "V1027", "V1028", "V1033", "Estrato"
)


# Download - 3º trimestre de 2013 (pré-crise) e 2016 (crise) ----
# crise2016: variável indicadora do período (0=2013, 1=2016)
# design=FALSE: o design será construído manualmente
# labels=FALSE: mantém códigos numéricos para comparações

pnad13 <- get_pnadc(2013, quarter = 3, design = FALSE, labels = FALSE, vars = vars_min) |>
  mutate(ano = 2013L, tri = 3L, crise2016 = 0L)

pnad16 <- get_pnadc(2016, quarter = 3, design = FALSE, labels = FALSE, vars = vars_min) |>
  mutate(ano = 2016L, tri = 3L, crise2016 = 1L)

pnad <- bind_rows(pnad13, pnad16)

#  cache local para evitar re-download
savedir <- file.path(tempdir(), "pnadc_cache")
dir.create(savedir, showWarnings = FALSE, recursive = TRUE)
# saveRDS(pnad, file.path(savedir, "pnad_filtrado.rds"))  # descomente para salvar


# Filtros amostrais ----
# Alvo: chefes de domicílio, 21-50 anos, área urbana metropolitana
#
# se get_pnadc devolver fatores em vez de códigos, usa o rótulo

v1022 <- to_num(pnad$V1022)   # situação do domicílio
v1023 <- to_num(pnad$V1023)   # classificação da área
v2005 <- to_num(pnad$V2005)   # condição no domicílio
v2009 <- to_num(pnad$V2009)   # idade

# Urbano (V1022 == 1)
cond_urb <- (v1022 == 1)
if (sum(cond_urb, na.rm = TRUE) == 0) {
  # fallback: busca pelo rótulo "urb" na string
  cond_urb <- grepl("^1$|urb", tolower(as.character(pnad$V1022)))
}

# Metropolitano: capital ou resto de RM/RIDE (V1023 %in% c(1,2))
cond_metro <- (v1023 %in% c(1, 2))
if (sum(cond_metro, na.rm = TRUE) == 0) {
  v1023_chr  <- tolower(as.character(pnad$V1023))
  cond_metro <- grepl("capital", v1023_chr) | grepl("rm|ride", v1023_chr)
}
cond_metro[is.na(cond_metro)] <- FALSE

# Chefe / pessoa de referência (V2005 == 1)
cond_chefe <- (v2005 == 1)
if (sum(cond_chefe, na.rm = TRUE) == 0) {
  cond_chefe <- grepl("refer|chefe", tolower(as.character(pnad$V2005)))
}
cond_chefe[is.na(cond_chefe)] <- FALSE

# Faixa etária 21-50 anos
cond_idade <- (v2009 >= 21 & v2009 <= 50)
cond_idade[is.na(cond_idade)] <- FALSE

# Aplica todos os filtros
pnad <- pnad[cond_idade & cond_urb & cond_metro & cond_chefe, , drop = FALSE]


# Variáveis analíticas ----
pnad$ocupado   <- as.integer(to_num(pnad$VD4002) == 1)  # 1=ocupado, 0=demais
pnad$renda_pri <- to_num(pnad$VD4019)                    # renda do trabalho principal
pnad$renda_tot <- to_num(pnad$VD4020)                    # renda de todos os trabalhos
pnad$renda     <- pnad$renda_pri                         # renda usada na análise = principal
pnad$ln_renda  <- ifelse(!is.na(pnad$renda) & pnad$renda > 0,
                         log(pnad$renda), NA_real_)       # log-renda (para análises auxiliares)

# Deflacionamento pelo IPCA (base = set/2013)
# Fator calculado a partir do IPCA acumulado set/2013 - set/2016 (~26,8%)
# Fonte: IBGE - Séries Históricas do IPCA
defl_16on13 <- 1.268
pnad$renda_real_2013T3 <- ifelse(
  pnad$crise2016 == 1,
  pnad$renda / defl_16on13,   # deflaciona os valores de 2016 para R$ de set/2013
  pnad$renda                  # valores de 2013 já estão na base correta
)


# Design amostral complexo (survey) ----
# para se faltar alguma coluna do desenho amostral (versões antigas do
# PNADcIBGE não traziam V1028/Estrato com design = FALSE)
stopifnot(
  "V1028 ausente - verifique se get_pnadc gerou a coluna de peso de calibração" =
    "V1028"   %in% names(pnad),
  "Estrato ausente - verifique se get_pnadc gerou a coluna de estrato" =
    "Estrato" %in% names(pnad),
  "UPA ausente" =
    "UPA"     %in% names(pnad)
)

# id     = UPA  (unidade primária de amostragem)
# strata = Estrato (estratificação do plano amostral)
# weights= V1028 (peso de calibração final - não usar V1027 aqui)
# nest   = TRUE (UPAs aninhadas dentro dos estratos, numeração pode se repetir)
des_base <- survey::svydesign(
  id      = ~UPA,
  strata  = ~Estrato,
  weights = ~V1028,
  data    = pnad,
  nest    = TRUE
)

message("01_dados.R concluído - ", nrow(pnad), " observações após filtros.")
