# Estimativas com pesos amostrais e testes t de diferença de médias
#
# Analisa:
#   - Renda média nominal dos ocupados (VD4019) - 2013-T3 vs 2016-T3
#   - Taxa de desemprego na PEA              - 2013-T3 vs 2016-T3
#   - Diferença de renda REAL (deflacionada pelo IPCA) via regressão svyglm

source("R/01_dados.R")   # carrega pnad, des_base e funções auxiliares


# Subdesigns de trabalho ----

# Ocupados com renda positiva (denominador das médias de renda)
des_sal <- subset(des_base, ocupado == 1 & !is.na(renda) & renda > 0)

# PEA = ocupados + desocupados (denominador da taxa de desemprego)
# A variável desocup é criada dentro do design via update() - para que o pacote survey propague corretamente a variância
des_base2 <- update(des_base,
                    pea     = as.numeric(VD4002 %in% c(1, 2)),
                    desocup = as.numeric(VD4002 == 2))
des_pea <- subset(des_base2, pea == 1)


# Renda média nominal por período + teste t ----

# svyby calcula a média ponderada dentro de cada nível de crise2016
# vartype="se","ci" entrega erro-padrão e intervalo de confiança 95% diretamente
rend_med <- svyby(~renda, ~crise2016, des_sal,
                  svymean, na.rm = TRUE, keep.var = TRUE,
                  vartype = c("se", "ci"))

# svyttest testa H0: média(renda | 2013) == média(renda | 2016)
# sob o desenho amostral complexo (não assume IID)
t_renda <- svyttest(renda ~ crise2016, design = des_sal)

cat("==== Renda média nominal (ocupados, renda>0) - VD4019 ====\n")
print(rend_med)
cat("\nTeste t ponderado (diferença 2016 vs 2013):\n")
print(t_renda)


# Taxa de desemprego por período + teste t ----

tx_des <- svyby(~desocup, ~crise2016, des_pea,
                svymean, na.rm = TRUE,
                vartype = c("se", "ci"))

t_des <- svyttest(desocup ~ crise2016, design = des_pea)

cat("\n==== Taxa de desemprego (PEA) ====\n")
print(tx_des)
cat("\nTeste t ponderado (diferença 2016 vs 2013):\n")
print(t_des)


# Tabelas formatadas (gt) ----

# --- Renda nominal ---
# Extrai o erro-padrão com fallback para diferentes versões do survey
SE_renda <- if ("se" %in% names(rend_med)) {
  rend_med$se
} else if (any(grepl("^se\\.", names(rend_med)))) {
  as.numeric(rend_med[[grep("^se\\.", names(rend_med))[1]]])
} else {
  sqrt(diag(attr(rend_med, "var")))
}

tab_renda <- data.frame(
  Periodo = ifelse(rend_med$crise2016 == 0, "2013-T3", "2016-T3"),
  Media   = as.numeric(rend_med$renda),
  SE      = as.numeric(SE_renda),
  CI_low  = rend_med$ci_l,
  CI_high = rend_med$ci_u
)

print(
  gt(tab_renda) |>
    tab_header(
      title    = md("**Renda média nominal dos ocupados (VD4019)**"),
      subtitle = md("21-50 anos; urbano metropolitano; chefes - 2013-T3 vs 2016-T3")
    ) |>
    fmt_number(columns = c(Media, SE, CI_low, CI_high),
               decimals = 2, use_seps = TRUE)
)

# --- Taxa de desemprego ---
SE_des <- if ("se" %in% names(tx_des)) {
  tx_des$se
} else if (any(grepl("^se\\.", names(tx_des)))) {
  as.numeric(tx_des[[grep("^se\\.", names(tx_des))[1]]])
} else {
  sqrt(diag(attr(tx_des, "var")))
}

tab_des <- data.frame(
  Periodo = ifelse(tx_des$crise2016 == 0, "2013-T3", "2016-T3"),
  Tx_Des  = as.numeric(tx_des$desocup),
  SE      = as.numeric(SE_des),
  CI_low  = tx_des$ci_l,
  CI_high = tx_des$ci_u
)

print(
  gt(tab_des) |>
    tab_header(
      title    = md("**Taxa de desemprego (PEA)**"),
      subtitle = md("21-50 anos; urbano metropolitano; chefes - 2013-T3 vs 2016-T3")
    ) |>
    fmt_percent(columns = c(Tx_Des, SE, CI_low, CI_high), decimals = 2)
)


# Diferença de renda REAL (deflacionada) ----
# des_base já tem renda_real_2013T3 criada em 01_dados.R
# atualiza o design com a variável real para uso no svyglm
des_base <- update(des_base, renda_real_2013T3 = pnad$renda_real_2013T3)
des_sal_real <- subset(des_base,
                       ocupado == 1 & !is.na(renda_real_2013T3) & renda_real_2013T3 > 0)

# Regressão linear ponderada: renda_real ~ crise2016
# O coeficiente de crise2016 é a diferença de médias 2016 - 2013 em R$ de set/2013
fit  <- svyglm(renda_real_2013T3 ~ crise2016, design = des_sal_real)
beta <- coef(fit)["crise2016"]
se   <- sqrt(vcov(fit)["crise2016", "crise2016"])
ci   <- confint(fit)["crise2016", ]
pval <- summary(fit)$coefficients["crise2016", "Pr(>|t|)"]

cat(sprintf(
  "\nΔ(2016-2013) REAL = R$ %.2f | SE = %.2f | IC95%% = [%.2f, %.2f] | p = %.4g\n",
  beta, se, ci[1], ci[2], pval
))

# Tabela resumo da diferença real
tab_real <- data.frame(
  Indicador                 = "Δ REAL (2016-2013)",
  `Estimativa (R$ 2013-T3)` = as.numeric(beta),
  `Erro-padrão`             = as.numeric(se),
  `IC95%`                   = sprintf("[%.2f, %.2f]", ci[1], ci[2]),
  `p-valor`                 = format.pval(pval, eps = 1e-4, digits = 3, scientific = TRUE),
  Sig                       = ifelse(pval < 0.001, "***",
                              ifelse(pval < 0.01,  "**",
                              ifelse(pval < 0.05,  "*", ""))),
  check.names = FALSE
)

print(
  gt(tab_real) |>
    tab_header(
      title    = md("**Diferença de médias - renda real (base 2013-T3)**"),
      subtitle = md("Deflator IPCA set/2016 ÷ set/2013 = 1,268 | Ocupados; 21-50; urbano metropolitano; chefes")
    ) |>
    fmt_number(columns = c(`Estimativa (R$ 2013-T3)`, `Erro-padrão`),
               decimals = 2, use_seps = TRUE) |>
    cols_align("center", columns = everything())
)

# Salva em CSV para referência
write.csv(tab_real, "outputs/tab_a_renda_real.csv", row.names = FALSE)

message("02_renda_desemprego.R concluído.")
