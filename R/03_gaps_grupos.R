# Gaps de renda e desemprego por sexo, raça e escolaridade, 2013 vs 2016
#
# Analisa três dimensões:
#   b1. Sexo            (Homem vs Mulher)
#   b2. Raça/Cor        (Branca vs Pretos/Pardos)
#   b3. Escolaridade    (Fundamental / Médio / Superior)
#
# Para cada dimensão:
#   - svyby  - médias ponderadas por período x grupo
#   - svyttest por período - testa se o gap é significativo em 2013 e em 2016
#   - svyglm com interação crise2016 x grupo - testa se o gap MUDOU no tempo

source("R/02_renda_desemprego.R")   # garante que des_base e pnad já estão prontos


# Variável de renda a usar (real se disponível, nominal como fallback) ----
renda_var <- if ("renda_real_2013T3" %in% names(pnad)) "renda_real_2013T3" else "renda"

# Atualiza o design com renda_use para manter nomenclatura consistente
# usa des_base$variables[[...]] em vez de get()
des_base <- update(des_base, renda_use = des_base$variables[[renda_var]])


# Variáveis de grupo ----

# Sexo (V2007: 1=Homem, 2=Mulher)
sexo_fac <- factor(
  ifelse(to_num(pnad$V2007) == 2, "Mulher", "Homem"),
  levels = c("Homem", "Mulher")
)

# Raça/cor - binária: Branca vs Pretos/Pardos (exclui Amarela, Indígena)
# V2010: 1=Branca, 2=Preta, 3=Amarela, 4=Parda, 5=Indígena
rnum     <- to_num(pnad$V2010)
raca_fac <- factor(
  ifelse(rnum %in% c(2, 4), "PP",
  ifelse(rnum == 1, "Branca", NA_character_)),
  levels = c("Branca", "PP")
)

# Escolaridade - três níveis
# Prioriza VD3004 (nível de instrução consolidado); fallback: V3002A (anos de estudo)
esc3 <- rep(NA_character_, nrow(pnad))

if ("VD3004" %in% names(pnad)) {
  # VD3004: 1=Sem instrução, 2-3=Fundamental, 4-5=Médio, 6-7=Superior
  v <- to_num(pnad$VD3004)
  esc3[v %in% c(2, 3)] <- "Fundamental"
  esc3[v %in% c(4, 5)] <- "Médio"
  esc3[v >= 6]          <- "Superior"
}

if (all(is.na(esc3)) && "V3002A" %in% names(pnad)) {
  # fallback por anos de estudo: <9=Fundamental, 9-11=Médio, >=12=Superior
  a <- to_num(pnad$V3002A)
  esc3[a < 9]            <- "Fundamental"
  esc3[a >= 9 & a < 12]  <- "Médio"
  esc3[a >= 12]          <- "Superior"
}

esc_fac <- factor(esc3, levels = c("Fundamental", "Médio", "Superior"))

# Registra as variáveis de grupo no design
des_base <- update(des_base,
                   pea     = as.numeric(VD4002 %in% c(1, 2)),
                   desocup = as.numeric(VD4002 == 2),
                   sexo    = factor(sexo_fac, levels = c("Homem", "Mulher")),
                   raca2   = factor(raca_fac, levels = c("Branca", "PP")),
                   esc3    = factor(esc_fac,  levels = c("Fundamental", "Médio", "Superior")))

# Subdesigns principais
des_pea   <- subset(update(des_base,
                           pea     = as.numeric(VD4002 %in% c(1, 2)),
                           desocup = as.numeric(VD4002 == 2)),
                    pea == 1)
des_renda <- subset(des_base, ocupado == 1 & !is.na(renda_use) & renda_use > 0)


# Checagens rápidas (distribuição cruzada) ----
cat("\n[CHECK] distribuição crise2016 x esc3 (renda):\n")
print(with(des_renda$variables, table(crise2016, esc3, useNA = "ifany")))

z <- qnorm(0.975)   # quantil para IC 95% (Normal)

cat("\n[CHECK] renda por período x esc3:\n")
rend_esc_chk <- svyby(~renda_use, ~crise2016 + esc3, des_renda,
                      svymean, na.rm = TRUE, vartype = "se")
rend_esc_chk$ci_l <- rend_esc_chk$renda_use - z * rend_esc_chk$se
rend_esc_chk$ci_u <- rend_esc_chk$renda_use + z * rend_esc_chk$se
print(rend_esc_chk)

cat("\n[CHECK] desemprego por período x esc3:\n")
txd_esc_chk <- svyby(~desocup, ~crise2016 + esc3, des_pea,
                     svymean, na.rm = TRUE, vartype = "se")
txd_esc_chk$ci_l <- txd_esc_chk$desocup - z * txd_esc_chk$se
txd_esc_chk$ci_u <- txd_esc_chk$desocup + z * txd_esc_chk$se
print(txd_esc_chk)


# Função auxiliar: teste de gap entre dois grupos num período específico ----
# Cria uma dummy binária dentro do subdesign filtrado e chama svyttest,
# garantindo que o design amostral seja respeitado na comparação.
mk_gap_test <- function(des, y, grp, lvl_low, lvl_high, periodo) {
  vars <- des$variables
  keep <- vars$crise2016 == periodo &
          !is.na(vars[[grp]])      &
          vars[[grp]] %in% c(lvl_low, lvl_high)
  dsub <- subset(des, keep)

  dsub$variables$grp2 <- factor(
    ifelse(dsub$variables[[grp]] == lvl_high, lvl_high, lvl_low),
    levels = c(lvl_low, lvl_high)
  )

  svyttest(as.formula(paste0(y, " ~ grp2")), design = dsub)
}


# b1. SEXO
cat("\n=== (b1) SEXO - Renda média e Desemprego ===\n")

rend_sexo <- svyby(~renda_use, ~crise2016 + sexo, des_renda,
                   svymean, na.rm = TRUE, vartype = c("se", "ci"))
print(rend_sexo)

txd_sexo <- svyby(~desocup, ~crise2016 + sexo, des_pea,
                  svymean, na.rm = TRUE, vartype = c("se", "ci"))
print(txd_sexo)

# Testes de gap por período
cat("\n[SEXO] Gap de renda por período:\n")
print(svyttest(renda_use ~ sexo, subset(des_renda, crise2016 == 0)))  # 2013
print(svyttest(renda_use ~ sexo, subset(des_renda, crise2016 == 1)))  # 2016

cat("\n[SEXO] Gap de desemprego por período:\n")
print(svyttest(desocup ~ sexo, subset(des_pea, crise2016 == 0)))
print(svyttest(desocup ~ sexo, subset(des_pea, crise2016 == 1)))

# Interação: o coeficiente de crise2016:sexoMulher testa se o gap Mulher-Homem
# mudou entre 2013 e 2016
fit_rend_sexo <- svyglm(renda_use ~ crise2016 * sexo, design = des_renda)
fit_des_sexo  <- svyglm(desocup   ~ crise2016 * sexo, design = des_pea)  # LPM

cat("\n[SEXO] Variação do gap - interação crise2016xsexo (renda):\n")
print(summary(fit_rend_sexo)$coefficients)
cat("\n[SEXO] Variação do gap - interação crise2016xsexo (desemprego):\n")
print(summary(fit_des_sexo)$coefficients)


# b2. RAÇA/COR (Branca vs Pretos/Pardos)
cat("\n=== (b2) RAÇA/COR - Renda média e Desemprego ===\n")

# Restringe aos dois grupos comparáveis (exclui Amarela, Indígena)
des_renda_raca2 <- subset(des_renda, !is.na(raca2))
des_pea_raca2   <- subset(des_pea,   !is.na(raca2))

rend_raca <- svyby(~renda_use, ~crise2016 + raca2, des_renda_raca2,
                   svymean, na.rm = TRUE, vartype = c("se", "ci"))
print(rend_raca)

txd_raca <- svyby(~desocup, ~crise2016 + raca2, des_pea_raca2,
                  svymean, na.rm = TRUE, vartype = c("se", "ci"))
print(txd_raca)

cat("\n[RAÇA] Gap de renda por período:\n")
print(svyttest(renda_use ~ raca2, subset(des_renda_raca2, crise2016 == 0)))
print(svyttest(renda_use ~ raca2, subset(des_renda_raca2, crise2016 == 1)))

cat("\n[RAÇA] Gap de desemprego por período:\n")
print(svyttest(desocup ~ raca2, subset(des_pea_raca2, crise2016 == 0)))
print(svyttest(desocup ~ raca2, subset(des_pea_raca2, crise2016 == 1)))

fit_rend_raca <- svyglm(renda_use ~ crise2016 * raca2, design = des_renda_raca2)
fit_des_raca  <- svyglm(desocup   ~ crise2016 * raca2, design = des_pea_raca2)

cat("\n[RAÇA] Variação do gap - interação crise2016xraca2 (renda):\n")
print(summary(fit_rend_raca)$coefficients)
cat("\n[RAÇA] Variação do gap - interação crise2016xraca2 (desemprego):\n")
print(summary(fit_des_raca)$coefficients)


# b3. ESCOLARIDADE (Fundamental / Médio / Superior)
cat("\n=== (b3) ESCOLARIDADE - Renda média e Desemprego ===\n")

rend_esc <- svyby(~renda_use, ~crise2016 + esc3, des_renda,
                  svymean, na.rm = TRUE, vartype = "se")
rend_esc$ci_l <- rend_esc$renda_use - z * rend_esc$se
rend_esc$ci_u <- rend_esc$renda_use + z * rend_esc$se
print(rend_esc)

txd_esc <- svyby(~desocup, ~crise2016 + esc3, des_pea,
                 svymean, na.rm = TRUE, vartype = "se")
txd_esc$ci_l <- txd_esc$desocup - z * txd_esc$se
txd_esc$ci_u <- txd_esc$desocup + z * txd_esc$se
print(txd_esc)

# Testes de gap entre pares de níveis, por período
cat("\n[ESC] Gap renda: Médio - Fundamental (2013):\n")
print(mk_gap_test(des_renda, "renda_use", "esc3", "Fundamental", "Médio",    0))
cat("\n[ESC] Gap renda: Médio - Fundamental (2016):\n")
print(mk_gap_test(des_renda, "renda_use", "esc3", "Fundamental", "Médio",    1))
cat("\n[ESC] Gap renda: Superior - Médio (2013):\n")
print(mk_gap_test(des_renda, "renda_use", "esc3", "Médio",       "Superior", 0))
cat("\n[ESC] Gap renda: Superior - Médio (2016):\n")
print(mk_gap_test(des_renda, "renda_use", "esc3", "Médio",       "Superior", 1))

cat("\n[ESC] Gap desemprego: Médio - Fundamental (2013):\n")
print(mk_gap_test(des_pea,   "desocup",   "esc3", "Fundamental", "Médio",    0))
cat("\n[ESC] Gap desemprego: Médio - Fundamental (2016):\n")
print(mk_gap_test(des_pea,   "desocup",   "esc3", "Fundamental", "Médio",    1))
cat("\n[ESC] Gap desemprego: Superior - Médio (2013):\n")
print(mk_gap_test(des_pea,   "desocup",   "esc3", "Médio",       "Superior", 0))
cat("\n[ESC] Gap desemprego: Superior - Médio (2016):\n")
print(mk_gap_test(des_pea,   "desocup",   "esc3", "Médio",       "Superior", 1))

# Interação período x grupo para escolaridade
# A categoria de referência é Fundamental; os coeficientes de interação
# medem a mudança dos gaps Médio-Fundamental e Superior-Médio entre os períodos
fit_rend_esc <- svyglm(renda_use ~ crise2016 * esc3, design = des_renda)
fit_des_esc  <- svyglm(desocup   ~ crise2016 * esc3, design = des_pea)

cat("\n[ESC] Variação do gap - interação crise2016xesc3 (renda):\n")
print(summary(fit_rend_esc)$coefficients)
cat("\n[ESC] Variação do gap - interação crise2016xesc3 (desemprego):\n")
print(summary(fit_des_esc)$coefficients)

cat("\n>>> Leitura dos resultados:\n")
cat("  svyby        : médias ponderadas e IC 95% por período x grupo\n")
cat("  svyttest     : testa se o gap entre grupos é significativo em cada período\n")
cat("  svyglm (interação) : coeficiente de interação testa se o GAP mudou entre 2013 e 2016\n")


# Resumo mínimo - extrai estimativas de A, B e C em tabela única ----

# helpers para extrair resultados de svyttest e svyglm de forma padronizada
grab_ttest <- function(obj, label) {
  data.frame(
    Indicador  = label,
    Estimativa = unname(obj$estimate),       # diferença: 2º grupo - 1º grupo
    CI_low     = unname(obj$conf.int[1]),
    CI_high    = unname(obj$conf.int[2]),
    `p-valor`  = obj$p.value,
    Stat       = unname(obj$statistic),
    GL         = unname(obj$parameter),
    check.names = FALSE
  )
}

grab_term <- function(fit, term, label) {
  co <- unname(coef(fit)[term])
  se <- sqrt(vcov(fit)[term, term])
  ci <- suppressMessages(confint(fit)[term, ])
  p  <- summary(fit)$coefficients[term, "Pr(>|t|)"]
  data.frame(
    Indicador  = label,
    Estimativa = co,
    CI_low     = unname(ci[1]),
    CI_high    = unname(ci[2]),
    `p-valor`  = p,
    SE         = se,
    check.names = FALSE
  )
}

# Sexo
res_sexo <- bind_rows(
  grab_ttest(svyttest(renda_use ~ sexo, subset(des_renda, crise2016 == 0)),
             "SEXO | Gap renda 2013 (Mulher - Homem)"),
  grab_ttest(svyttest(renda_use ~ sexo, subset(des_renda, crise2016 == 1)),
             "SEXO | Gap renda 2016 (Mulher - Homem)"),
  grab_term(fit_rend_sexo, "crise2016:sexoMulher",
            "SEXO | ΔGAP renda (2016-2013)"),
  grab_ttest(svyttest(desocup ~ sexo, subset(des_pea, crise2016 == 0)),
             "SEXO | Gap desemprego 2013 (Mulher - Homem)"),
  grab_ttest(svyttest(desocup ~ sexo, subset(des_pea, crise2016 == 1)),
             "SEXO | Gap desemprego 2016 (Mulher - Homem)"),
  grab_term(fit_des_sexo, "crise2016:sexoMulher",
            "SEXO | ΔGAP desemprego (2016-2013)")
)

# Raça/cor
res_raca <- bind_rows(
  grab_ttest(svyttest(renda_use ~ raca2, subset(des_renda_raca2, crise2016 == 0)),
             "RAÇA | Gap renda 2013 (PP - Branca)"),
  grab_ttest(svyttest(renda_use ~ raca2, subset(des_renda_raca2, crise2016 == 1)),
             "RAÇA | Gap renda 2016 (PP - Branca)"),
  grab_term(fit_rend_raca, "crise2016:raca2PP",
            "RAÇA | ΔGAP renda (2016-2013)"),
  grab_ttest(svyttest(desocup ~ raca2, subset(des_pea_raca2, crise2016 == 0)),
             "RAÇA | Gap desemprego 2013 (PP - Branca)"),
  grab_ttest(svyttest(desocup ~ raca2, subset(des_pea_raca2, crise2016 == 1)),
             "RAÇA | Gap desemprego 2016 (PP - Branca)"),
  grab_term(fit_des_raca, "crise2016:raca2PP",
            "RAÇA | ΔGAP desemprego (2016-2013)")
)

# Escolaridade
res_esc <- bind_rows(
  grab_term(fit_rend_esc, "crise2016:esc3Médio",
            "ESC | ΔGAP renda (Médio - Fundamental)"),
  grab_term(fit_rend_esc, "crise2016:esc3Superior",
            "ESC | ΔGAP renda (Superior - Médio)"),
  grab_term(fit_des_esc,  "crise2016:esc3Médio",
            "ESC | ΔGAP desemprego (Médio - Fundamental)"),
  grab_term(fit_des_esc,  "crise2016:esc3Superior",
            "ESC | ΔGAP desemprego (Superior - Médio)")
)

# Consolida e padroniza
resumo_minimo <- bind_rows(res_sexo, res_raca, res_esc) |>
  mutate(
    Estimativa = round(Estimativa, 4),
    CI_low     = round(CI_low,     4),
    CI_high    = round(CI_high,    4),
    `p-valor`  = signif(`p-valor`, 3)
  )

# SE ausente nos t-tests: aproxima a partir do IC95%
resumo_minimo <- resumo_minimo |>
  mutate(SE = ifelse(is.na(SE), (CI_high - CI_low) / (2 * 1.96), SE))

# Estatística t e graus de liberdade para linhas de interação (svyglm)
gl_renda <- survey::degf(des_renda)
gl_des   <- survey::degf(des_pea)

resumo_minimo <- resumo_minimo |>
  mutate(
    is_modelo = grepl("ΔGAP", Indicador),
    Stat = ifelse(is.na(Stat) & is_modelo, Estimativa / SE, Stat),
    GL   = ifelse(is.na(GL) & is_modelo & grepl("renda",     Indicador), gl_renda,
           ifelse(is.na(GL) & is_modelo & grepl("desemprego", Indicador), gl_des, GL))
  ) |>
  select(-is_modelo)

# Salva resultados
write.csv(resumo_minimo, "outputs/tab_b_resumo_gaps.csv", row.names = FALSE)

if (requireNamespace("gt", quietly = TRUE)) {
  gt(resumo_minimo) |>
    tab_header(
      title    = md("**Resumo - gaps e sua variação entre 2013 e 2016**"),
      subtitle = md("Estimativa, IC 95% e p-valor")
    ) |>
    fmt_number(columns = c(Estimativa, CI_low, CI_high), decimals = 2) |>
    gtsave("outputs/tab_b_resumo_gaps.html")
}

message("03_gaps_grupos.R concluído.")
