# Tabela resumo informativa por marcadores socioeconômicos
#
# Consolida em uma única tabela gt:
#   - Renda média real [IC95], mediana [P25-P75], CV e Δ 2016-2013
#   - Taxa de desemprego [IC95] e Δ em pontos percentuais
#   - Para sexo, raça/cor e escolaridade
#   - ΔGAP (interação período x grupo) por marcador
#
# ATENÇÃO: des_sal e des_un são redefinidos nesta seção com y_use.
#          Os objetos homônimos de 03_gaps_grupos.R são, aqui, sobrescritos.
#          Caso seja preciso preservá-los, devemos salvá-los antes de executar este script:
#            des_sal_b <- des_sal; des_un_b <- des_un

source("R/03_gaps_grupos.R")


# Define y_use (renda real se disponível, nominal como fallback) ----
if ("renda_real_2013T3" %in% names(des_base$variables)) {
  des_base <- update(des_base, y_use = renda_real_2013T3)
  y_label  <- "Renda real (base 2013-T3, R$)"
} else {
  des_base <- update(des_base, y_use = renda)
  y_label  <- "Renda nominal (R$)"
}

# Garante variáveis binárias de grupo no design
des_base <- update(
  des_base,
  sexo_fem = as.numeric(V2007 == 2),        # 1=Mulher
  raca_pp  = as.numeric(V2010 %in% c(2, 4)) # 1=Preto/Pardo
)

# Escolaridade (mesmo código de 03_gaps_grupos.R, aplicado ao des_base atualizado)
esc_nm <- intersect(c("VD3004", "V3002A"), names(des_base$variables))[1]
if (!is.na(esc_nm)) {
  esc_raw <- des_base$variables[[esc_nm]]
  esc_num <- suppressWarnings(as.numeric(as.character(esc_raw)))
  esc3_c  <- if (esc_nm == "VD3004") {
    ifelse(esc_num %in% c(2, 3), "Fundamental",
    ifelse(esc_num %in% c(4, 5), "Médio",
    ifelse(esc_num >= 6,         "Superior", NA)))
  } else {
    ifelse(esc_num < 9,  "Fundamental",
    ifelse(esc_num < 12, "Médio",
    ifelse(esc_num >= 12,"Superior", NA)))
  }
  des_base$variables$esc3 <- factor(esc3_c,
                                    levels = c("Fundamental", "Médio", "Superior"))
}

# Subdesigns para esta seção
des_sal <- subset(des_base, ocupado == 1 & !is.na(y_use) & y_use > 0)
des_un  <- subset(
  update(des_base,
         pea     = as.numeric(VD4002 %in% c(1, 2)),
         desocup = as.numeric(VD4002 == 2)),
  pea == 1
)

# Constante para contagem ponderada (svytotal)
des_sal <- update(des_sal, one = 1)
des_un  <- update(des_un,  one = 1)


# Funções auxiliares de extração estatística ----

# Máscara lógica para des_sal - período e subgrupo opcionais
# periodo=NULL inclui todos os períodos (usado em delta_p para o grupo inteiro)
mask_sal <- function(periodo, gvar = NULL, gval = NULL) {
  v <- des_sal$variables
  m <- if (is.null(periodo)) (v$y_use > 0) else (v$crise2016 == periodo) & (v$y_use > 0)
  if (!is.null(gvar)) m <- m & !is.na(v[[gvar]]) & v[[gvar]] == gval
  m
}

# Máscara lógica para des_un
mask_un <- function(periodo, gvar = NULL, gval = NULL) {
  v <- des_un$variables
  m <- if (is.null(periodo)) rep(TRUE, nrow(v)) else (v$crise2016 == periodo)
  if (!is.null(gvar)) m <- m & !is.na(v[[gvar]]) & v[[gvar]] == gval
  m
}

# Contagem amostral (n) e populacional estimada (N em unidades)
count_nN <- function(des, mask) {
  dsub <- subset(des, mask)
  n    <- nrow(dsub$variables)
  N    <- if (n > 0) as.numeric(coef(svytotal(~one, dsub))) else NA_real_
  c(n = n, N = N)
}

# Média ponderada com IC95% e coeficiente de variação
mean_ci_cv <- function(des, mask, y, pct = FALSE) {
  dsub <- subset(des, mask)
  if (nrow(dsub$variables) == 0)
    return(list(lbl = "", m = NA_real_, se = NA_real_, cv = NA_real_))
  m_obj <- svymean(as.formula(paste0("~", y)), dsub, na.rm = TRUE)
  mm <- as.numeric(coef(m_obj))
  ss <- as.numeric(SE(m_obj))
  cv <- ifelse(is.na(mm) || mm == 0, NA_real_, 100 * ss / mm)
  lbl <- if (pct)
    sprintf("%.2f%% (%.2f; %.2f)", 100 * mm, 100 * (mm - 1.96 * ss), 100 * (mm + 1.96 * ss))
  else
    sprintf("%.2f (%.2f; %.2f)", mm, mm - 1.96 * ss, mm + 1.96 * ss)
  list(lbl = lbl, m = mm, se = ss, cv = cv)
}

# Mediana e intervalo interquartil [P25-P75]
med_ijr <- function(des, mask, y) {
  dsub <- subset(des, mask)
  if (nrow(dsub$variables) == 0) return("")
  tryCatch({
    qv <- svyquantile(as.formula(paste0("~", y)), dsub,
                      c(.25, .5, .75), ci = FALSE, na.rm = TRUE)
    qq <- unname(as.numeric(coef(qv)))
    # Checagem antes do sprintf para evitar resultados inválidos
    if (length(qq) != 3 || any(!is.finite(qq))) return(NA_character_)
    sprintf("%.0f (%.0f-%.0f)", qq[2], qq[1], qq[3])
  }, error = function(e) "")
}

# Variação absoluta e relativa entre 2013 e 2016 (via svyglm)
delta_p <- function(des, y, mask) {
  dsub <- subset(des, mask)
  if (nrow(dsub$variables) == 0) return(list(delta = NA, se = NA, p = NA))
  m <- svyglm(as.formula(paste0(y, " ~ crise2016")), dsub)
  list(delta = unname(coef(m)["crise2016"]),
       se    = sqrt(vcov(m)["crise2016", "crise2016"]),
       p     = summary(m)$coefficients["crise2016", "Pr(>|t|)"])
}

# Formata variação absoluta e relativa para exibição
delta_abs_rel <- function(m13, m16, p) {
  d_abs <- m16 - m13
  d_rel <- ifelse(is.na(m13) || m13 == 0, NA, 100 * (d_abs / m13))
  list(
    lbl_abs = sprintf("%.2f %s", d_abs, stars(p)),
    lbl_rel = ifelse(is.na(d_rel), "", sprintf("%.1f%%", d_rel))
  )
}

# ΔGAP via interação período x grupo: variável binária (0/1)
deltagap_bin <- function(des, y, gvar) {
  f <- as.formula(paste0(y, " ~ crise2016 * ", gvar))
  m <- svyglm(f, design = des)
  k <- paste0("crise2016:", gvar)
  list(b  = unname(coef(m)[k]),
       se = sqrt(vcov(m)[k, k]),
       p  = summary(m)$coefficients[k, "Pr(>|t|)"])
}

# ΔGAP via interação período x grupo: par de categorias de variável ordinal
deltagap_pair <- function(des, y, gvar, lvl_high, lvl_low) {
  v    <- des$variables
  keep <- !is.na(v[[gvar]]) & v[[gvar]] %in% c(lvl_low, lvl_high)
  dd   <- subset(des, keep)
  if (nrow(dd$variables) == 0 ||
      length(unique(na.omit(dd$variables[[gvar]]))) < 2)
    return(list(b = NA, se = NA, p = NA))
  dd$variables$g_bin <- as.numeric(dd$variables[[gvar]] == lvl_high)
  m <- svyglm(as.formula(paste0(y, " ~ crise2016 * g_bin")), dd)
  list(b  = unname(coef(m)["crise2016:g_bin"]),
       se = sqrt(vcov(m)["crise2016:g_bin", "crise2016:g_bin"]),
       p  = summary(m)$coefficients["crise2016:g_bin", "Pr(>|t|)"])
}


# Construtores de painel ----

# Painel para variável binária (sexo, raça)
build_panel_binary2 <- function(label_panel, gvar, lab0, lab1) {

  # Renda
  e0_13 <- mean_ci_cv(des_sal, mask_sal(0, gvar, 0), "y_use")
  e0_16 <- mean_ci_cv(des_sal, mask_sal(1, gvar, 0), "y_use")
  e1_13 <- mean_ci_cv(des_sal, mask_sal(0, gvar, 1), "y_use")
  e1_16 <- mean_ci_cv(des_sal, mask_sal(1, gvar, 1), "y_use")
  n0_13 <- count_nN(des_sal, mask_sal(0, gvar, 0))
  n0_16 <- count_nN(des_sal, mask_sal(1, gvar, 0))
  n1_13 <- count_nN(des_sal, mask_sal(0, gvar, 1))
  n1_16 <- count_nN(des_sal, mask_sal(1, gvar, 1))
  med0_13 <- med_ijr(des_sal, mask_sal(0, gvar, 0), "y_use")
  med0_16 <- med_ijr(des_sal, mask_sal(1, gvar, 0), "y_use")
  med1_13 <- med_ijr(des_sal, mask_sal(0, gvar, 1), "y_use")
  med1_16 <- med_ijr(des_sal, mask_sal(1, gvar, 1), "y_use")
  d0      <- delta_p(des_sal, "y_use", mask_sal(NULL, gvar, 0))
  d1      <- delta_p(des_sal, "y_use", mask_sal(NULL, gvar, 1))
  d0_lbl  <- delta_abs_rel(e0_13$m, e0_16$m, d0$p)
  d1_lbl  <- delta_abs_rel(e1_13$m, e1_16$m, d1$p)

  # Desocupação
  u0_13  <- mean_ci_cv(des_un, mask_un(0, gvar, 0), "desocup", pct = TRUE)
  u0_16  <- mean_ci_cv(des_un, mask_un(1, gvar, 0), "desocup", pct = TRUE)
  u1_13  <- mean_ci_cv(des_un, mask_un(0, gvar, 1), "desocup", pct = TRUE)
  u1_16  <- mean_ci_cv(des_un, mask_un(1, gvar, 1), "desocup", pct = TRUE)
  nu0_13 <- count_nN(des_un, mask_un(0, gvar, 0))
  nu0_16 <- count_nN(des_un, mask_un(1, gvar, 0))
  nu1_13 <- count_nN(des_un, mask_un(0, gvar, 1))
  nu1_16 <- count_nN(des_un, mask_un(1, gvar, 1))
  du0    <- delta_p(des_un, "desocup", mask_un(NULL, gvar, 0))
  du1    <- delta_p(des_un, "desocup", mask_un(NULL, gvar, 1))

  # ΔGAP (interação)
  dg_r <- deltagap_bin(des_sal, "y_use",   gvar)
  dg_u <- deltagap_bin(des_un,  "desocup", gvar)

  tibble::tibble(
    Painel = label_panel,
    Grupo  = c(lab0, lab1, paste0("ΔGAP (", lab1, " - ", lab0, ")")),

    `n/N̂ 2013 (ocup.)`      = c(sprintf("%d / %.1f", n0_13["n"], n0_13["N"] / 1e6),
                                 sprintf("%d / %.1f", n1_13["n"], n1_13["N"] / 1e6), ""),
    `Média 2013 [IC95]`      = c(e0_13$lbl, e1_13$lbl, ""),
    `Mediana 2013 [P25-P75]` = c(med0_13,   med1_13,   ""),
    `CV 2013 (%)`            = c(sprintf("%.1f", e0_13$cv), sprintf("%.1f", e1_13$cv), ""),

    `n/N̂ 2016 (ocup.)`      = c(sprintf("%d / %.1f", n0_16["n"], n0_16["N"] / 1e6),
                                 sprintf("%d / %.1f", n1_16["n"], n1_16["N"] / 1e6), ""),
    `Média 2016 [IC95]`      = c(e0_16$lbl, e1_16$lbl, ""),
    `Mediana 2016 [P25-P75]` = c(med0_16,   med1_16,   ""),
    `CV 2016 (%)`            = c(sprintf("%.1f", e0_16$cv), sprintf("%.1f", e1_16$cv), ""),

    `Δ R$ (p)` = c(d0_lbl$lbl_abs, d1_lbl$lbl_abs,
                   sprintf("%.2f %s", dg_r$b, stars(dg_r$p))),
    `Δ %`      = c(d0_lbl$lbl_rel, d1_lbl$lbl_rel, ""),

    `n/N̂ 2013 (PEA)` = c(sprintf("%d / %.1f", nu0_13["n"], nu0_13["N"] / 1e6),
                          sprintf("%d / %.1f", nu1_13["n"], nu1_13["N"] / 1e6), ""),
    `2013 % [IC95]`   = c(u0_13$lbl, u1_13$lbl, ""),
    `n/N̂ 2016 (PEA)` = c(sprintf("%d / %.1f", nu0_16["n"], nu0_16["N"] / 1e6),
                          sprintf("%d / %.1f", nu1_16["n"], nu1_16["N"] / 1e6), ""),
    `2016 % [IC95]`   = c(u0_16$lbl, u1_16$lbl, ""),
    `Δ p.p. (p)`      = c(sprintf("%.2f %s", 100 * du0$delta, stars(du0$p)),
                          sprintf("%.2f %s", 100 * du1$delta, stars(du1$p)),
                          sprintf("%.2f %s", 100 * dg_u$b,    stars(dg_u$p))),
    `Δ % (taxa)`      = c(ifelse(!is.na(u0_13$m),
                                 sprintf("%.1f%%", 100 * (u0_16$m - u0_13$m) / u0_13$m), ""),
                          ifelse(!is.na(u1_13$m),
                                 sprintf("%.1f%%", 100 * (u1_16$m - u1_13$m) / u1_13$m), ""),
                          "")
  )
}

# Painel para escolaridade (3 categorias ordenadas + ΔGAPs entre pares)
build_panel_esc2 <- function() {
  if (!"esc3" %in% names(des_sal$variables)) return(NULL)
  L <- levels(des_sal$variables$esc3)
  L <- L[!is.na(L)]

  linhas <- lapply(L, function(lv) {
    e13 <- mean_ci_cv(des_sal, mask_sal(0, "esc3", lv), "y_use")
    e16 <- mean_ci_cv(des_sal, mask_sal(1, "esc3", lv), "y_use")
    n13 <- count_nN(des_sal, mask_sal(0, "esc3", lv))
    n16 <- count_nN(des_sal, mask_sal(1, "esc3", lv))
    m13 <- med_ijr(des_sal, mask_sal(0, "esc3", lv), "y_use")
    m16 <- med_ijr(des_sal, mask_sal(1, "esc3", lv), "y_use")
    d   <- delta_p(des_sal, "y_use", mask_sal(NULL, "esc3", lv))
    d_lbl <- delta_abs_rel(e13$m, e16$m, d$p)

    u13  <- mean_ci_cv(des_un, mask_un(0, "esc3", lv), "desocup", pct = TRUE)
    u16  <- mean_ci_cv(des_un, mask_un(1, "esc3", lv), "desocup", pct = TRUE)
    nu13 <- count_nN(des_un, mask_un(0, "esc3", lv))
    nu16 <- count_nN(des_un, mask_un(1, "esc3", lv))
    du   <- delta_p(des_un, "desocup", mask_un(NULL, "esc3", lv))

    tibble::tibble(
      Painel = "Escolaridade", Grupo = lv,
      `n/N̂ 2013 (ocup.)`      = sprintf("%d / %.1f", n13["n"], n13["N"] / 1e6),
      `Média 2013 [IC95]`      = e13$lbl,
      `Mediana 2013 [P25-P75]` = m13,
      `CV 2013 (%)`            = sprintf("%.1f", e13$cv),
      `n/N̂ 2016 (ocup.)`      = sprintf("%d / %.1f", n16["n"], n16["N"] / 1e6),
      `Média 2016 [IC95]`      = e16$lbl,
      `Mediana 2016 [P25-P75]` = m16,
      `CV 2016 (%)`            = sprintf("%.1f", e16$cv),
      `Δ R$ (p)`               = d_lbl$lbl_abs,
      `Δ %`                    = d_lbl$lbl_rel,
      `n/N̂ 2013 (PEA)`        = sprintf("%d / %.1f", nu13["n"], nu13["N"] / 1e6),
      `2013 % [IC95]`          = u13$lbl,
      `n/N̂ 2016 (PEA)`        = sprintf("%d / %.1f", nu16["n"], nu16["N"] / 1e6),
      `2016 % [IC95]`          = u16$lbl,
      `Δ p.p. (p)` = sprintf("%.2f %s", 100 * du$delta, stars(du$p)),
      `Δ % (taxa)` = ifelse(!is.na(u13$m),
                            sprintf("%.1f%%", 100 * (u16$m - u13$m) / u13$m), "")
    )
  })

  # ΔGAPs entre pares de nível de escolaridade
  g1 <- deltagap_pair(des_sal, "y_use",   "esc3", "Médio",    "Fundamental")
  g2 <- deltagap_pair(des_sal, "y_use",   "esc3", "Superior", "Médio")
  h1 <- deltagap_pair(des_un,  "desocup", "esc3", "Médio",    "Fundamental")
  h2 <- deltagap_pair(des_un,  "desocup", "esc3", "Superior", "Médio")

  emp <- c("", "")   # célula vazia para colunas sem valor nas linhas de ΔGAP

  dplyr::bind_rows(
    dplyr::bind_rows(linhas),
    tibble::tibble(
      Painel = "Escolaridade",
      Grupo  = c("ΔGAP (Médio - Fundamental)", "ΔGAP (Superior - Médio)"),
      `n/N̂ 2013 (ocup.)`      = emp, `Média 2013 [IC95]`      = emp,
      `Mediana 2013 [P25-P75]` = emp, `CV 2013 (%)`            = emp,
      `n/N̂ 2016 (ocup.)`      = emp, `Média 2016 [IC95]`      = emp,
      `Mediana 2016 [P25-P75]` = emp, `CV 2016 (%)`            = emp,
      `Δ R$ (p)` = c(sprintf("%.2f %s", g1$b, stars(g1$p)),
                     sprintf("%.2f %s", g2$b, stars(g2$p))),
      `Δ %`      = emp,
      `n/N̂ 2013 (PEA)` = emp, `2013 % [IC95]` = emp,
      `n/N̂ 2016 (PEA)` = emp, `2016 % [IC95]` = emp,
      `Δ p.p. (p)` = c(sprintf("%.2f %s", 100 * h1$b, stars(h1$p)),
                       sprintf("%.2f %s", 100 * h2$b, stars(h2$p))),
      `Δ % (taxa)` = emp
    )
  )
}


# Monta e renderiza a tabela final ----
tab_sexo <- build_panel_binary2("Sexo",         "sexo_fem", "Homem",          "Mulher")
tab_raca <- build_panel_binary2("Raça/Cor",      "raca_pp",  "Brancos/Outros", "Pretos/Pardos")
tab_esc  <- build_panel_esc2()

tab_c <- dplyr::bind_rows(tab_sexo, tab_raca, tab_esc)

gt_tab <- gt(tab_c, rowname_col = "Grupo", groupname_col = "Painel") |>
  tab_header(
    title    = md("**(c) Resumo - Renda e Desemprego por marcadores socioeconômicos**"),
    subtitle = md(paste0(
      y_label, " | 21-50 anos; urbano metropolitano; chefes - 2013-T3 vs 2016-T3",
      "<br><span style='font-size:12px'>",
      "Δ = 2016 - 2013 | N̂ em milhões | CV = 100·SE/média | ",
      "* p&lt;0,05 | ** p&lt;0,01 | *** p&lt;0,001",
      "</span>"
    ))
  ) |>
  tab_spanner(
    label   = y_label,
    columns = c(`n/N̂ 2013 (ocup.)`, `Média 2013 [IC95]`, `Mediana 2013 [P25-P75]`, `CV 2013 (%)`,
                `n/N̂ 2016 (ocup.)`, `Média 2016 [IC95]`, `Mediana 2016 [P25-P75]`, `CV 2016 (%)`,
                `Δ R$ (p)`, `Δ %`)
  ) |>
  tab_spanner(
    label   = "Taxa de desemprego (PEA)",
    columns = c(`n/N̂ 2013 (PEA)`, `2013 % [IC95]`,
                `n/N̂ 2016 (PEA)`, `2016 % [IC95]`,
                `Δ p.p. (p)`, `Δ % (taxa)`)
  ) |>
  cols_align("center", columns = everything()) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
  opt_row_striping() |>
  opt_table_lines() |>
  fmt_missing(columns = everything(), missing_text = "")

print(gt_tab)
# gtsave(gt_tab, "outputs/tab_c_resumo_marcadores.html")  # descomente para salvar

message("04_resumo_marcadores.R concluído.")
