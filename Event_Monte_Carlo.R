# ---- Pacotes --------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(fixest)
library(janitor)
library(ggplot2)
library(mvtnorm)
library(purrr)
library(tibble)
library(stringi)
library(MASS)
library(data.table)
library(broom)

# ---- Preparação da base ---------------------------------------------------------
df <- read_excel("C:/Users/carlo/OneDrive/Área de Trabalho/Phillips/margem/Margem_Equatorial.xlsx")

df %>% 
  rename(
    pib_nominal   = "Tabela 5938 - Produto interno bruto a preços correntes",
    emprego_formal= "estoque_medio_de_emprego_formal_caged",
    royalties_mil = "Tabela 2.19 – Distribuição de royalties sobre a produção de petróleo e de gás natural, segundo beneficiários (mil R$)",
    petroleo_prod = "Table 1.3 – Oil production, per location  (10³ barrels)",
    brent_price   = "Europe Brent Spot Price FOB (Dollars per Barrel)",
    cambio        = "3694 - Taxa de câmbio - Livre - Dólar americano (venda) - Média de período - anual",
    selic         = "432 - Média Taxa de juros - Meta Selic definida pelo Copom - % a.a.",
    ipca          = "ipca_anual - %",
    populacao     = "6579 - População residente estimada"
  ) %>% 
  mutate(
    ano = as.integer(ano),
    across(c(tratado, controle, fase_exploratoria, fase_desenvolvimento, fase_producao),
           ~ as.integer(.x))
  ) -> df

base <- df %>%
  clean_names() %>%
  mutate(
    estado    = as.factor(estado),
    ano       = as.integer(ano),
    pib_pc    = pib_nominal / pmax(populacao, 1),
    ln_pib_pc = log(pmax(pib_pc, 1e-9)),
    ln_emprego= log(pmax(emprego_formal, 1)),
    ln_oil    = log1p(pmax(petroleo_prod, 0)),
    ln_roy    = log1p(pmax(royalties_mil, 0)),
    brent     = brent_price,
    fx        = cambio,
    selic     = selic,
    infl      = ipca
  ) %>%
  arrange(estado, ano)

# ---- Coorte g robusta (sem warnings/Inf)
coortes <- base %>%
  group_by(estado) %>%
  summarise(
    g = if (any(fase_producao == 1, na.rm = TRUE)) min(ano[fase_producao == 1], na.rm = TRUE) else NA_integer_,
    .groups = "drop"
  )

base <- base %>% left_join(coortes, by = "estado")

# ---- Modelo econométrico (event-study) --------------------------------------------------
es_pib <- feols(
  ln_pib_pc ~ sunab(g, ano) + brent + fx + selic + infl | estado,
  data = base, cluster = ~estado
)

es_emp <- feols(
  ln_emprego ~ sunab(g, ano) + brent + fx + selic + infl | estado,
  data = base, cluster = ~estado
)

# ---- Validação e projeção --------------------------------------------------
wald(es_pib, c("ano::-4", "ano::-3", "ano::-2"))
coef(es_pib)[grep("ano::[1-9]", names(coef(es_pib)))]

ref_pib_estado <- base %>%
  group_by(estado) %>%
  filter(!is.na(pib_pc)) %>%
  slice_max(ano) %>%
  select(estado, pib_pc)

efeitos_pib <- c(0.03395, 0.02629, 0.02648, -0.03628, 0.04182)

proj_multi_pib <- ref_pib_estado %>%
  crossing(ano_rel = 1:5) %>%
  mutate(
    efeito_ln = efeitos_pib[ano_rel],
    efeito_acumulado = cumsum(efeito_ln),
    ano = 2025 + ano_rel,
    pib_pc_proj = pib_pc * exp(efeito_acumulado)
  )

coef(es_emp)[grep("ano::[1-9]", names(coef(es_emp)))]
efeitos_emp <- as.numeric(ef_emp_pos)[1:5]

proj_emp <- tibble(
  ano = 2026:2030,
  efeito_ln = efeitos_emp,
  efeito_pct = (exp(efeitos_emp) - 1) * 100,
  emprego_proj = emprego_proj
)

ref_emp_estado <- base %>%
  group_by(estado) %>%
  filter(!is.na(emprego_formal)) %>%
  slice_max(ano) %>%
  ungroup() %>%
  select(estado, emprego_formal)

proj_multi_emp <- ref_emp_estado %>%
  crossing(ano_rel = 1:5) %>%
  mutate(
    efeito_ln = efeitos_emp[ano_rel],
    efeito_acumulado = cumsum(efeito_ln),         
    ano = 2025 + ano_rel,
    emprego_proj = emprego_formal * exp(efeito_acumulado),
    ganho_abs = emprego_proj - emprego_formal,
    ganho_pct = (emprego_proj / emprego_formal - 1) * 100
  ) %>%
  group_by(estado) %>%
  arrange(estado, ano) %>%
  mutate(efeito_acumulado = cumsum(efeito_ln),   
         emprego_proj = first(emprego_formal) * exp(efeito_acumulado),
         ganho_abs = emprego_proj - first(emprego_formal),
         ganho_pct = (emprego_proj / first(emprego_formal) - 1) * 100) %>%
  ungroup()


# ---- Monte Carlo (PIB per capita) ------------------------------------------
S <- 5000
betas_draw_p <- rmvnorm(S, mean = as.numeric(P$b), sigma = P$V)  

mult_mat_p <- t(apply(betas_draw_p, 1, function(b) exp(cumsum(b)))) 
colnames(mult_mat_p) <- as.character(1:5)

mult_df_p <- as_tibble(mult_mat_p) %>%
  mutate(sim = row_number()) %>%
  pivot_longer(-sim, names_to = "ano_rel_chr", values_to = "mult") %>%
  mutate(ano_rel = as.integer(ano_rel_chr)) %>%
  select(sim, ano_rel, mult)

ref_pib_estado <- base %>%
  group_by(estado) %>%
  filter(!is.na(pib_pc)) %>%
  slice_max(ano, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(estado, pib0 = pib_pc)

anos_proj <- 2026:2030
mc_pib <- ref_pib_estado %>%
  tidyr::crossing(sim = unique(mult_df_p$sim), ano = anos_proj) %>%
  mutate(ano_rel = ano - 2025L) %>%
  left_join(mult_df_p, by = c("sim", "ano_rel")) %>%
  group_by(estado, sim) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(pib_pc_proj = first(pib0) * mult) %>%
  ungroup()

sum_pib <- mc_pib %>%
  group_by(estado, ano) %>%
  summarise(
    mean = mean(pib_pc_proj, na.rm = TRUE),
    p05  = quantile(pib_pc_proj, 0.05, na.rm = TRUE),
    p95  = quantile(pib_pc_proj, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Monte Carlo (Emprego) --------------------------------------------------
post_short_e <- c("ano::4","ano::5","ano::6","ano::7","ano::8")

cn_coef_e <- names(coef(es_emp))
rn_vcov_e <- rownames(vcov(es_emp))

post_short_e <- post_short_e[post_short_e %in% cn_coef_e]
stopifnot(length(post_short_e) > 0)

match_long_e <- function(s) {
  hit <- grep(paste0("^", s, ":"), rn_vcov_e, value = TRUE)
  if (length(hit) == 0) stop("Não achei na vcov (emprego) o termo: ", s)
  hit[1]
}
post_long_e <- vapply(post_short_e, match_long_e, character(1))

beta_hat_e <- coef(es_emp)[post_short_e]
V_e_full   <- vcov(es_emp)
V_e        <- V_e_full[post_long_e, post_long_e, drop = FALSE]
diag(V_e)  <- diag(V_e) + 1e-10

ref_emp_estado <- base %>%
  group_by(estado) %>%
  filter(!is.na(emprego_formal)) %>%
  slice_max(ano) %>%
  ungroup() %>%
  select(estado, emp_ref = emprego_formal)

set.seed(321)
S <- 2000
betas_draw_e <- mvtnorm::rmvnorm(S, mean = beta_hat_e, sigma = V_e)

mult_mat_e <- t(apply(betas_draw_e, 1, function(b) exp(cumsum(b))))
colnames(mult_mat_e) <- as.character(seq_len(ncol(mult_mat_e)))

mult_df_e <- as_tibble(mult_mat_e) %>%
  mutate(sim = row_number()) %>%
  pivot_longer(cols = -sim, names_to = "ano_rel_chr", values_to = "mult") %>%
  mutate(ano_rel = as.integer(ano_rel_chr)) %>%
  select(sim, ano_rel, mult)

anos <- 2026:2030
k <- ncol(mult_mat_e)  
anos_emp <- anos[seq_len(k)]

mc_emp <- ref_emp_estado %>%
  tidyr::crossing(sim = unique(mult_df_e$sim), ano = anos_emp) %>%
  mutate(ano_rel = ano - min(anos_emp) + 1L) %>%
  left_join(mult_df_e, by = c("sim","ano_rel")) %>%
  mutate(emp_proj = emp_ref * mult)

sum_emp <- mc_emp %>%
  group_by(estado, ano) %>%
  summarise(
    mean = mean(emp_proj, na.rm = TRUE),
    p05  = quantile(emp_proj, 0.05, na.rm = TRUE),
    p95  = quantile(emp_proj, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Gráficos --------------------------------------------------
proj_multi_pib %>%
  ggplot(aes(x = ano, y = pib_pc_proj, color = estado)) +
  geom_line(size = 1.2) +
  labs(title = "PIB per capita projetado (com exploração) — 2026 a 2030",
       x = NULL, y = "R$ por habitante") +
  theme_minimal()

sum_emp %>%
  filter(estado != "BRASIL") %>%  # ⬅️ remove BRASIL
  ggplot(aes(x = ano, y = mean, ymin = p05, ymax = p95, color = estado, fill = estado)) +
  geom_ribbon(alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  labs(
    title = "Emprego formal projetado (com exploração) — média e IC (P05–P95)",
    x = NULL,
    y = "Empregos (nível)"
  ) +
  theme_minimal()

ggplot(sum_pib, aes(x = ano, y = mean, group = estado)) +
  geom_line() + geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.15) +
  facet_wrap(~estado, scales = "free_y") +
  labs(title = "PIB per capita projetado (média e IC 5–95%) — 2026–2030",
       x = NULL, y = "R$ por habitante") +
  theme_minimal()

ggplot(sum_emp, aes(x = ano, y = mean, group = estado)) +
  geom_line() + geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.15) +
  facet_wrap(~estado, scales = "free_y") +
  labs(title = "Emprego formal projetado (média e IC 5–95%) — 2026–2030",
       x = NULL, y = "Nível de emprego") +
  theme_minimal()

mapa <- geobr::read_state(year = 2020, simplified = TRUE) %>%
  select(code_state, name_state, abbrev_state, geom) %>%   
  rename(sigla = abbrev_state)

var_pib_2030 <- var_pib_2030 %>%
  mutate(estado = toupper(as.character(estado)))

mapa_merged <- mapa %>%
  left_join(var_pib_2030, by = c("sigla" = "estado"))

ggplot(mapa_merged) +
  geom_sf(aes(fill = var_pct), color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "#d73027", mid = "#ffffbf", high = "#1a9850", midpoint = 0,
    labels = label_percent(accuracy = 0.1), name = "Variação até 2030"
  ) +
  labs(
    title = "Variação % do PIB per capita até 2030",
    subtitle = "Comparado ao baseline (≤ 2025) — Estados da Margem Equatorial",
    caption = "Elaboração própria (event study + Monte Carlo)"
  ) +
  theme_void() +
  theme(legend.position = "right")
