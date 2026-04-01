# =============================================================================
#
# "Who Suffers the Heat? Partial Adaptation and Persistent Inequalities in France"
# Ecological Economics 241 (2026) 108873
# =============================================================================
# These sections correspond to analyses in the published paper following comments of reviewers

# =============================================================================


# =====================================================================
# 1. TABLE 5 — ROBUSTNESS: ALTERNATIVE FIXED EFFECTS (1980–2019)
# =====================================================================

library(fixest)

summary(feolss1 <- feols(
  taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
    five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
    humidity_bin_moins_20_part + humidity_bin_40_60_part +
    humidity_bin_60_80_part + humidity_bin_plus_80_part + rain_bin_0_3_part +
    rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
    wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | 
    COM^mois + mois^year + DEP^year,
  data     = communes_dates_1980_2022_temperature_final_mois,
  se       = "cluster",
  cluster  = ~COM,
  weights  = ~value_estimated_population
))


# 2) FE alternatifs : remplacer DEP×year par year
feolss2 <- feols(
  taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
    five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
    humidity_bin_moins_20_part + humidity_bin_40_60_part +
    humidity_bin_60_80_part + humidity_bin_plus_80_part + rain_bin_0_3_part +
    rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
    wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part |
    COM^mois + mois^year + year,
  data     = communes_dates_1980_2022_temperature_final_mois,
  se       = "cluster",
  cluster  = ~ COM,
  weights  = ~ value_estimated_population
)

# 3) FE alternatifs : remplacer mois×year par mois
feolss3 <- feols(
  taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
    five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
    humidity_bin_moins_20_part + humidity_bin_40_60_part +
    humidity_bin_60_80_part + humidity_bin_plus_80_part + rain_bin_0_3_part +
    rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
    wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part |
    COM^mois + mois + DEP^year,
  data     = communes_dates_1980_2022_temperature_final_mois,
  se       = "cluster",
  cluster  = ~ COM,
  weights  = ~ value_estimated_population
)

# 4) Exclure les autres contrôles météo (garder seulement les binnings de T°)
feolss4 <- feols(
  taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
    five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 |
    COM^mois + mois^year + DEP^year,
  data     = communes_dates_1980_2022_temperature_final_mois,
  se       = "cluster",
  cluster  = ~ COM,
  weights  = ~ value_estimated_population
)

# -- Tableau comparatif --
etable(
  list(
    "Baseline"                     = feolss1,
    "Year FE (vs DEP×Year)"        = feolss2,
    "Month FE (vs Month×Year)"     = feolss3,
    "No humidity/rain/wind ctrls"  = feolss4
  ), tex = TRUE
)


# =====================================================================
# 2. FIGURE 4 — 4-PANEL FACETED PLOT: INCOME × DENSITY (2001–2019)
# =====================================================================

library(dplyr)
library(fixest)
library(ggplot2)
library(grid)
library(scales)
library(viridis)


communes_dates_1980_2022_temperature_final_mois$income_scale1 = as.numeric(scale(communes_dates_1980_2022_temperature_final_mois$mediane))

# Sous-échantillons densité + période
base_high_density <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  value_estimated_sum_densite > 300, year >= 2001, year <= 2019
)
base_low_density  <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  value_estimated_sum_densite < 300, year >= 2001, year <= 2019
)

# ================== 1) ESTIMATION DES MODÈLES ==================
fml_int <- taux_mortalite_total ~
  (below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight) * income_scale1 +
  humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
  rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
  wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part |
  COM^mois + mois^year + DEP^year

m_ld <- feols(fml_int, data = base_low_density,
              se = "cluster", cluster = ~ COM,
              weights = ~ value_estimated_population)

m_hd <- feols(fml_int, data = base_high_density,
              se = "cluster", cluster = ~ COM,
              weights = ~ value_estimated_population)

# ================== 2) OUTILS ROBUSTES POUR LES EFFETS ==================
# Trouve le nom de l'interaction bin × income_scale1, quel que soit l'ordre
int_name <- function(m, bin, inc_var = "income_scale1") {
  cn <- names(coef(m))
  # Essais directs
  cand <- c(paste0(bin, ":", inc_var), paste0(inc_var, ":", bin))
  hit  <- cand[cand %in% cn]
  if (length(hit) > 0) return(hit[1])
  # Fallback regex (au cas où il y ait des backticks ou transformations)
  rx <- paste0("(^|:)", bin, "(:|$)")
  rx2 <- paste0("(^|:)", inc_var, "(:|$)")
  ix <- which(grepl(rx, cn) & grepl(rx2, cn))
  if (length(ix) == 0) stop("Interaction manquante pour: ", bin, " × ", inc_var,
                            "\nNoms de coefficients:\n", paste(cn, collapse = ", "))
  cn[ix[1]]
}

# Vérifie/retourne le nom du terme de base pour le bin
base_name <- function(m, bin) {
  cn <- names(coef(m))
  if (bin %in% cn) return(bin)
  # Fallback tolérant (rarement utile si les noms sont propres)
  ix <- which(grepl(paste0("(^|`)", bin, "($|`)"), cn))
  if (length(ix) == 0) stop("Terme de base manquant: ", bin,
                            "\nNoms de coefficients:\n", paste(cn, collapse = ", "))
  cn[ix[1]]
}

# Effet marginal + SE par delta-method pour un bin et un z donnés
one_bin_effect <- function(m, bin, z, inc_var = "income_scale1") {
  b  <- coef(m); V <- vcov(m)
  ib <- base_name(m, bin)
  ii <- int_name(m, bin, inc_var)
  eff <- as.numeric(b[ib]) + z * as.numeric(b[ii])
  se  <- sqrt(V[ib, ib] + z^2 * V[ii, ii] + 2 * z * V[ib, ii])
  c(eff = eff, se = se)
}

# Construit le DF pour un modèle (LD/HD) et un z donné
effects_for_model <- function(m, z, income_lab, density_lab) {
  bins <- c("below_minus_20_to_0","zero_to_15","twenty_to_28","above_twenty_eight")
  out <- lapply(bins, function(bin) {
    x <- one_bin_effect(m, bin, z, "income_scale1")
    data.frame(Temperature = bin, Effet = x["eff"], SE = x["se"])
  }) |> bind_rows()
  
  # Ajouter la catégorie de référence "15 to 20" (Effet=0, SE=0)
  out <- bind_rows(
    data.frame(Temperature = "fifteen_to_20", Effet = 0, SE = 0),
    out
  )
  
  out$Temperature_lbl <- factor(out$Temperature,
                                levels = c("below_minus_20_to_0","zero_to_15","fifteen_to_20","twenty_to_28","above_twenty_eight"),
                                labels = c("Below -20 to 0","0 to 15","15 to 20","20 to 28","Above 28")
  )
  out$Lower_CI <- out$Effet - 1.96 * out$SE
  out$Upper_CI <- out$Effet + 1.96 * out$SE
  out$Municipality_Type <- paste(income_lab, "&", density_lab)
  out
}

# ================== 3) DÉFINITION "PAUVRES/RICHES" EN Z-SCORE ==================
z_low  <- -1   # ≈ 16e centile
z_high <- +1   # ≈ 84e centile

# ================== 4) CONSTRUIRE LES 4 COURBES ==================
ld_low  <- effects_for_model(m_ld, z_low,  "Low income",  "low density")
ld_high <- effects_for_model(m_ld, z_high, "High income", "low density")
hd_low  <- effects_for_model(m_hd, z_low,  "Low income",  "high density")
hd_high <- effects_for_model(m_hd, z_high, "High income", "high density")

resultats <- bind_rows(ld_low, ld_high, hd_low, hd_high)

resultats$Municipality_Type <- factor(
  resultats$Municipality_Type,
  levels = c(
    "Low income & high density",
    "High income & high density",
    "Low income & low density",
    "High income & low density"
  )
)

# Sanity check : pas de NA
if (anyNA(resultats$Effet)) {
  stop("Des effets NA subsistent. Vérifie les noms de coefs avec:\n",
       "names(coef(m_ld))\n",
       "names(coef(m_hd))")
}

# ================== 5) FIGURE (4 panneaux, même Y, IC95%, étiquette >28°C) ==================
ymin <- min(resultats$Lower_CI, na.rm = TRUE)
ymax <- max(resultats$Upper_CI, na.rm = TRUE)
dy   <- 0.03 * (ymax - ymin)

lab_df <- resultats %>%
  filter(Temperature_lbl == "Above 28") %>%
  mutate(label = sprintf("%.2f", Effet))

# 2) Remet les couleurs (on garde le rouge pour "High income & low density")
pal4 <- viridis::viridis(4)
names(pal4) <- levels(resultats$Municipality_Type)
pal4["High income & low density"] <- "#D62728"



p_four_side <- ggplot(
  resultats,
  aes(x = Temperature_lbl, y = Effet, color = Municipality_Type, fill = Municipality_Type)
) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Municipality_Type), alpha = 0.20, color = NA) +
  geom_line(aes(group = Municipality_Type), size = 1) +
  geom_point(size = 2.8) +
  geom_label(
    data = lab_df,
    aes(label = label),
    nudge_y = dy,
    label.size = 0,
    label.padding = unit(0.12, "lines"),
    alpha = 0.18,
    show.legend = FALSE
  ) +
  facet_wrap(~ Municipality_Type, nrow = 1, scales = "fixed") +
  scale_color_manual(values = pal4) +
  scale_fill_manual(values  = pal4) +
  scale_y_continuous(
    limits = c(ymin, ymax),
    expand = expansion(mult = c(0, .05)),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Temperature bins (°C)",
    y = "Mortality rate per 10,000"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text      = element_text(face = "bold"),
    panel.grid.minor= element_blank(),
    panel.spacing   = unit(1.1, "lines")
  )

p_four_side


# =====================================================================
# 3. TABLE 2 — PRE-2003 SENSITIVITY FOR 75+ (three pre-windows)
# =====================================================================

library(dplyr)
library(fixest)

df <- communes_dates_1980_2022_temperature_final_mois

fml_75 <- taux_mortalite_75_plus ~ 
  below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
  five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
  humidity_bin_moins_20_part + humidity_bin_40_60_part +
  humidity_bin_60_80_part + humidity_bin_plus_80_part +
  rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
  wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part |
  COM^mois + mois^year + DEP^year

fit_window <- function(data, start_year, end_year) {
  dd <- data %>% dplyr::filter(year >= start_year, year <= end_year)
  feols(
    fml_75,
    data     = dd,
    se       = "cluster", 
    cluster  = ~ COM,
    weights  = ~ value_estimated_population
  )
}

# Pré exclu 2003 : 1990-2002
m_pre_excl <- fit_window(df, 1990, 2002)

# Pré inclu 2003 : 1990-2003
m_pre_incl <- fit_window(df, 1990, 2003)

# Nouveau pré long : 1980-2003
m_pre_1980_2003 <- fit_window(df, 1980, 2003)

etable(
  m_pre_excl, m_pre_incl, m_pre_1980_2003,
  tex = TRUE,
  headers = c(
    "Pre (1990-2002, excl. 2003)",
    "Pre (1990-2003, incl. 2003)",
    "Pre (1980-2003)"
  ),
  fitstat = ~ n + r2
)


# =====================================================================
# 4. FIGURE 6(b) — 3-BLOCK WINDOWS FOR 75+ (1990-2003, 2004-2015, 2016-2019)
# =====================================================================

library(dplyr)
library(fixest)
library(ggplot2)
library(tidyr)
library(tibble)

df <- communes_dates_1980_2022_temperature_final_mois

pre_incl <- c(1990L, 2003L)
post_blocks <- list(c(2004L, 2015L),
                    c(2016L, 2019L))

pal <- c(
  "Pre (1990-2003)"  = "#1f77b4",
  "Post (2004-2015)" = "#2ca02c",
  "Post (2016-2019)" = "#2ca02c"
)

fml_75 <- taux_mortalite_75_plus ~ 
  below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
  five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
  humidity_bin_moins_20_part + humidity_bin_40_60_part +
  humidity_bin_60_80_part + humidity_bin_plus_80_part +
  rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
  wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part |
  COM^mois + mois^year + DEP^year

coef_rhs <- c(
  "below_minus_20_to_minus_5","minus_5_to_0","zero_to_5",
  "five_to_10","ten_to_15","twenty_to_25","twentyfive_to_30","above_30",
  "humidity_bin_moins_20_part","humidity_bin_40_60_part",
  "humidity_bin_60_80_part","humidity_bin_plus_80_part",
  "rain_bin_0_3_part","rain_bin_10_100_part","rain_bin_plus_100_part","rain_bin_zero_part",
  "wind_bin_0_3_part","wind_bin_10_20_part","wind_bin_plus_20_part"
)

fit_window <- function(d, start_year, end_year, coef_name, fml,
                       needed_rhs = coef_rhs, min_obs = 200L,
                       allow_fe_relax = TRUE) {
  dd0 <- d %>% filter(year >= start_year, year <= end_year)
  n_raw <- nrow(dd0)
  
  keep_vars <- unique(c("taux_mortalite_75_plus","value_estimated_population","COM","mois","DEP","year", needed_rhs))
  dd <- dd0 %>%
    dplyr::select(dplyr::any_of(keep_vars)) %>%
    tidyr::drop_na()
  
  n_clean <- nrow(dd)
  if (n_clean < min_obs) {
    return(tibble::tibble(
      Estimate = NA_real_, SE = NA_real_, N = n_clean, n_raw = n_raw,
      start_year = start_year, end_year = end_year, status = "too_few_nonNA"
    ))
  }
  
  m <- tryCatch(
    feols(fml, data = dd, se = "cluster", cluster = ~ COM, weights = ~ value_estimated_population),
    error = function(e) e
  )
  
  # fallback if perfect collinearity with FE
  if (inherits(m, "error") && allow_fe_relax &&
      grepl("collinear with the fixed effects|estimation is void", conditionMessage(m), ignore.case = TRUE)) {
    fml_relax <- update(fml, . ~ . | COM^mois + year) # drop DEP^year in fallback
    m2 <- tryCatch(
      feols(fml_relax, data = dd, se = "cluster", cluster = ~ COM, weights = ~ value_estimated_population),
      error = function(e) e
    )
    if (!inherits(m2, "error")) m <- m2
  }
  
  if (inherits(m, "error")) {
    return(tibble::tibble(
      Estimate = NA_real_, SE = NA_real_, N = n_clean, n_raw = n_raw,
      start_year = start_year, end_year = end_year,
      status = paste0("error: ", conditionMessage(m))
    ))
  }
  
  b <- coef(m); V <- vcov(m)
  if (!coef_name %in% names(b)) {
    return(tibble::tibble(
      Estimate = NA_real_, SE = NA_real_, N = nobs(m), n_raw = n_raw,
      start_year = start_year, end_year = end_year, status = "coef_absent_or_dropped"
    ))
  }
  
  est <- unname(b[coef_name])
  se  <- sqrt(unname(V[coef_name, coef_name]))
  tibble::tibble(
    Estimate = est, SE = se, N = nobs(m), n_raw = n_raw,
    start_year = start_year, end_year = end_year, status = "ok"
  )
}

compute_series <- function(coef_name, fml) {
  res_pre <- fit_window(df, pre_incl[1], pre_incl[2], coef_name, fml) %>%
    mutate(Series = "Pre (1990-2003)",
           WindowLabel = sprintf("%d-%d", start_year, end_year))
  
  res_post <- lapply(post_blocks, function(be) {
    lab <- sprintf("%d-%d", be[[1]], be[[2]])
    fit_window(df, be[[1]], be[[2]], coef_name, fml) %>%
      mutate(Series = paste0("Post (", lab, ")"),
             WindowLabel = lab)
  }) %>% bind_rows()
  
  bind_rows(res_pre, res_post) %>%
    mutate(CI_low = Estimate - 1.96 * SE,
           CI_high = Estimate + 1.96 * SE)
}

make_plot <- function(tab) {
  lvl_x <- c("1990-2003", "2004-2015", "2016-2019")
  
  plot_df <- tab %>%
    filter(!is.na(Estimate), !is.na(SE)) %>%
    mutate(WindowLabel = factor(WindowLabel, levels = lvl_x),
           x_id = as.numeric(WindowLabel))
  
  # integer ticks
  y_min <- floor(min(plot_df$CI_low,  na.rm = TRUE))
  y_max <- ceiling(max(plot_df$CI_high, na.rm = TRUE))
  if (y_min == y_max) { y_min <- y_min - 1; y_max <- y_max + 1 }
  
  ggplot(plot_df, aes(x = x_id, y = Estimate, color = Series)) +
    geom_vline(xintercept = 1.5, linetype = "dashed", linewidth = 0.4, color = "grey50") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.18, linewidth = 0.6, alpha = 0.9) +
    geom_point(size = 2.8) +
    scale_color_manual(
      values = pal,
      breaks = c("Pre (1990-2003)", "Post (2004-2015)", "Post (2016-2019)"),
      guide = "none"  # hide legend
    ) +
    scale_x_continuous(
      breaks = 1:3,
      labels = lvl_x,
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    scale_y_continuous(
      breaks = seq(y_min, y_max, by = 1),
      limits = c(y_min, y_max)
    ) +
    labs(x = NULL, y = "coefficients for temperatures >30°C (95% CI)") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
}

tab_30 <- compute_series("above_30", fml_75)
gg_30  <- make_plot(tab_30)
print(gg_30)

# Diagnostics
diag_30 <- tab_30 %>%
  dplyr::select(Series, WindowLabel, N, n_raw, status) %>%
  arrange(Series, WindowLabel)
print(diag_30)


# =====================================================================
# 5. FIGURE 6(a) — 4-YEAR POST BLOCKS FOR 75+ (rolling windows)
# =====================================================================

library(dplyr)
library(fixest)
library(ggplot2)
library(tidyr)
library(tibble)

df <- communes_dates_1980_2022_temperature_final_mois

pre_incl <- c(1990L, 2003L)
first_post_year <- 2004L
max_year <- max(df$year, na.rm = TRUE)

make_post_blocks <- function(start, end, width = 4L) {
  if (start > end) return(list())
  starts <- seq(start, end, by = width)
  ends   <- pmin(starts + width - 1L, end)
  Map(function(a,b) c(a,b), starts, ends)
}
post_blocks <- make_post_blocks(first_post_year, max_year, width = 4L)

pal <- c(
  "Pre (1990-2003)"     = "#1f77b4",
  "Post (4-year blocks)"= "#2ca02c"
)

fml_75 <- taux_mortalite_75_plus ~ 
  below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
  five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
  humidity_bin_moins_20_part + humidity_bin_40_60_part +
  humidity_bin_60_80_part + humidity_bin_plus_80_part +
  rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
  wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part |
  COM^mois + mois^year + DEP^year

coef_rhs <- c(
  "below_minus_20_to_minus_5","minus_5_to_0","zero_to_5",
  "five_to_10","ten_to_15","twenty_to_25","twentyfive_to_30","above_30",
  "humidity_bin_moins_20_part","humidity_bin_40_60_part",
  "humidity_bin_60_80_part","humidity_bin_plus_80_part",
  "rain_bin_0_3_part","rain_bin_10_100_part","rain_bin_plus_100_part","rain_bin_zero_part",
  "wind_bin_0_3_part","wind_bin_10_20_part","wind_bin_plus_20_part"
)

# NB: fit_window already defined in section 4 above

compute_series <- function(coef_name, fml) {
  res_pre <- fit_window(df, pre_incl[1], pre_incl[2], coef_name, fml) %>%
    mutate(Series = "Pre (1990-2003)",
           WindowLabel = sprintf("%d-%d", start_year, end_year))
  
  res_post <- lapply(post_blocks, function(be) {
    fit_window(df, be[[1]], be[[2]], coef_name, fml) %>%
      mutate(Series = "Post (4-year blocks)",
             WindowLabel = sprintf("%d-%d", start_year, end_year))
  }) %>% bind_rows()
  
  bind_rows(res_pre, res_post) %>%
    mutate(
      CI_low  = Estimate - 1.96 * SE,
      CI_high = Estimate + 1.96 * SE
    )
}

make_plot <- function(tab) {
  post_labels <- vapply(post_blocks, function(be) sprintf("%d-%d", be[[1]], be[[2]]), character(1))
  lvl_x <- c(sprintf("%d-%d", pre_incl[1], pre_incl[2]), post_labels)
  
  plot_df <- tab %>% filter(!is.na(Estimate), !is.na(SE))
  if (nrow(plot_df) == 0) stop("No usable window (all NA).")
  
  plot_df$WindowLabel <- factor(plot_df$WindowLabel, levels = lvl_x)
  plot_df <- plot_df %>% mutate(x_id = as.numeric(WindowLabel))
  
  # Integer y ticks based on CI range
  y_min <- floor(min(plot_df$CI_low,  na.rm = TRUE))
  y_max <- ceiling(max(plot_df$CI_high, na.rm = TRUE))
  if (y_min == y_max) { y_min <- y_min - 1; y_max <- y_max + 1 }
  
  ggplot(plot_df, aes(x = x_id, y = Estimate, color = Series)) +
    geom_vline(xintercept = 1.5, linetype = "dashed", linewidth = 0.4, color = "grey50") + # pre/post separator
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.18, linewidth = 0.6, alpha = 0.9) +
    geom_point(size = 2.8) +
    scale_color_manual(
      values = pal,
      breaks = c("Pre (1990-2003)", "Post (4-year blocks)"),
      guide = "none"  # hide legend
    ) +
    scale_x_continuous(
      breaks = seq_along(lvl_x),
      labels = lvl_x,
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    scale_y_continuous(
      breaks = seq(y_min, y_max, by = 1),
      limits = c(y_min, y_max)
    ) +
    labs(x = NULL, y = "coefficients for temperatures >30°C (95% CI)") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
}

tab_30 <- compute_series("above_30", fml_75)
gg_30  <- make_plot(tab_30)
print(gg_30)

# Diagnostics
diag_30 <- tab_30 %>%
  dplyr::select(Series, WindowLabel, N, n_raw, status) %>%
  arrange(Series, WindowLabel)
print(diag_30)
