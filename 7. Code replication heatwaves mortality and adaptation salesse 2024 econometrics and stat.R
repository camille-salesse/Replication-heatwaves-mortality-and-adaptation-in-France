


# Replication Script for: Who Suffers the Heat? Partial Adaptation and Persistent Inequalities in France (date: June 2025)
# This script reproduces the econometric results presented in the paper.

# 1. Load Required Libraries

library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(tidyr)
library(plm)
library(fixest)
library(ggplot2)


####### OPEN HERE 

communes_dates_1980_2022_temperature_final_mois <- fread("/heatwave and mortality code and data/base_finale_NEW_final.csv")

###############


# Regressions

# Overall mortality rate (all ages)
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

# Mortality rate: 75 years and older
summary(feolss2 <- feols(
  taux_mortalite_75_plus ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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

# Mortality rate: 70–74 years
summary(feolss3 <- feols(
  taux_mortalite_70_74 ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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

# Mortality rate: 65–69 years
summary(feolss4 <- feols(
  taux_mortalite_65_69 ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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

# Mortality rate: 60–64 years
summary(feolss5 <- feols(
  taux_mortalite_60_64 ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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

# Mortality rate: 40–59 years
summary(feolss6 <- feols(
  taux_mortalite_40_59 ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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

# Mortality rate: 20–39 years
summary(feolss7 <- feols(
  taux_mortalite_20_39 ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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

# Mortality rate: 10–19 years
summary(feolss8 <- feols(
  taux_mortalite_10_19 ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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

# Mortality rate: 0–9 years
summary(feolss9 <- feols(
  taux_mortalite_0_9 ~ below_minus_20_to_minus_5 + minus_5_to_0 + zero_to_5 +
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


# 4. Table of Regression Results 

etable(feolss1, feolss2, feolss3, feolss4, feolss5, feolss6, feolss7, feolss8, feolss9, tex = TRUE)


# 5. Coefficient Plot: Temperature Bins (All Ages)

coefficients <- coef(feolss1)
conf_int     <- confint(feolss1)

coef_df <- data.frame(
  Variable    = c("below_minus_20_to_minus_5", names(coefficients)[-1]),
  Coefficient = coefficients,
  Lower_CI    = conf_int[, 1],
  Upper_CI    = conf_int[, 2]
)

coef_df <- coef_df[c(1:8),]

# Add reference bin (15–20 °C)
nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0, Lower_CI = 0, Upper_CI = 0)
coef_df        <- rbind(coef_df, nouvelle_ligne)

coef_df <- coef_df[c(1, 2, 3, 4, 5, 9, 6, 7, 8),]

coef_df <- coef_df %>%
  mutate(Variable = recode(Variable,
                           "above_30"                     = ">30",
                           "below_minus_20_to_minus_5"    = "<-20 to -5",
                           "five_to_10"                   = "5 to 10",
                           "minus_10_to_minus_5"          = "-10 to -5",
                           "minus_5_to_0"                 = "-5 to 0",
                           "twentyfive_to_30"             = "25 to 30",
                           "twenty_to_25"                 = "20 to 25",
                           "zero_to_5"                    = "0 to 5",
                           "ten_to_15"                    = "10 to 15",
                           "minus_15_to_minus_10"         = "-15 to -10"
  ))

# Plot
ggplot(coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient)) +
  geom_point() +
  geom_line(aes(group = 1), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, group = 1) +
  labs(
    title = "",
    x     = "Temperature bins (°C)",
    y     = "Mortality rate per 10,000"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text   = element_text(size = 12),
    axis.title  = element_text(size = 14, face = "bold"),
    plot.title  = element_blank()
  )


# 6. Coefficient Plot: >30°C by Age Group


# Extract coefficients for the ">30 °C" bin across age‑specific models
coef_values <- c(
  coef(feolss2)["above_30"],  # 75+
  coef(feolss3)["above_30"],  # 70–74
  coef(feolss4)["above_30"],  # 65–69
  coef(feolss5)["above_30"],  # 60–64
  coef(feolss6)["above_30"],  # 40–59
  coef(feolss7)["above_30"],  # 20–39
  coef(feolss8)["above_30"],  # 10–19
  coef(feolss9)["above_30"]   # 0–9
)

# Extract confidence intervals
conf_int_values <- rbind(
  confint(feolss2)["above_30", ],
  confint(feolss3)["above_30", ],
  confint(feolss4)["above_30", ],
  confint(feolss5)["above_30", ],
  confint(feolss6)["above_30", ],
  confint(feolss7)["above_30", ],
  confint(feolss8)["above_30", ],
  confint(feolss9)["above_30", ]
)

coef_df <- data.frame(
  AgeGroup   = c("75+", "70-74", "65-69", "60-64", "40-59", "20-39", "10-19", "0-9"),
  Coefficient = coef_values,
  Lower_CI    = conf_int_values[, 1],
  Upper_CI    = conf_int_values[, 2]
)

# Plot
ggplot(coef_df, aes(x = AgeGroup, y = Coefficient)) +
  geom_point(size = 2.5, color = "darkblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +
  labs(
    x = "Age Groups",
    y = "Coefficients for Temperatures > 30°C"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text   = element_text(size = 12),
    axis.title  = element_text(size = 14, face = "bold"),
    plot.title  = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )




# 1. Heat‑Wave Binary Variable

################################ Regressions

summary(feolss1 <-  feols(taux_mortalite_total ~ canicule_binaire +
                            humidity_bin_moins_20_part + humidity_bin_40_60_part +
                            humidity_bin_60_80_part + humidity_bin_plus_80_part +
                            rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part +
                            rain_bin_zero_part + wind_bin_0_3_part + wind_bin_10_20_part +
                            wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                          data    = communes_dates_1980_2022_temperature_final_mois,
                          se      = "cluster", cluster = ~COM,
                          weights = ~value_estimated_population))



summary(feolss2 <-  feols(taux_mortalite_75_plus ~ canicule_binaire +
                            humidity_bin_moins_20_part + humidity_bin_40_60_part +
                            humidity_bin_60_80_part + humidity_bin_plus_80_part +
                            rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part +
                            rain_bin_zero_part + wind_bin_0_3_part + wind_bin_10_20_part +
                            wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                          data    = communes_dates_1980_2022_temperature_final_mois,
                          se      = "cluster", cluster = ~COM,
                          weights = ~value_estimated_population))



etable(feolss1, feolss2, tex = TRUE)


# 2. Pre‑2003 vs Post‑2003 Comparison


base__pre2003  <- filter(communes_dates_1980_2022_temperature_final_mois, year < 2004)
base__post2003 <- filter(communes_dates_1980_2022_temperature_final_mois, year >= 2004)

# Total mortality – Pre‑2003
summary(feolss1 <-  feols(taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                            zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                            humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                            humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                            rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                            wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                          data    = base__pre2003,
                          se      = "cluster", cluster = ~COM,
                          weights = ~value_estimated_population))



# Total mortality – Post‑2003
summary(feolss2 <-  feols(taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                            zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                            humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                            humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                            rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                            wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                          data    = base__post2003,
                          se      = "cluster", cluster = ~COM,
                          weights = ~value_estimated_population))

#

# 75+ mortality – Pre‑2003
summary(feolss3 <-  feols(taux_mortalite_75_plus ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                            zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                            humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                            humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                            rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                            wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                          data    = base__pre2003,
                          se      = "cluster", cluster = ~COM,
                          weights = ~value_estimated_population))

#

# 75+ mortality – Post‑2003
summary(feolss4 <-  feols(taux_mortalite_75_plus ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                            zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                            humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                            humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                            rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                            wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                          data    = base__post2003,
                          se      = "cluster", cluster = ~COM,
                          weights = ~value_estimated_population))

#

etable(feolss1, feolss2, feolss3, feolss4, tex = TRUE)

# 2. Visual Comparison: Pre vs Post 2003


coefficients   <- coef(feolss1)
conf_int       <- confint(feolss1)
coefficients2  <- coef(feolss2)
conf_int2      <- confint(feolss2)

coef_df <- data.frame(
  Variable    = c("below_minus_20_to_minus_5", names(coefficients)[-1]),
  Coefficient = coefficients,
  Lower_CI    = conf_int[, 1],
  Upper_CI    = conf_int[, 2],
  Regression  = "Régression 1"
)[c(1:8),]

coef_df2 <- data.frame(
  Variable    = c("below_minus_20_to_minus_5", names(coefficients2)[-1]),
  Coefficient = coefficients2,
  Lower_CI    = conf_int2[, 1],
  Upper_CI    = conf_int2[, 2],
  Regression  = "Régression 2"
)[c(1:8),]

combined_coef_df <- rbind(coef_df, coef_df2) %>%
  mutate(Variable = recode(Variable,
                           "above_30"                     = ">30",
                           "below_minus_20_to_minus_5"    = "<-20 to -5",
                           "five_to_10"                   = "5 to 10",
                           "minus_10_to_minus_5"          = "-10 to -5",
                           "minus_5_to_0"                 = "-5 to 0",
                           "twenty_eight_to_30"           = "28 to 30",
                           "twenty_to_25"                 = "20 to 25",
                           "twentyfive_to_28"            = "25 to 28",
                           "zero_to_5"                    = "0 to 5",
                           "ten_to_15"                    = "10 to 15",
                           "twentyfive_to_30"             = "25 to 30"
  ),
  Regression = recode(Regression,
                      "Régression 1" = "1980-2003",
                      "Régression 2" = "2004-2019")
  )

# Add reference rows
combined_coef_df <- rbind(
  combined_coef_df,
  data.frame(Variable="15 to 20 (REF)", Coefficient=0, Lower_CI=0, Upper_CI=0, Regression="1980-2003"),
  data.frame(Variable="15 to 20 (REF)", Coefficient=0, Lower_CI=0, Upper_CI=0, Regression="2004-2019")
)

combined_coef_df <- combined_coef_df[c(1,2,3,4,5,18,6,7,8,9,10,11,12,13,17,14,15,16),]

# Plot (MetBrewer palette)
palette_teal <- met.brewer("Demuth", n = 2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)),
                             y = Coefficient, fill = Regression, color = Regression)) +
  geom_point(size = 3) +
  geom_line(aes(group = Regression), linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2, color = NA) +
  labs(x = "Temperature bins (°C)", y = "Mortality rate per 10,000", color = "Period", fill = "Period") +
  scale_color_manual(values = palette_teal) +
  scale_fill_manual(values = palette_teal) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"), legend.box = "horizontal") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)





########### Urban/Rural ################

base_high_density <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite > 300
)

base_low_density <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite < 300
)


# 2. Fixed‑Effects Regressions


# Total mortality – High Density
summary(feolss1 <- feols(taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                           zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                           humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                           rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                           wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_high_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

# Total mortality – Low Density
summary(feolss2 <- feols(taux_mortalite_total ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                           zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                           humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                           rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                           wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_low_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

# 75+ mortality – High Density
summary(feolss3 <- feols(taux_mortalite_75_plus ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                           zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                           humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                           rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                           wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_high_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

# 75+ mortality – Low Density
summary(feolss4 <- feols(taux_mortalite_75_plus ~ below_minus_20_to_minus_5 + minus_5_to_0 +
                           zero_to_5 + five_to_10 + ten_to_15 + twenty_to_25 + twentyfive_to_30 + above_30 +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part +
                           humidity_bin_plus_80_part + rain_bin_0_3_part + rain_bin_10_100_part +
                           rain_bin_plus_100_part + rain_bin_zero_part + wind_bin_0_3_part +
                           wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_low_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))


# 3. Table of Results 


etable(feolss1, feolss2, feolss3, feolss4, tex = TRUE)


# 4. Coefficient Plots

library(fixest)
library(ggplot2)

# Extract coefficients & CIs – High Density
coefficients <- coef(feolss1)
conf_int     <- confint(feolss1)

coef_df <- data.frame(
  Variable    = c("below_minus_20_to_minus_5", names(coefficients)[-1]),
  Coefficient = coefficients,
  Lower_CI    = conf_int[, 1],
  Upper_CI    = conf_int[, 2],
  Regression  = "Régression 1"
)[c(1:8),]

# Extract coefficients & CIs – Low Density
coefficients2 <- coef(feolss2)
conf_int2     <- confint(feolss2)

coef_df2 <- data.frame(
  Variable    = c("below_minus_20_to_minus_5", names(coefficients2)[-1]),
  Coefficient = coefficients2,
  Lower_CI    = conf_int2[, 1],
  Upper_CI    = conf_int2[, 2],
  Regression  = "Régression 2"
)[c(1:8),]

# Combine
combined_coef_df <- rbind(coef_df, coef_df2) %>%
  mutate(
    Variable = recode(Variable,
                      "above_30"                  = ">30",
                      "below_minus_20_to_minus_5" = "<-20 to -5",
                      "five_to_10"                = "5 to 10",
                      "minus_10_to_minus_5"       = "-10 to -5",
                      "minus_5_to_0"              = "-5 to 0",
                      "twenty_eight_to_30"        = "28 to 30",
                      "twenty_to_25"              = "20 to 25",
                      "twentyfive_to_28"          = "25 to 28",
                      "zero_to_5"                 = "0 to 5",
                      "ten_to_15"                 = "10 to 15",
                      "twentyfive_to_30"          = "25 to 30"
    ),
    Regression = recode(Regression,
                        "Régression 1" = "High Density",
                        "Régression 2" = "Low Density")
  )

# Add reference rows
combined_coef_df <- rbind(
  combined_coef_df,
  data.frame(Variable="15 to 20 (REF)", Coefficient=0, Lower_CI=0, Upper_CI=0, Regression="High Density"),
  data.frame(Variable="15 to 20 (REF)", Coefficient=0, Lower_CI=0, Upper_CI=0, Regression="Low Density")
)

combined_coef_df <- combined_coef_df[c(1,2,3,4,5,18,6,7,8,9,10,11,12,13,17,14,15,16),]

# Basic plot
ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression)) +
  geom_point() +
  geom_line(aes(group = Regression), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2) +
  labs(x = "Temperature bins (°C)", y = "Mortality rate per 10,000") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")

# Enhanced plot with MetBrewer palette
library(MetBrewer)
combined_coef_df$Regression <- factor(combined_coef_df$Regression, levels = c("Low Density", "High Density"))
palette_teal <- met.brewer("Paquin", n = 2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression, color = Regression)) +
  geom_point(size = 3) +
  geom_line(aes(group = Regression), linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2, color = NA) +
  labs(x = "Temperature bins (°C)", y = "Mortality rate per 10,000", color = "Municipality type", fill = "Municipality type") +
  scale_color_manual(values = palette_teal) +
  scale_fill_manual(values = palette_teal) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.title = element_text(size = 12, face = "bold"), legend.box = "horizontal") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)

############## urbain/rural différentes periodes #######

base_high_density <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite > 300 & year < 2004
)

base_low_density <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite < 300 & year < 2004
)

base_high_density_post2003 <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite > 300 & year >= 2004
)

base_low_density_post2003 <- filter(
  communes_dates_1980_2022_temperature_final_mois,
  communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite < 300 & year >= 2004
)


# 2. Fixed‑Effects Regressions (Total Mortality


summary(feolss1 <- feols(taux_mortalite_total ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_low_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

summary(feolss2 <- feols(taux_mortalite_total ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_high_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

summary(feolss3 <- feols(taux_mortalite_total ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_low_density_post2003,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

summary(feolss4 <- feols(taux_mortalite_total ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_high_density_post2003,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

etable(feolss1, feolss3, feolss2, feolss4, tex = TRUE)


# 3. Fixed‑Effects Regressions (75+ Mortality


summary(feolss1 <- feols(taux_mortalite_75_plus ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_low_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

summary(feolss2 <- feols(taux_mortalite_75_plus ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_high_density,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

summary(feolss3 <- feols(taux_mortalite_75_plus ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_low_density_post2003,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

summary(feolss4 <- feols(taux_mortalite_75_plus ~ below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight +
                           humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
                           rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
                           wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part | COM^mois + mois^year + DEP^year,
                         data    = base_high_density_post2003,
                         se      = "cluster", cluster = ~COM,
                         weights = ~value_estimated_population))

etable(feolss1, feolss3, feolss2, feolss4, tex = TRUE)



#### PLOT 

library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("below_minus_20_to_0", names(coefficients)[-1]),  
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2],
  Regression = "Régression 1"
)

coef_df<-coef_df[c(1:4),]






library(fixest)
library(ggplot2)


coefficients2 <- coef(feolss2)
conf_int2 <- confint(feolss2)

coef_df2 <- data.frame(
  Variable = c("below_minus_20_to_0", names(coefficients2)[-1]), 
  Coefficient = coefficients2,
  Lower_CI = conf_int2[, 1],
  Upper_CI = conf_int2[, 2],
  Regression = "Régression 2"
)

coef_df2<-coef_df2[c(1:4),]

combined_coef_df <- rbind(coef_df, coef_df2)


combined_coef_df <- combined_coef_df %>%
  mutate(Variable = recode(Variable,
                           "below_minus_20_to_0" = "<-20 to 0",
                           "zero_to_15" = "0 to 15",
                           "twenty_to_28" = "20 to 28",
                           "above_twenty_eight" = ">28",
                           
  ))



combined_coef_df <- combined_coef_df %>%
  mutate(Regression = recode(Regression,
                             "Régression 1" = "Low Density",
                             "Régression 2" = "High Density"
                             
  ))


nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="Low Density")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="High Density")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)


combined_coef_df<-combined_coef_df[c(1,2,9,3,4,5,6,10,7,8,9),]

library(ggplot2)
library(MetBrewer)

combined_coef_df$Regression <- factor(combined_coef_df$Regression, 
                                      levels = c("Low Density", "High Density"),
                                      labels = c("Low Density", "High Density"))

palette_teal <- met.brewer("Paquin", n = 2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), 
                             y = Coefficient, 
                             fill = Regression, 
                             color = Regression)) +
  geom_point(size = 3) +  
  geom_line(aes(group = Regression), linetype = "dashed", size = 1) + 
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), 
              alpha = 0.2, color = NA) +
  labs(x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000",
       color = "Municipality type", 
       fill = "Municipality type") +
  scale_color_manual(values = palette_teal) + # Palette sombre
  scale_fill_manual(values = palette_teal) +
  theme_minimal(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.box = "horizontal",  
        plot.title = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)




# Urban/Rural & Income Inequality 


base_high_density <- filter(communes_dates_1980_2022_temperature_final_mois,
                            communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite > 300)

base_low_density  <- filter(communes_dates_1980_2022_temperature_final_mois,
                            communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite < 300)


# 1.1 Filter period 2001‑2019
data_filtered <- base_low_density[
  base_low_density$year >= 2001 & base_low_density$year <= 2019, ]

# 1.2 Compute log(median income) quantiles
log_mediane <- log(data_filtered$mediane)

top_10    <- quantile(log_mediane, 0.85, na.rm = TRUE)   # intended top ~15%
bottom_10 <- quantile(log_mediane, 0.15, na.rm = TRUE)    # intended bottom ~15%


# 2. FE Regression with Interaction -------------------------------------------


feolss1 <- feols(
  taux_mortalite_total ~ (below_minus_20_to_0 + zero_to_15 + twenty_to_28 + above_twenty_eight) * log(mediane) +
    humidity_bin_moins_20_part + humidity_bin_40_60_part + humidity_bin_60_80_part + humidity_bin_plus_80_part +
    rain_bin_0_3_part + rain_bin_10_100_part + rain_bin_plus_100_part + rain_bin_zero_part +
    wind_bin_0_3_part + wind_bin_10_20_part + wind_bin_plus_20_part |
    COM^mois + mois^year + DEP^year,
  data     = base_low_density,
  se       = "cluster", cluster = ~COM,
  weights  = ~value_estimated_population
)


# 3. Extract Coefficients & Variance‑Covariance

vce <- vcov(feolss1)


coef_below_20_0 <- coef(feolss1)["below_minus_20_to_0"]
coef_below_20_0_interaction <- coef(feolss1)["below_minus_20_to_0:log(mediane)"]
se_below_20_0 <- sqrt(vce["below_minus_20_to_0", "below_minus_20_to_0"])
se_below_20_0_interaction <- sqrt(vce["below_minus_20_to_0:log(mediane)", "below_minus_20_to_0:log(mediane)"])
cov_below_20_0_interaction <- vce["below_minus_20_to_0", "below_minus_20_to_0:log(mediane)"]

coef_zero_15 <- coef(feolss1)["zero_to_15"]
coef_zero_15_interaction <- coef(feolss1)["zero_to_15:log(mediane)"]
se_zero_15 <- sqrt(vce["zero_to_15", "zero_to_15"])
se_zero_15_interaction <- sqrt(vce["zero_to_15:log(mediane)", "zero_to_15:log(mediane)"])
cov_zero_15_interaction <- vce["zero_to_15", "zero_to_15:log(mediane)"]

coef_twenty_28 <- coef(feolss1)["twenty_to_28"]
coef_twenty_28_interaction <- coef(feolss1)["twenty_to_28:log(mediane)"]
se_twenty_28 <- sqrt(vce["twenty_to_28", "twenty_to_28"])
se_twenty_28_interaction <- sqrt(vce["twenty_to_28:log(mediane)", "twenty_to_28:log(mediane)"])
cov_twenty_28_interaction <- vce["twenty_to_28", "twenty_to_28:log(mediane)"]

coef_above_28 <- coef(feolss1)["above_twenty_eight"]
coef_above_28_interaction <- coef(feolss1)["above_twenty_eight:log(mediane)"]
se_above_28 <- sqrt(vce["above_twenty_eight", "above_twenty_eight"])
se_above_28_interaction <- sqrt(vce["above_twenty_eight:log(mediane)", "above_twenty_eight:log(mediane)"])
cov_above_28_interaction <- vce["above_twenty_eight", "above_twenty_eight:log(mediane)"]

# 5. Calcul des effets marginaux pour top 15% et bottom 15%
effect_top_below_20_0 <- coef_below_20_0 + coef_below_20_0_interaction * top_10
effect_bottom_below_20_0 <- coef_below_20_0 + coef_below_20_0_interaction * bottom_10

effect_top_zero_15 <- coef_zero_15 + coef_zero_15_interaction * top_10
effect_bottom_zero_15 <- coef_zero_15 + coef_zero_15_interaction * bottom_10

effect_top_twenty_28 <- coef_twenty_28 + coef_twenty_28_interaction * top_10
effect_bottom_twenty_28 <- coef_twenty_28 + coef_twenty_28_interaction * bottom_10

effect_top_above_28 <- coef_above_28 + coef_above_28_interaction * top_10
effect_bottom_above_28 <- coef_above_28 + coef_above_28_interaction * bottom_10

# 6. Calcul des erreurs standards pour les effets marginaux
se_top_below_20_0 <- sqrt(se_below_20_0^2 + top_10^2 * se_below_20_0_interaction^2 + 2 * top_10 * cov_below_20_0_interaction)
se_bottom_below_20_0 <- sqrt(se_below_20_0^2 + bottom_10^2 * se_below_20_0_interaction^2 + 2 * bottom_10 * cov_below_20_0_interaction)

se_top_zero_15 <- sqrt(se_zero_15^2 + top_10^2 * se_zero_15_interaction^2 + 2 * top_10 * cov_zero_15_interaction)
se_bottom_zero_15 <- sqrt(se_zero_15^2 + bottom_10^2 * se_zero_15_interaction^2 + 2 * bottom_10 * cov_zero_15_interaction)

se_top_twenty_28 <- sqrt(se_twenty_28^2 + top_10^2 * se_twenty_28_interaction^2 + 2 * top_10 * cov_twenty_28_interaction)
se_bottom_twenty_28 <- sqrt(se_twenty_28^2 + bottom_10^2 * se_twenty_28_interaction^2 + 2 * bottom_10 * cov_twenty_28_interaction)

se_top_above_28 <- sqrt(se_above_28^2 + top_10^2 * se_above_28_interaction^2 + 2 * top_10 * cov_above_28_interaction)
se_bottom_above_28 <- sqrt(se_above_28^2 + bottom_10^2 * se_above_28_interaction^2 + 2 * bottom_10 * cov_above_28_interaction)

# 7. Organiser les résultats dans un tableau
results <- data.frame(
  Temperature = rep(c("Below -20 to 0", "0 to 15", "20 to 28", "Above 28"), each = 2),
  Revenu = rep(c("Bas revenu (15%)", "Haut revenu (15%)"), 4),
  Effet = c(effect_bottom_below_20_0, effect_top_below_20_0,
            effect_bottom_zero_15, effect_top_zero_15,
            effect_bottom_twenty_28, effect_top_twenty_28,
            effect_bottom_above_28, effect_top_above_28),
  SE = c(se_bottom_below_20_0, se_top_below_20_0,
         se_bottom_zero_15, se_top_zero_15,
         se_bottom_twenty_28, se_top_twenty_28,
         se_bottom_above_28, se_top_above_28)
)

results <- data.frame(
  Temperature = rep(c("Below -20 to 0", "0 to 15", "15 to 20", "20 to 28", "Above 28"), each = 2),
  Revenu = rep(c("Bas revenu (15%)", "Haut revenu (15%)"), 5),
  Effet = c(effect_bottom_below_20_0, effect_top_below_20_0,
            effect_bottom_zero_15, effect_top_zero_15,
            0, 0,  # Effet de 15 à 20 pour bas et haut revenu
            effect_bottom_twenty_28, effect_top_twenty_28,
            effect_bottom_above_28, effect_top_above_28),
  SE = c(se_bottom_below_20_0, se_top_below_20_0,
         se_bottom_zero_15, se_top_zero_15,
         0, 0,  # SE de 15 à 20 pour bas et haut revenu
         se_bottom_twenty_28, se_top_twenty_28,
         se_bottom_above_28, se_top_above_28)
)


results$Lower_CI <- results$Effet - 1.96 * results$SE
results$Upper_CI <- results$Effet + 1.96 * results$SE

results_sorted <- results %>%
  arrange(Revenu, factor(Temperature, levels = c("below_minus_20_to_0", "zero_to_15", "fifteen_to_20", "twenty_to_28", "above_twenty_eight")))



library(ggplot2)
library(MetBrewer)

# 
results_sorted$Municipality_Type <- factor(results_sorted$Revenu,
                                           levels = c("Bas revenu (15%)", "Haut revenu (15%)"),
                                           labels = c("Low income & low density", "High income & low density"))

# 
palette_tam <- met.brewer("Austria", n = 2)

#
ggplot(results_sorted, aes(x = factor(Temperature, levels = c("Below -20 to 0", "0 to 15", "15 to 20", "20 to 28", "Above 28")), 
                           y = Effet, 
                           color = Municipality_Type, 
                           fill = Municipality_Type)) +
  geom_point(size = 3) +  # Points plus grands
  geom_line(aes(group = Municipality_Type), linetype = "dashed", size = 1) + # Ligne plus épaisse
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Municipality_Type), 
              alpha = 0.2, color = NA) +
  labs(x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000",
       color = "Municipality type", 
       fill = "Municipality type") +
  scale_color_manual(values = palette_tam) + # Palette sombre
  scale_fill_manual(values = palette_tam) +
  theme_minimal(base_size = 14) + # Taille de base augmentée
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Légende en bas
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.box = "horizontal",  # Orientation horizontale
        plot.title = element_blank()) + # Pas de titre
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)




##### urban



data_filtered <- base_high_density[
  base_high_density$year >= 2001 &
    base_high_density$year <= 2019, ]

log_mediane <- log(data_filtered$mediane)
top_10 <- quantile(log_mediane, 0.85, na.rm = TRUE)
bottom_10 <- quantile(log_mediane, 0.15, na.rm = TRUE)

# 3. Estimation du modèle avec feols
feolss1 <- feols(taux_mortalite_total~ (below_minus_20_to_0+zero_to_15+twenty_to_28+above_twenty_eight)*log(mediane)
                 +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                 +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                 +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year+DEP^year,
                 data = base_high_density,se = "cluster", cluster = ~COM,weights=~value_estimated_population)

vce <- vcov(feolss1)

# Coefficients et erreurs standards pour chaque plage de température
coef_below_20_0 <- coef(feolss1)["below_minus_20_to_0"]
coef_below_20_0_interaction <- coef(feolss1)["below_minus_20_to_0:log(mediane)"]
se_below_20_0 <- sqrt(vce["below_minus_20_to_0", "below_minus_20_to_0"])
se_below_20_0_interaction <- sqrt(vce["below_minus_20_to_0:log(mediane)", "below_minus_20_to_0:log(mediane)"])
cov_below_20_0_interaction <- vce["below_minus_20_to_0", "below_minus_20_to_0:log(mediane)"]

coef_zero_15 <- coef(feolss1)["zero_to_15"]
coef_zero_15_interaction <- coef(feolss1)["zero_to_15:log(mediane)"]
se_zero_15 <- sqrt(vce["zero_to_15", "zero_to_15"])
se_zero_15_interaction <- sqrt(vce["zero_to_15:log(mediane)", "zero_to_15:log(mediane)"])
cov_zero_15_interaction <- vce["zero_to_15", "zero_to_15:log(mediane)"]

coef_twenty_28 <- coef(feolss1)["twenty_to_28"]
coef_twenty_28_interaction <- coef(feolss1)["twenty_to_28:log(mediane)"]
se_twenty_28 <- sqrt(vce["twenty_to_28", "twenty_to_28"])
se_twenty_28_interaction <- sqrt(vce["twenty_to_28:log(mediane)", "twenty_to_28:log(mediane)"])
cov_twenty_28_interaction <- vce["twenty_to_28", "twenty_to_28:log(mediane)"]

coef_above_28 <- coef(feolss1)["above_twenty_eight"]
coef_above_28_interaction <- coef(feolss1)["above_twenty_eight:log(mediane)"]
se_above_28 <- sqrt(vce["above_twenty_eight", "above_twenty_eight"])
se_above_28_interaction <- sqrt(vce["above_twenty_eight:log(mediane)", "above_twenty_eight:log(mediane)"])
cov_above_28_interaction <- vce["above_twenty_eight", "above_twenty_eight:log(mediane)"]

# 5. Calcul des effets marginaux pour top 10% et bottom 10%
effect_top_below_20_0 <- coef_below_20_0 + coef_below_20_0_interaction * top_10
effect_bottom_below_20_0 <- coef_below_20_0 + coef_below_20_0_interaction * bottom_10

effect_top_zero_15 <- coef_zero_15 + coef_zero_15_interaction * top_10
effect_bottom_zero_15 <- coef_zero_15 + coef_zero_15_interaction * bottom_10

effect_top_twenty_28 <- coef_twenty_28 + coef_twenty_28_interaction * top_10
effect_bottom_twenty_28 <- coef_twenty_28 + coef_twenty_28_interaction * bottom_10

effect_top_above_28 <- coef_above_28 + coef_above_28_interaction * top_10
effect_bottom_above_28 <- coef_above_28 + coef_above_28_interaction * bottom_10

# 6. Calcul des erreurs standards pour les effets marginaux
se_top_below_20_0 <- sqrt(se_below_20_0^2 + top_10^2 * se_below_20_0_interaction^2 + 2 * top_10 * cov_below_20_0_interaction)
se_bottom_below_20_0 <- sqrt(se_below_20_0^2 + bottom_10^2 * se_below_20_0_interaction^2 + 2 * bottom_10 * cov_below_20_0_interaction)

se_top_zero_15 <- sqrt(se_zero_15^2 + top_10^2 * se_zero_15_interaction^2 + 2 * top_10 * cov_zero_15_interaction)
se_bottom_zero_15 <- sqrt(se_zero_15^2 + bottom_10^2 * se_zero_15_interaction^2 + 2 * bottom_10 * cov_zero_15_interaction)

se_top_twenty_28 <- sqrt(se_twenty_28^2 + top_10^2 * se_twenty_28_interaction^2 + 2 * top_10 * cov_twenty_28_interaction)
se_bottom_twenty_28 <- sqrt(se_twenty_28^2 + bottom_10^2 * se_twenty_28_interaction^2 + 2 * bottom_10 * cov_twenty_28_interaction)

se_top_above_28 <- sqrt(se_above_28^2 + top_10^2 * se_above_28_interaction^2 + 2 * top_10 * cov_above_28_interaction)
se_bottom_above_28 <- sqrt(se_above_28^2 + bottom_10^2 * se_above_28_interaction^2 + 2 * bottom_10 * cov_above_28_interaction)

# 7. Organiser les résultats dans un tableau
results <- data.frame(
  Temperature = rep(c("Below -20 to 0", "0 to 15", "20 to 28", "Above 28"), each = 2),
  Revenu = rep(c("Bas revenu (20%)", "Haut revenu (20%)"), 4),
  Effet = c(effect_bottom_below_20_0, effect_top_below_20_0,
            effect_bottom_zero_15, effect_top_zero_15,
            effect_bottom_twenty_28, effect_top_twenty_28,
            effect_bottom_above_28, effect_top_above_28),
  SE = c(se_bottom_below_20_0, se_top_below_20_0,
         se_bottom_zero_15, se_top_zero_15,
         se_bottom_twenty_28, se_top_twenty_28,
         se_bottom_above_28, se_top_above_28)
)

results <- data.frame(
  Temperature = rep(c("Below -20 to 0", "0 to 15", "15 to 20", "20 to 28", "Above 28"), each = 2),
  Revenu = rep(c("Bas revenu (20%)", "Haut revenu (20%)"), 5),
  Effet = c(effect_bottom_below_20_0, effect_top_below_20_0,
            effect_bottom_zero_15, effect_top_zero_15,
            0, 0,  # Effet de 15 à 20 pour bas et haut revenu
            effect_bottom_twenty_28, effect_top_twenty_28,
            effect_bottom_above_28, effect_top_above_28),
  SE = c(se_bottom_below_20_0, se_top_below_20_0,
         se_bottom_zero_15, se_top_zero_15,
         0, 0,  # SE de 15 à 20 pour bas et haut revenu
         se_bottom_twenty_28, se_top_twenty_28,
         se_bottom_above_28, se_top_above_28)
)


results$Lower_CI <- results$Effet - 1.96 * results$SE
results$Upper_CI <- results$Effet + 1.96 * results$SE

results_sorted2 <- results %>%
  arrange(Revenu, factor(Temperature, levels = c("below_minus_20_to_0", "zero_to_15", "fifteen_to_20", "twenty_to_28", "above_twenty_eight")))



library(ggplot2)
library(MetBrewer)

# Mise à jour explicite des groupes
results_sorted2$Municipality_Type <- factor(results_sorted2$Revenu,
                                            levels = c("Bas revenu (20%)", "Haut revenu (20%)"),
                                            labels = c("Low income & high density", "High income & high density"))

# Palette sombre et contrastée : Tam
palette_tam <- met.brewer("Austria", n = 2)

# Graphique harmonisé avec légende corrigée
ggplot(results_sorted2, aes(x = factor(Temperature, levels = c("Below -20 to 0", "0 to 15", "15 to 20", "20 to 28", "Above 28")), 
                            y = Effet, 
                            color = Municipality_Type, 
                            fill = Municipality_Type)) +
  geom_point(size = 3) +  # Points plus grands
  geom_line(aes(group = Municipality_Type), linetype = "dashed", size = 1) + # Ligne plus épaisse
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Municipality_Type), 
              alpha = 0.2, color = NA) +
  labs(x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000",
       color = "Municipality type", 
       fill = "Municipality type") +
  scale_color_manual(values = palette_tam) + # Palette sombre
  scale_fill_manual(values = palette_tam) +
  theme_minimal(base_size = 14) + # Taille de base augmentée
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Légende en bas
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.box = "horizontal",  # Orientation horizontale
        plot.title = element_blank()) + # Pas de titre
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)



### double graph 



results_sorted$Revenu <- paste0(results_sorted$Revenu, "_low_density")
results_sorted2$Revenu <- paste0(results_sorted2$Revenu, "_high_density")
resultats<-rbind(results_sorted,results_sorted2)



library(ggplot2)

ggplot(resultats, aes(x = factor(Temperature, levels = unique(Temperature)), y = Effet, fill = Revenu)) +
  geom_point() +
  geom_line(aes(group = Revenu), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Revenu), alpha = 0.2) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")



# 2 
library(ggplot2)
library(viridis) 

palette_tam <- viridis::viridis(4) 

ggplot(resultats, aes(x = factor(Temperature, levels = unique(Temperature)), 
                      y = Effet, 
                      color = as.factor(Municipality_Type), 
                      fill = as.factor(Municipality_Type))) +

  geom_point(size = 2.5) +
 
  geom_line(aes(group = Municipality_Type), linetype = "solid", size = 1) +
 
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
 
  labs(
    x = "Temperature bins (°C)",
    y = "Mortality rate per 10,000",
    color = "",
    fill = ""
  ) +
 
  scale_color_manual(values = palette_tam) +
  scale_fill_manual(values = palette_tam) +
 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.box = "horizontal",
    plot.title = element_blank()
  )
#3
library(ggplot2)
library(dplyr)
library(MetBrewer) 


resultatsbis <- filter(resultats, 
                       Revenu == "Bas revenu (20%)_high_density" | 
                         Revenu == "Haut revenu (20%)_low_density")


resultatsbis$Municipality_Type <- factor(resultatsbis$Revenu,
                                         levels = c("Bas revenu (20%)_high_density", 
                                                    "Haut revenu (20%)_low_density"),
                                         labels = c("Low income & high density", 
                                                    "High income & low density"))


palette_vangogh <- met.brewer("Austria", n = 2)


ggplot(resultatsbis, aes(x = factor(Temperature, levels = unique(Temperature)), 
                         y = Effet, 
                         color = Municipality_Type, 
                         fill = Municipality_Type)) +
  geom_point(size = 3) +  # Points plus grands
  geom_line(aes(group = Municipality_Type), linetype = "dashed", size = 1) + # Ligne plus épaisse
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Municipality_Type), 
              alpha = 0.2, color = NA) +
  labs(x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000",
       color = "Municipality type", 
       fill = "Municipality type") +
  scale_color_manual(values = palette_vangogh) + # Couleurs personnalisées avec VanGogh1
  scale_fill_manual(values = palette_vangogh) +
  theme_minimal(base_size = 14) + # Taille de base augmentée
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Légende en bas
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.box = "horizontal",  # Orientation horizontale
        plot.title = element_blank()) + # Pas de titre
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)











############# ROBUSTNESS LAG ############




library(dplyr)


data <- arrange(communes_dates_1980_2022_temperature_final_mois, year, mois, COM)


data <- data %>% 
  group_by(COM) %>% 
  mutate(minus_5_to_0_lag = dplyr::lag(minus_5_to_0, n = 1),
         below_minus_20_to_minus_5_lag = dplyr::lag(below_minus_20_to_minus_5, n = 1),
         twentyfive_to_30_lag = dplyr::lag(twentyfive_to_30, n = 1),
         minus_10_to_minus_5_lag = dplyr::lag(minus_10_to_minus_5, n = 1),
         minus_15_to_minus_10_lag = dplyr::lag(minus_15_to_minus_10, n = 1),
         minus_20_to_minus_15_lag = dplyr::lag(minus_20_to_minus_15, n = 1),
         zero_to_5_lag = dplyr::lag(zero_to_5, n = 1),
         five_to_10_lag = dplyr::lag(five_to_10, n = 1),
         ten_to_15_lag = dplyr::lag(ten_to_15, n = 1),
         twenty_to_25_lag = dplyr::lag(twenty_to_25, n = 1),
         twentyfive_to_28_lag = dplyr::lag(twentyfive_to_28, n = 1),
         twenty_eight_to_30_lag = dplyr::lag(twenty_eight_to_30, n = 1),
         above_30_lag = dplyr::lag(above_30, n = 1),
         below_minus_20_lag = dplyr::lag(below_minus_20, n = 1),
         
         
         
         humidity_bin_moins_20_part_lag = dplyr::lag(humidity_bin_moins_20_part, n = 1),
         humidity_bin_20_40_part_lag = dplyr::lag(humidity_bin_20_40_part, n = 1),
         humidity_bin_40_60_part_lag = dplyr::lag(humidity_bin_40_60_part, n = 1),
         humidity_bin_60_80_part_lag = dplyr::lag(humidity_bin_60_80_part, n = 1),
         humidity_bin_plus_80_part_lag = dplyr::lag(humidity_bin_plus_80_part, n = 1),
         rain_bin_0_3_part_lag = dplyr::lag(rain_bin_0_3_part, n = 1),
         rain_bin_3_10_part_lag = dplyr::lag(rain_bin_3_10_part, n = 1),
         rain_bin_10_100_part_lag = dplyr::lag(rain_bin_10_100_part, n = 1),
         rain_bin_plus_100_part_lag = dplyr::lag(rain_bin_plus_100_part, n = 1),
         rain_bin_zero_part_lag = dplyr::lag(rain_bin_zero_part, n = 1),
         wind_bin_0_3_part_lag = dplyr::lag(wind_bin_0_3_part, n = 1),
         wind_bin_3_10_part_lag = dplyr::lag(wind_bin_3_10_part, n = 1),
         wind_bin_10_20_part_lag = dplyr::lag(wind_bin_10_20_part, n = 1),
         wind_bin_plus_20_part_lag = dplyr::lag(wind_bin_plus_20_part, n = 1)) %>% 
  ungroup()







summary(feolss1 <-  feols(taux_mortalite_total~ below_minus_20_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_30+above_30+
                            below_minus_20_to_minus_5_lag+minus_10_to_minus_5_lag+minus_5_to_0_lag+zero_to_5_lag+five_to_10_lag+ten_to_15_lag+twenty_to_25_lag+twentyfive_to_30_lag+above_30_lag
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois + mois^year + DEP^year
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





#
summary(feolss2 <-  feols(taux_mortalite_75_plus~ below_minus_20_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_30+above_30+
                            below_minus_20_to_minus_5_lag+minus_10_to_minus_5_lag+minus_5_to_0_lag+zero_to_5_lag+five_to_10_lag+ten_to_15_lag+twenty_to_25_lag+twentyfive_to_30_lag+above_30_lag
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois + mois^year + DEP^year
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

etable(feolss1,feolss2, tex = TRUE)





#################### temperature maximum #####################



communes_dates_1980_2022_temperature_final_mois$temp_max_bin_inférieur_moins_10_to_0_tempmax_part<-communes_dates_1980_2022_temperature_final_mois$temp_max_bin_inférieur_moins_10_tempmax_part+ communes_dates_1980_2022_temperature_final_mois$temp_max_bin_moins10_0_tempmax_part




summary(feolss1 <-  feols(taux_mortalite_total~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois + mois^year + DEP^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






summary(feolss2 <-  feols(taux_mortalite_75_plus~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois + mois^year + DEP^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

etable(feolss1, feolss2, tex = TRUE)




library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("temp_max_bin_inférieur_moins_10_to_0_tempmax_part", names(coefficients)[-1]),  
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2]
)

coef_df<-coef_df[c(1:7),]

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0)
coef_df<-rbind(coef_df,nouvelle_ligne)


coef_df<-coef_df[c(1,2,3,8,4,5,6,7),]

coef_df <- coef_df %>%
  mutate(Variable = recode(Variable,
                           "temp_max_bin_inférieur_moins_10_to_0_tempmax_part" = "<-10 to 0",
                           "temp_max_bin_0_10_tempmax_part" = "0 to 10",
                           "temp_max_bin_10_15_tempmax_part" = "10 to 15",
                           "temp_max_bin_20_25_tempmax_part" = "20 to 25",
                           "temp_max_bin_25_30_tempmax_part" = "25 to 30",
                           "temp_max_bin_30_35_tempmax_part" = "30 to 35",
                           "temp_max_bin_plus_35_part" = ">35"
                           
  ))

library(ggplot2)


library(ggplot2)


ggplot(coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient)) +
  geom_point() +  # Points plus gros et de couleur bleue
  geom_line(aes(group = 1), linetype = "dashed") +  # Ligne reliant les points (en pointillés)
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, group = 1) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









####### ROBUSTNESS #####










base_high_density<-filter(communes_dates_1980_2022_temperature_final_mois, communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite>300 )
base_low_density<-filter(communes_dates_1980_2022_temperature_final_mois, communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite<300 )


feolss1 <- feols(taux_mortalite_total~ (below_minus_20_to_0+zero_to_15+twenty_to_28+above_twenty_eight)*log(mediane)
                 +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                 +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                 +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+mois^year+DEP^year,
                 data = base_low_density,se = "cluster", cluster = ~COM,weights=~value_estimated_population)
feolss2 <- feols(taux_mortalite_total~ (below_minus_20_to_0+zero_to_15+twenty_to_28+above_twenty_eight)*log(mediane)
                 +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                 +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                 +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+mois^year+DEP^year,
                 data = base_high_density,se = "cluster", cluster = ~COM,weights=~value_estimated_population)




etable(feolss1, feolss2, tex = T)







