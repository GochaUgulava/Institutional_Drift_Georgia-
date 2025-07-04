# H2 testing

library(vdemdata)
library(tidyr)
library(dplyr)
library(ggplot2)
library(nlme)
library(lmtest)

# Data
data("vdem")  

# institutional_drift
df_i <- vdem %>%
  filter(country_name == "Georgia", year >= 2010, year <= 2024) %>%
  select(year,
         v2x_horacc,
         v2x_veracc,                             
         v2x_diagacc)
 
df_i <- df_i %>%
  arrange(year) %>%
  mutate(
    delta_hor  = v2x_horacc - lag(v2x_horacc),
    delta_vert = v2x_veracc - lag(v2x_veracc),
    delta_diag = v2x_diagacc - lag(v2x_diagacc)
  )

df_i <- df_i %>%
  mutate(
    drift_hor  = -delta_hor,
    drift_vert = -delta_vert,
    drift_diag = -delta_diag
  )

df_i <- df_i %>%
  mutate(institutional_drift = rowMeans(select(., drift_hor, drift_diag, drift_vert), na.rm = TRUE))


# judiciary_capture

df_j <- vdem %>%
  filter(country_name == "Georgia", year >= 2010, year <= 2024) %>%
  select(year,
         v2juncind,
         v2jureview,
         v2jucorrdc)

df_j <- df_j %>%
  mutate(
    inv_v2juncind = 1 - v2juncind,
    inv_v2jureview = 1 - v2jureview
  ) %>%
  mutate(
    judiciary_capture = rowMeans(select(., 
                                        inv_v2juncind, 
                                        inv_v2jureview, 
                                        v2jucorrdc
    ), na.rm = TRUE)
  )

# parliamentary_weakness

df_p <- vdem %>%
  filter(country_name == "Georgia", year >= 2010, year <= 2024) %>%
  select(year,
         v2xlg_legcon,
         v2lgoppart,
         v2lgotovst)

df_p <- df_p %>%
  mutate(
    inv_v2xlg_legcon = 1 - v2xlg_legcon,
    inv_v2lgoppart = 1 - v2lgoppart,
    inv_v2lgotovst = 1 - v2lgotovst
  ) %>%
  mutate(
    parliamentary_weakness = rowMeans(select(., 
                                             inv_v2xlg_legcon, 
                                             inv_v2lgoppart, 
                                             inv_v2lgotovst
    ), na.rm = TRUE)
  )

# Pivot df
df <- bind_cols(
  year = df_j$year, 
  judiciary_capture = df_j$judiciary_capture,
  parliamentary_weakness = df_p$parliamentary_weakness,
  institutional_drift = df_i$institutional_drift
)
df <- df %>% filter(year != 2010)

# Regression Model 1
model <- lm(institutional_drift ~ judiciary_capture + parliamentary_weakness, data = df)
summary(model)

# Regression Model 2
df <- df %>%
  arrange(year) %>%
  mutate(
    judiciary_capture_lag = lag(judiciary_capture),
    parliamentary_weakness_lag = lag(parliamentary_weakness)
  )
model_lag <- lm(institutional_drift ~ judiciary_capture_lag + parliamentary_weakness_lag, data = df)
summary(model_lag)
dwtest(model_lag)

# GLS model with AR(1) error correction 3
df <- df %>% filter(year != 2011)
model_gls_ar1 <- gls(
  institutional_drift ~ judiciary_capture_lag + parliamentary_weakness_lag,
  data = df,
  correlation = corAR1(form = ~ year)
)
summary(model_gls_ar1)

# GLS model with AR(1) error correction 4 (final)
model_mix <- gls(
  institutional_drift ~ judiciary_capture_lag + parliamentary_weakness,
  data = df,
  correlation = corAR1(form = ~ year)
)
summary(model_mix)

# Visualization
ggplot(df, aes(x = year)) +
  geom_line(aes(y = institutional_drift), color = "red") +
  geom_line(aes(y = judiciary_capture), color = "blue", linetype = "dashed") +
  geom_line(aes(y = parliamentary_weakness), color = "darkgreen", linetype = "dashed") +
  labs(title = "Mechanisms of Institutional Drift",
       y = "Index", x = "Year") +
  theme_minimal(base_size = 12) + 
  geom_label(
      label="institutional drift", 
      x=2020,
      y=0.12,
      color = "black",
      fill="white"
  ) +
  geom_label(
    label="judiciary capture", 
    x=2017,
    y=0.8,
    color = "black",
    fill="white"
  ) +
  geom_label(
    label="parliamentary weakness", 
    x=2022,
    y=-0.25,
    color = "black",
    fill="white"
  )

# write.csv(df,"df2.csv")
