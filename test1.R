# H1 testing

library(vdemdata)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data
data("vdem")  

df <- vdem %>%
  filter(country_name == "Georgia", year >= 2010, year <= 2024) %>%
  select(year,
         v2x_horacc,
         v2x_veracc,                             
         v2x_diagacc)

# Calculations
df <- df %>%
  arrange(year) %>%
  mutate(
    delta_hor  = v2x_horacc - lag(v2x_horacc),
    delta_vert = v2x_veracc - lag(v2x_veracc),
    delta_diag = v2x_diagacc - lag(v2x_diagacc)
  )


df <- df %>%
  mutate(
    drift_hor  = -delta_hor,
    drift_vert = -delta_vert,
    drift_diag = -delta_diag
 )


df <- df %>%
  mutate(inst_drift = rowMeans(select(., drift_hor, drift_diag, drift_vert), na.rm = TRUE))

# write.csv(df,"df1.csv")

ggplot(df, aes(x = year)) +
  geom_line(aes(y = inst_drift), color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Institutional Drift in Georgia (2010–2024)",
       y = "Drift Index",
       x = "Year")+
  theme_minimal(base_size = 12)


drift_data <- data.frame(
  year = 2010:2024,
  drift_hor = df$drift_hor,
  drift_vert = df$drift_vert,
  drift_diag = df$drift_diag
)

drift_long <- pivot_longer(drift_data, cols = starts_with("drift"), names_to = "direction", values_to = "value")

ggplot(drift_long, aes(x = year, y = value, color = direction)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Institutional Drift in Georgia (2010–2024) by Type",
    x = "Year",
    y = "Drift by type",
    color = "Type"
  ) +
  theme_minimal(base_size = 12)
          