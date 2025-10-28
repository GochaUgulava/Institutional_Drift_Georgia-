# H1  robustness checks 

library(vdemdata)
library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)   # for rolling means

#Load and prepare data
data("vdem")

df <- vdem %>%
  filter(country_name == "Georgia", year >= 2010, year <= 2024) %>%
  select(year, v2x_horacc, v2x_veracc, v2x_diagacc) %>%
  arrange(year) %>%
  mutate(
    delta_hor  = v2x_horacc - lag(v2x_horacc),
    delta_vert = v2x_veracc - lag(v2x_veracc),
    delta_diag = v2x_diagacc - lag(v2x_diagacc),
    drift_hor  = -delta_hor,
    drift_vert = -delta_vert,
    drift_diag = -delta_diag
  )

#Institutional Drift Index (unweighted mean)
df <- df %>%
  mutate(IDI_mean = rowMeans(select(., drift_hor, drift_vert, drift_diag), na.rm = TRUE))

# 1. Smoothed averages (3-year moving average)
df <- df %>%
  mutate(IDI_3yr = rollmean(IDI_mean, k = 3, fill = NA, align = "center"))

# 2. Median-based aggregation
df <- df %>%
  mutate(IDI_median = apply(select(., drift_hor, drift_vert, drift_diag), 1, median, na.rm = TRUE))

# 3 Variance-weighted aggregation Compute weights inversely proportional to variance
vars <- df %>%
  summarise(across(c(drift_hor, drift_vert, drift_diag), var, na.rm = TRUE))
weights <- as.numeric(1 / vars / sum(1 / vars))

# 4 Weighted index
df <- df %>%
  mutate(IDI_weighted = drift_hor * weights[1] +
           drift_vert * weights[2] +
           drift_diag * weights[3])

# Combine and compare all versions 
IDI_long <- df %>%
  select(year, IDI_mean, IDI_3yr, IDI_median, IDI_weighted) %>%
  pivot_longer(cols = -year, names_to = "method", values_to = "value")

# Plot 
ggplot(IDI_long, aes(x = year, y = value, color = method)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(
    title = "Institutional Drift in Georgia (2010–2024): Robustness Checks",
    subtitle = "Comparison of Mean, Smoothed, Median, and Variance-Weighted Indices",
    x = "Year",
    y = "Institutional Drift Index",
    color = "Specification"
  ) +
  theme_minimal(base_size = 13)

# save 
# write.csv(df, "institutional_drift_robustness.csv", row.names = FALSE)
