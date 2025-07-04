# H3 testing

library(vdemdata)
library(tidyr)
library(dplyr)
library(ggplot2)



# Data 

data(vdem)

# Institutional drift
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


#  Informal Institutions

informal_vars <- c(
  "v2xnp_client",            
  "v2x_corr",    
  "v2x_mpi",    
  "v2clrspct",   
  "v2smdefabu", 
  "v2elintim",   
  "v2elembaut",   
  "v2elfrfair", 
  "v2mecenefm"  
)

df_informal <- vdem %>%
  filter(country_name == "Georgia", year >= 2010, year <= 2024) %>%
  select(year, all_of(informal_vars))

df_informal <- df_informal %>%
  mutate(
    v2x_mpi_inv            = 1 - v2x_mpi,          
    v2clrspct_inv          = 1 - v2clrspct / 4,
    v2smdefabu_inv         = 1 - v2smdefabu / 4,
    v2elintim_inv          = 1- v2elintim /4,
    v2elembaut_inv         = 1 - v2elembaut / 4, 
    v2elfrfair_inv         = 1 - v2elfrfair / 4,
    v2mecenefm_inv         = 1-v2mecenefm /4
  ) 

# All

df_all <- df_informal %>%
  left_join(df_i %>% select(year, institutional_drift), by = "year")


# Correlation

informal_vars <- c(
  "v2xnp_client",       
  "v2x_corr",           
  "v2x_mpi_inv",        
  "v2clrspct_inv",      
  "v2smdefabu_inv",     
  "v2elintim_inv",      
  "v2elembaut_inv",     
  "v2elfrfair_inv",     
  "v2mecenefm_inv"      
)

cor_values <- sapply(informal_vars, function(var) {
  cor(df_all[[var]], df_all$institutional_drift, use = "complete.obs", method = "pearson")
})

df_cor <- data.frame(
  indicator = informal_vars,
  correlation = cor_values
)

ggplot(df_cor, aes(x = indicator, y = "drift_corr", fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Indicators of Informal Institutions",
       title = "Correlation with Institutional Drift") +
  coord_fixed(ratio = 0.5)





