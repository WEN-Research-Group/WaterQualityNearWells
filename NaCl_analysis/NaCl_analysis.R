#Match Na and Cl measurements by location and date, 
#then compute the ratio and plot against distance
#to the nearest well

library(ggpubr)
library(ggplot2)
library(dplyr)
library(lubridate)

# ── 1. Create dataset ─────────────────────────────────────────────────────────
source("Na_Cl_data_comb_fun.R")

#Function inputs
#1) State
#2) Well type
#3) Water type

S_t = "NY"
well_t = "marginal"
water_t = "SW"

#Creating dataset
Cl_Na = Na_Cl_data_comb_fun(S_t, well_t, water_t)

# ── 2. Plot  ─────────────────────────────────────────────────────────

# Cl concentration as color 
ggplot(Cl_Na %>% filter(!is.na(nearest_distance_m),
                        nearest_distance_m < 3000),
       aes(x = nearest_distance_m, y = na_cl_molar,
           colour = cl_conc)) +         
  #geom_hline(yintercept = 1.0, linetype = "dashed",
  #           colour = "red", linewidth = 0.8) +
  geom_point(alpha = 0.4, size = 1.2) +
  #geom_smooth(method = "loess", colour = "black",
  geom_smooth(method = "lm", colour = "black",
              linewidth = 1, se = TRUE) +
  stat_cor(method = "spearman",
           #stat_cor(method = "pearson",
           label.x.npc = "left",   # horizontal position
           label.y.npc = "top",    # vertical position
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_viridis_c(
    name   = "Cl (ug/L)",
    trans  = "log10",
    option = "plasma"
  ) +
  annotation_logticks(sides = "bl") +
  labs(
    title    = paste("Molar Na/Cl ratio vs. distance to nearest ", well_t, " well", sep = ""),
    subtitle = "Color = Cl concentration; ",
    x        = "Distance to nearest marginal well (m)",
    y        = paste(water_t,"Molar Na/Cl ratio in",S_t)
  ) +
  theme_bw(base_size = 12)


# ── 3. Cl trends over time ─────────────────────────────────────────────────────────

# Aggregate to annual median per site
Cl_annual <- Cl_Na %>%
  mutate(year = year(Samplingdate)) %>%
  group_by(SiteCode, year) %>%
  summarise(
    cl_median     = median(cl_conc, na.rm = TRUE),
    na_cl_median  = median(na_cl_molar, na.rm = TRUE),
    n             = n(),
    .groups = "drop"
  )

# Plot Cl trend over time
ggplot(Cl_annual, aes(x = year, y = cl_median)) +
  geom_point(alpha = 0.1, size = 0.8, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "black", linewidth = 1, se = TRUE) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  labs(
    title = paste("Chloride concentration trend over time in ",S_t,sep=""),
    x     = "Year",
    y     = "Median annual Cl (ug/L)"
  ) +
  theme_bw(base_size = 12)

# Fit the model
fit <- lm(log10(cl_median) ~ year,
          data = Cl_annual %>% filter(!is.na(cl_median), cl_median > 0))

# Extract slope and other stats
slope     <- coef(fit)["year"]
r2        <- summary(fit)$r.squared
pval      <- summary(fit)$coefficients["year", "Pr(>|t|)"]

