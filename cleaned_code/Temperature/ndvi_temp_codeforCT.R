## NDVI and temp data for Hurricane social change manuscript (2020) 

# Load data --------------
load("ndvi_temp.RData")

# Format session ------------
hurr_theme <- theme_classic() + 
  theme(legend.position = "right",
        axis.ticks.x = element_blank(), 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 16, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 16),
        axis.title.y = element_text(family="Helvetica", color = "black", size = 16, face = "bold", angle = 90, vjust = 0.5),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 16), 
        legend.title = element_text(family="Helvetica", color = "black", size = 16, face = "bold"), 
        legend.text = element_text(family = "Helvetica", color = "black", size = 16, face = "bold"))


# NDVI -------------
# Are NDVI pre-/post- distributions sig. different 
ks.test(x = ndvi_vals[ndvi_vals$storm == "pre",]$median, 
        y = ndvi_vals[ndvi_vals$storm == "post",]$median)

# Plot percent change of NDVI values (compared to pre-hurricane median)
perc_change_plot <- ndvi_vals %>% 
  mutate(pos_neg = ifelse(perc_change >= 0, "pos", "neg")) %>% 
  ggplot(aes(x = date, y = perc_change)) + 
  geom_col(color = "white", fill = "grey70", size = 0.5, width = 8) + 
  geom_point(aes(color = pos_neg), size = 2.5, alpha = 1, show.legend = F) + 
  geom_segment(aes(x = date[1], 
                   xend = date[47], 
                   y = baseline, 
                   yend = baseline), 
               color = "grey20", size = 0.5, lty = 1) + 
  geom_vline(aes(xintercept = as.Date("2017-09-20")), lty = 2, size = 0.8, color = "grey20") +
  hurr_theme + 
  scale_y_continuous(limits = c(-90,60), breaks = seq(from = 40, to = -100,by = -20)) + # add space for pics of Cayo
  scale_color_manual(values = c("sienna4", "forestgreen")) + 
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank()) + 
  labs(x = "Date", y = "Percent vegetation change") 
ggsave(perc_change_plot, filename = "perc_change_plot.pdf", device = "pdf", path = "~/Desktop/", width = 8, height = 6)

# Plot NDVI values 
ndvi_vals %>% ggplot(aes(x = date, y = median)) + 
  geom_point(aes(color = median), size = 2) +
  geom_vline(aes(xintercept = as.Date("2017-09-20")), lty = 2, size = 1.2, color = "grey60") +
  hurr_theme + 
  geom_line(aes(color = median), size = 1.5) +
  scale_y_continuous(limits = c(0, 0.6), breaks = c(0, 0.25, 0.5)) + #c(-1, 1)
  labs(y = "Median NDVI", x = "Date") + 
  scale_color_gradient(low = "sienna", high = "darkgreen") + 
  theme(legend.position = "none")


# Thermochron temps -------------
# plot temp by quarter of the year and state
state_by_quater <- temps %>% 
  ggplot(aes(x = quarter, y = Value, fill = state)) + 
  geom_boxplot() + # outlier.colour = "white" # can hide outliers 
  hurr_theme + 
  scale_fill_manual(values=c("#5C821A", "#F4CC70","grey60"), name = "") + 
  scale_color_manual(values=c("#5C821A", "#F4CC70","grey60"), name = "") + 
  labs(y = "Median temp (°C)", x = "Quarter")
ggsave(state_by_quater, filename = "state_by_quater.png", device = "png", path = "~/Desktop/", width = 8, height = 6)

# 
summary(lmer(Value ~ state + (1 | ThermochronNumber) + (1|DeploymentNumber), data = temps))
summary(lmer(Value ~ state*quarter + (1 | ThermochronNumber) + (1|DeploymentNumber), data = temps))


