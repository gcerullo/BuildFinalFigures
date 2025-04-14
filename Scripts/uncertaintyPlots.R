#uncertainty plots #####
#lets develop these at the species-level - plotting the error of taking the median across species grp
#conveys differences in per-species reponses rather than showing error - and it becomes unweildy. 


library(tidyverse)
library(ggplot2)
library(data.table)
library(ggpubr)
library(stringr) 
library(cowplot)
library(foreach)
library(biscale)
library(png)
library(grid)
library(forcats)
library(gridExtra)

#read in functions
source('Inputs/FixedScenarioParams.R')
source("Functions/scenarioCompositionFunctions.R")


#read in species-level data: 
birds <- readRDS("Data/species_level_relative_occ.rds")
carbon <-  readRDS("Data/MasterCarbonPerformance_withuncertainty.rds") 

#do with carbon years instead of econmic flux effects
carbon <- readRDS("Data/carbonstock_years__withuncertainty.rds") %>%
  rename( TOTcarbon_all_impact = all_carbon_stock,
          TOTcarbon_impact_all_upr =  all_carbon_stock_upr,
          TOTcarbon_impact_all_lwr =  all_carbon_stock_lwr,
          TOTcarbon_ACD_impact = aboveground_carbon_stock) %>%
  crossing(discount_rate = c("2%", "4%","6%" )) %>%  data.table() %>%  
  rename(TOTcarbon_all_impact_se = all_carbon_stock_err )
#.....................................
#CArbon

#.....................................
# Organize scenario composition for carbon data
propOGcomp <- prop_OG_fun(scenario_composition) %>% ungroup() %>% as.data.table()
carbon_data <- propOGcomp[carbon, on = .(index, production_target)] 
carbon_data <- bivariate_colours_PRIM(carbon_data) %>% rename(hexP = hex)
carbon_data <- bivariate_colours_1L(carbon_data) %>% rename(hex1L = hex)
carbon_data <- bivariate_colours_2L(carbon_data) %>% rename(hex2L = hex)
carbon_data <- plantation_type(carbon_data)
carbon_data <- rename_ScenarioName_fun(carbon_data)

# # Calculate standard errors from confidence intervals
# carbon_data <- carbon_data %>%
#   mutate(
#     # Standard error for TOTcarbon_all_impact
#     TOTcarbon_all_impact_se = (TOTcarbon_impact_all_upr - TOTcarbon_impact_all_lwr) / (2 * 1.96),
#     
#     # Standard error for TOTcarbon_ACD_impact
#     TOTcarbon_ACD_impact_se = (TOTcarbon_impact_ACD_upr - TOTcarbon_impact_ACD_lwr) / (2 * 1.96)
#   )

# Filter data as needed
carbon_filt <- carbon_data %>%
  filter(scenarioStart == "all_primary" |scenarioStart == "mostly_1L" |scenarioStart == "mostly_2L") %>% 
  filter(scenarioName == "AllPrimary" | scenarioName == "Mostly1L" | scenarioName == "Mostly2L")

# Add a jittered x column to the data
set.seed(123) # Set seed for reproducibility
carbon_filt <- carbon_filt %>%
  mutate(jittered_x = production_target + runif(n(), -0.01, 0.01)) # Shared jitter

# Plot total carbon impact with 95% CI
plot_total_carbon <- carbon_filt %>%
  ggplot(aes(x = jittered_x, y = TOTcarbon_all_impact)) +
  
  # Error bars with matching jitter
  geom_errorbar(aes(
    ymin = TOTcarbon_impact_all_lwr,
    ymax = TOTcarbon_impact_all_upr,
    color = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    )
  ),
  width = 0.02, # Horizontal cap width
  alpha = 0.8, # Fainter error bars
  size = 0.5 # Thinner error bars
  ) +
  
  # Points with increased prominence
  geom_point(aes(
    color = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    ),
    shape = ifelse(propPlant > 0, "Cross", "Point"),
    alpha = 0.8, # More solid points
    size = 2 # Larger point size
  )) +
  
  # Color scale and shape mapping
  scale_colour_identity() +
  scale_shape_manual(values = c("Point" = 19, "Cross" = 17)) +
  
  # Scale y-axis in billions for better readability
  scale_y_continuous(
    labels = function(x) paste0(round(x / 1e9, 1)), 
    limits = c(0,17000000000)
    ) +
  
  # Add labels
  labs(
  #  title = "Total Carbon Impact with 95% CI",
    y = "Scenario Carbon Stock Years (Bn Mg C)",
    x = "Production Target"
  ) +
  
  # Other plot elements
  xlim(0, 1) +
  facet_wrap(~scenarioName, ncol = 4) +
  theme_bw(base_size = textSize) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), # Remove minor gridlines
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) # Subtle major gridlines

plot_total_carbon


#Export for Supplementary
width <- 8.27
height <- 11.69
ggsave(plot_total_carbon, 
       filename =  "Figures/GeomPointFigs/manuscript_figures/carbonstockyear_figure.pdf",
       width =  width, #in pixels 
       height = height/2,
       units = "in")

# Plot ACD carbon impact with 95% CI
plot_ACD_carbon <- carbon_filt %>%
  ggplot(aes(x = jittered_x, y = TOTcarbon_ACD_impact)) +
  
  # Error bars for ACD
  geom_errorbar(aes(
    ymin = TOTcarbon_impact_ACD_lwr,
    ymax = TOTcarbon_impact_ACD_upr,
    color = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    )
  ),
  width = 0.02, # Horizontal cap width
  alpha = 0.8, # Fainter error bars
  size = 0.5 # Thinner error bars
  ) +
  
  # Points with increased prominence
  geom_point(aes(
    color = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    ),
    shape = ifelse(propPlant > 0, "Cross", "Point"),
    alpha = 0.8, # More solid points
    size = 2 # Larger point size
  )) +
  
  # Color scale and shape mapping
  scale_colour_identity() +
  scale_shape_manual(values = c("Point" = 19, "Cross" = 17)) +
  
  # Scale y-axis in billions for better readability
  scale_y_continuous(labels = function(x) paste0(round(x / 1e9, 1), "B")) +
  
  # Add labels
  labs(
    title = "Aboveground Carbon Density Impact with 95% CI",
    y = "ACD Carbon Impact (Mg)",
    x = "Production Target"
  ) +
  
  # Other plot elements
  xlim(0, 1) +
  facet_wrap(~scenarioName, ncol = 4) +
  theme_bw(base_size = textSize) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), # Remove minor gridlines
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) # Subtle major gridlines

plot_ACD_carbon


#.....................................
#organise scenario composition of scenarios
propOGcomp <- prop_OG_fun(scenario_composition) %>% ungroup %>% as.data.table()
birds <- propOGcomp[birds, on = .(index, production_target)] 
birds <- bivariate_colours_PRIM(birds) %>% rename(hexP = hex)
birds <- bivariate_colours_1L(birds) %>% rename(hex1L = hex)
birds <- bivariate_colours_2L(birds) %>% rename(hex2L = hex)
birds <- plantation_type(birds)
birds <- rename_ScenarioName_fun(birds)

unique(birds$species)
sample_spp <- "Helmeted Hornbill"

birdsfilt <- birds %>%  
  #filter(production_target >0.63 & production_target <0.65 ) %>% 
  #filter(production_target >0.2 & production_target <0.3 ) %>% 
  #filter(scenarioStart == "all_primary") %>%  
  filter(species == sample_spp) %>% 
  filter(scenarioStart == "all_primary")
  
#plot the 90% CI ####

# Add a jittered x column to the data
set.seed(123) # Set seed for reproducibility
birdsfilt <- birdsfilt %>%
  mutate(jittered_x = production_target + runif(n(), -0.01, 0.01)) # Shared jitter

# Plot with shared jitter
plot <- birdsfilt %>%
  ggplot(aes(x = jittered_x, y = medianRelativeOccupancy)) +
  
  # Error bars with matching jitter and reduced prominence
  geom_errorbar(aes(
    ymin = p1_medianRelativeOccupancy,
    ymax = p9_medianRelativeOccupancy,
    colour = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    )
  ),
  width = 0.02, # Horizontal cap width
  alpha = 0.8, # Fainter error bars
  size = 0.5 # Thinner error bars
  ) +
  
  # Points with increased prominence
  geom_point(aes(
    colour = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    ),
    shape = ifelse(propPlant > 0, "Cross", "Point"),
    alpha = 0.8, # More solid points
    size = 2 # Larger point size
  )) +
  
  # Color scale and shape mapping
  scale_colour_identity() +
  scale_shape_manual(values = c("Point" = 19, "Cross" = 17)) +
  
  # Other plot elements
  xlim(0, 1) +
  xlab(element_blank()) +
  facet_wrap(~scenarioName, ncol = 4) +
  theme_bw(base_size = textSize) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), # Remove minor gridlines
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) # Subtle major gridlines)

plot

#plot the interquartile range ####

# Add a jittered x column to the data
set.seed(123) # Set seed for reproducibility
birdsfilt <- birdsfilt %>%
  mutate(jittered_x = production_target + runif(n(), -0.01, 0.01)) # Shared jitter

# Plot with shared jitter
plotIQR <- birdsfilt %>%
  ggplot(aes(x = jittered_x, y = medianRelativeOccupancy)) +
  
  # Error bars for IQR with matching jitter and reduced prominence
  geom_errorbar(aes(
    ymin = medianRelativeOccupancy - IQR_medianRelativeOccupancy / 2,
    ymax = medianRelativeOccupancy + IQR_medianRelativeOccupancy / 2,
    colour = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    )
  ),
  width = 0.02, # Horizontal cap width
  alpha = 0.8, # Fainter error bars
  size = 0.5 # Thinner error bars
  ) +
  
  # Points with increased prominence
  geom_point(aes(
    colour = case_when(
      scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
      scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
      scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
    ),
    shape = ifelse(propPlant > 0, "Cross", "Point"),
    alpha = 0.8, # More solid points
    size = 2 # Larger point size
  )) +
  
  # Color scale and shape mapping
  scale_colour_identity() +
  scale_shape_manual(values = c("Point" = 19, "Cross" = 17)) +
  
  # Other plot elements
  xlim(0, 1) +
  xlab(element_blank()) +
  facet_wrap(~scenarioName, ncol = 4) +
  theme_bw(base_size = textSize) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), # Remove minor gridlines
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) # Subtle major gridlines)

plotIQR

