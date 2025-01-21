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
sample_spp <- "Great Argus"

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

