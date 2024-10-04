rm(list = ls())

##build petal plots on a scenario basis 
library(tidyverse)
library(ggplot2)
library(viridis)
library(data.table)
library(cowplot)
library(patchwork)

source('Inputs/FixedScenarioParams.R')

options(scipen = 999)

# Define colors for each habitat
habitat_colors <- c(
  "primary" = "#4d9221",
  "once-logged" = "#7fbc41",
  "restored" = "#b8e186",
  "twice-logged" = "#e6f5d0",
  "albizia_current" = "#fde0e6",
  "eucalyptus_current" = "#f1b6da",
  "deforested" = "#c51b7d"
)

# Join colors to the appropriate habitat in all_start_landscape
habitat_colors_df <- data.frame(habitat = names(habitat_colors), colors = habitat_colors, stringsAsFactors = FALSE)
all_start_landscape <- all_start_landscape %>%
  left_join(habitat_colors_df, by = "habitat")

print(all_start_landscape)
#get scenario composition:
scenarios <- readRDS("Inputs/MasterAllScenarios.rds")
scenario_composition <- rbindlist(scenarios, use.names=TRUE) # get scenario composition
rm(scenarios)



#read in performance outcomes 
birds <- readRDS("Data/OG_baseline_birdsSept24.rds") %>% select(index, production_target, scenarioName, scenarioStart, medianRelativeOccupancy, spp_category, outcome)
dungBeetles <- readRDS("Data/MasterDBPerformance.rds") %>% select(index, production_target, scenarioName, scenarioStart, medianRelativeOccupancy, spp_category, outcome)
carbon <-  readRDS("Data/MasterCarbonPerformance2.rds") %>% select(index, production_target, scenarioName, scenarioStart, TOTcarbon_all_impact, discount_rate, outcome)
megatrees <- readRDS("Data/MasterMegatreePerformance.rds") %>% select(index, production_target, scenarioName, scenarioStart, landscape_prop, outcome)
profits_df <- readRDS("Data/MasterFinancialPerformance.rds") %>%  
  pivot_longer(cols = starts_with("NPV"), names_to = "discount_rate", values_to = "NPV") %>%
  mutate(discount_rate = gsub("NPV", "", discount_rate)) %>% 
  mutate(discount_rate = paste0(discount_rate,"%")) %>% na.omit() %>% 
  select(index, production_target, scenarioName, scenarioStart, NPV, discount_rate, costType, outcome)


names(birds)
names(dungBeetles)
names(carbon)
names(megatrees)
names(profits_df)



# ------merge performance outcomes into one master dataframe -------- 
birds<- birds %>% rename(performance = medianRelativeOccupancy, 
                         modulator = spp_category)
dungBeetles<- dungBeetles %>% rename(performance = medianRelativeOccupancy, 
                         modulator = spp_category)

carbon <- carbon %>% rename(performance = TOTcarbon_all_impact, 
                                 modulator = discount_rate) #%>%  
  #remove negatives from carbon. So now bigger values are worse (i.e have a larger social carbon impact)
 # mutate(performance = abs(performance))

megatrees <- megatrees %>% rename(performance = landscape_prop) %>%  
  cbind(modulator = "MT")



profits <- profits_df %>% filter(costType == "HarvestProfits") %>% select(-costType)  %>%  
  rename(performance= NPV, 
         modulator = discount_rate)

costs <-  profits_df %>% filter(costType == "ProtectionCosts") %>% select(-costType)  %>%
  rename(performance= NPV,
         modulator = discount_rate) %>%
  mutate(outcome = "costs")


#combine and arrange the bars in a given order
# Define the order of outcomes
outcome_order <- c("profits", "costs","birds", "dungBeetles", "carbon", "megatrees")
masterDF <- birds %>% rbind(dungBeetles) %>% rbind(carbon) %>% 
  rbind(megatrees) %>% rbind(profits) %>% 
  rbind(costs) %>% 
 as.data.frame() %>%  
  #remove accidental space in output column 
  mutate(outcome  = str_replace_all(outcome, "\\s+", "")) %>% 
  mutate(outcome = factor(outcome, levels = outcome_order))



x <- masterDF %>%
  filter(scenarioStart == "all_primary") %>% 
  select(index,production_target, scenarioStart) %>%
  unique %>% group_by(production_target) %>% count()

#---- find some intersting scenarios to visualise ----------

petal_plot_fun <- function(x, P, SL_defined, DR_filt, spCategory,legend, filter_scenarios = NULL) {

single_production_target <- {{P}}


#select which SL to plot for 

SL <- {{SL_defined}}



#define starting parametres 
subs_df<- x %>% filter(modulator == "MT"| modulator == {{DR_filt}}| modulator == {{spCategory}} ) %>%  
  filter(str_detect(index, "CY_D")) %>% filter(scenarioStart == {{SL}}) %>% 
  filter(production_target ==single_production_target) %>% unique() %>% 
  mutate(performance = abs(performance))

# extract certain scenarios by index; if no filter_scenarios is provided, take all indexes 
if (!is.null(filter_scenarios) && length(filter_scenarios) > 0) {
  subs_df <- subs_df %>%
    filter(index %in% {{filter_scenarios}})
}

x <- subs_df 

#get the maximum observed performance for a scenario of the same production target
max_values <- x %>% filter(production_target == single_production_target) %>% 
  group_by(outcome) %>% filter(performance ==max(performance)) %>%  
  select(-c(index,production_target, scenarioName, scenarioStart,modulator)) %>% unique() %>% rename(max_performance = performance) 
min_values <-  x %>% filter(production_target == single_production_target) %>% 
  group_by(outcome) %>% filter(performance ==min(performance, na.rm = TRUE)) %>%  
  select(-c(index,production_target, scenarioName, scenarioStart,modulator)) %>% unique() %>% rename(min_performance = performance) 
min_max <-max_values %>% left_join(min_values)
#normalise performance so that it is relative to the maximum observed value
#(diffScenario to diff worst)/(diff best to difference worst)
x <- x %>% left_join(min_max) %>% unique() %>%
  mutate(diffScenMin = performance - min_performance, 
             diffRange = max_performance - min_performance, 
             normalised_performance = diffScenMin/diffRange) %>%  
 #add 0.1 to all values to that 0 values are still visible as a bar
   mutate(normalised_performance = normalised_performance+ 0.1 )

#store production targeted indexes
indexes <- x %>% select(index,production_target) %>% unique() %>%
  arrange(index)
#store starting landscape name 
SL_name <- x %>% select(scenarioStart) %>% slice(1) %>% pull()

#change order
outcome_order <- c("profits", "costs","birds", "dungBeetles", "carbon", "megatrees")
x %>% mutate(outcome = factor(outcome, levels = outcome_order))


#do petal plots 
Petalplots <- x %>% ggplot(aes(x = outcome, y = normalised_performance, fill = outcome)) +
  geom_bar(stat = "identity", width = 0.4, colour = "black", linewidth = 0.1, alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) + 
  ylim(0,1.1) +
  geom_hline(yintercept = 1.1, linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "black")+
  #facet_wrap(~index, nrow = 1)+
  facet_wrap(~index, ncol =  1)+
  scale_linetype_manual("Break-even", values = c("Break-even" = 2), name = "") +
  coord_polar(clip = "off") +
  
  # scale_fill_manual(values = adjusted_cols) +
  xlab("") + ylab("") +
  labs(fill = "Outcome") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = {{legend}},
        strip.text = element_blank()  # Remove facet titles
        ) +
  ggtitle(paste("P =", single_production_target, "\nStarting Landscape:", SL_name))


#get compositions of plotted scenario 
sc <- indexes %>% left_join(scenario_composition)

# Create pie charts for each 'index' and 'production_target' combination
composition_pies <- sc %>%
  arrange(index) %>% 
  group_by(index, production_target) %>%
  mutate(percentage = num_parcels /1000,
         habitat = factor(habitat, levels = names(habitat_colors))) %>%
  ggplot(aes(x = "", y = num_parcels, fill = habitat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
#  facet_wrap(~index, nrow = 1)+
  facet_wrap(~index, ncol = 1)+
  scale_fill_manual(values = habitat_colors) +
  labs( fill = "Habitat") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5), 
    legend.position = {{legend}},
    strip.text = element_blank(),  # Remove facet titles
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

#final_plots <-plot_grid(Petalplots, composition_pies, nrow =2, rel_heights = c(1, 0.3), align = 'v', axis = '-1000')
#final_plots<- Petalplots/composition_pies
final_plots <-plot_grid(composition_pies, Petalplots, nrow =2, rel_heights = c(0.2, 1), align = 'v', axis = '-1000')


#, rel_heights = c(1, 0.4))
return(final_plots)
}


#-------- Generate plots -------------
SL_vals  <- unique(masterDF$scenarioStart)
print(SL_vals)


#plot all petal plots; if I don't define filter_scenarios, will return all sxenarios
#for the production target 

a <- petal_plot_fun(x = masterDF, 
               P = 0.38,
               SL_defined = "all_primary", 
               DR_filt = "6%",
               legend = "none",
               spCategory = "loser", 
               filter_scenarios = c("all_primary_CY_D.csv 192",
                                    "all_primary_CY_D.csv 193",
                                    "all_primary_CY_D.csv 194",
                                    "all_primary_CY_D.csv 195")) 

#plot all petal plots
b <- petal_plot_fun(x = masterDF, 
               P = 0.38,
               SL_defined = "mostly_1L", 
               DR_filt = "6%", 
               legend = "none",
               spCategory = "loser", 
               filter_scenarios = c("mostly_1L_CY_D.csv 335",
                                    "mostly_1L_CY_D.csv 337",
                                    "mostly_1L_CY_D.csv 340",
                                    "mostly_1L_CY_D.csv 342"))

#plot all petal plots
c <- petal_plot_fun(x = masterDF, 
               P = 0.38,
               SL_defined = "mostly_2L", 
               DR_filt = "6%", 
               legend = "none",
               spCategory = "loser", 
               filter_scenarios = c("mostly_2L_CY_D.csv 274",
                                    "mostly_2L_CY_D.csv 270",
                                    "mostly_2L_CY_D.csv 271",
                                    "mostly_2L_CY_D.csv 272"))

fig1 <- plot_grid(a,b,c, nrow =3)
fig1 <- plot_grid(a,b,c, ncol =3)

fig1

#get the pie charts for the starting landscape: 
SL_pies <- all_start_landscape %>%
  group_by(scenarioStart) %>%
  mutate(percentage = num_parcels /1000,
         habitat = factor(habitat, levels = names(habitat_colors))) %>%
  ggplot(aes(x = "", y = num_parcels, fill = habitat)) +
  geom_bar(stat = "identity", width = 1, colour = "black") +
  coord_polar("y", start = 0) +
  facet_wrap(~scenarioStart, nrow = 1)+
  scale_fill_manual(values = habitat_colors) +
  labs( fill = "Habitat") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5), 
#    legend.position = {{legend}},
    strip.text = element_blank(),  # Remove facet titles
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )


#get legends 

#do petal plots 
with_legend <- petal_plot_fun(x = masterDF, 
                    P = 0.26,
                    SL_defined = "mostly_1L", 
                    DR_filt = "2%", 
                    legend = "bottom",
                    spCategory = "loser")


#----------- export figure -----------------
#set figure export path
path = "figures/PetalPlots"
# Set the dimensions for A4 size in inches
width <- 8.27
height <- 11.69


#SINGLE PLOT EXPORTS 
ggsave(fig1, 
       filename = paste0(path, "//fig2PetalPlotsROutput.pdf"),
       width =  width, #in pixels 
       height = height,
       units = "in")

ggsave(SL_pies, 
       filename = paste0(path, "//StartingLandscapePies.pdf"),
       width =  width, #in pixels 
       height = height,
       units = "in")

ggsave(with_legend, 
       filename = paste0(path, "//legend.pdf"),
       width =  width, #in pixels 
       height = height,
       units = "in")
