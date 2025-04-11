#build multi-panel scenario performance figure 


rm(list = ls())

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


source('Inputs/FixedScenarioParams.R')
#.....................................
#notes
#For birds and dung beetles, we must choose which spp_category we want to plot (i.e. winner/loser/intermediates)
#for megatrees, there is currently no temporal dimension incorporated (TO DO!). Also, megatrees is not a relative measure. it plots scenario landscape; SL megatrees is a seperate column. 
#for carbon and profits we must select the discount rate being applied; and whether we are looking at aboveground carbon only (ACD) or all carbon, including belowground (all)
#for profits, we must also select which costType we are plotting ("All_costs","HarvestProfits","ProtectionCosts" "CutAndRun"  )
#.....................................

#read in performance outcomes 

#select birds categorised either by my owner winner/loser/int classification...
birds <- readRDS("Data/OG_baseline_birdsSept24.rds") %>% unique() %>% rename(bird_grp = spp_category)
#or by whether threatened on IUCN red list 
birds2 <- readRDS("Data/OG_baseline_birdsIUCNSept24.rds") %>% unique() %>%  rename(bird_grp = threatened)

#join IUCN status and habitat-affinities
birds <- birds %>% rbind(birds2) 

#read in where old-growth forest is added as the baseline
dungBeetles <- readRDS("Data/MasterDBPerformance.rds")

carbon <-  readRDS("Data/MasterCarbonPerformance_withuncertainty.rds") 

# DO NOT USE OR TRUST THESE. 

# carbon_years <- readRDS("Data/test_carbonstock_years_withuncertainty.rds") %>%
#   rename( TOTcarbon_all_impact = all_carbon_stock,
#           TOTcarbon_ACD_impact = aboveground_carbon_stock) %>%
#   crossing(discount_rate = c("2%", "4%","6%" )) %>%
#   cbind(outcome = "carbon")
#   

megatrees <- readRDS("Data/MasterMegatreePerformance_with_uncertainty.rds") 

profits <- readRDS("Data/MasterFinancialPerformance.rds") %>%  unique() %>% 
  pivot_longer(cols = starts_with("NPV"), names_to = "discount_rate", values_to = "NPV") %>%
  mutate(discount_rate = gsub("NPV", "", discount_rate)) %>%  
  mutate(discount_rate = paste0(discount_rate,"%")) %>% na.omit() %>% as.data.table()

protection <- profits %>% filter(costType == "ProtectionCosts") %>% mutate(outcome = "protection")

# scenario_filters_D <- c("AllPrimary", "Mostly1L", "Mostly2L")
# protection %>% 
#   filter(scenarioName %in% scenario_filters_D) %>% 
#   ggplot(aes(x = production_target, y = NPV)) +
#   geom_point(aes(x = production_target, y = NPV)) +
#   facet_wrap(~scenarioName)+
#   theme_bw()

names(birds)
names(dungBeetles)
names(carbon)
names(megatrees)
names(profits)
names(protection)

#check same #of scenarios in each outcome
carbon %>%summarise(n = n_distinct(index, production_target))
megatrees %>%summarise(n = n_distinct(index))
birds %>%summarise(n = n_distinct(index))
dungBeetles %>%summarise(n = n_distinct(index))
profits %>%summarise(n = n_distinct(index))


#read in scenario information

#yield-matched scenarios
scenarios <- readRDS("Inputs/MasterAllScenarios.rds")
scenario_composition <- rbindlist(scenarios, use.names=TRUE) # get scenario composition
rm(scenarios)

#calculate total # of scenarios 
ScenarioNum <- scenario_composition %>% 
  filter(str_detect(scenarioName, "CY")) %>% #only extract current yield scenarios
  select(production_target, index) %>% unique() %>%  count()
print(ScenarioNum) #7703
# --------------Summarise scenario composition -------------------

#---- Calculate Prop OG in scenario ----

#get the amount of hab in each starting landscape 

habInStart <- all_start_landscape %>% select(scenarioStart) %>% unique() %>% 
  mutate(originalOG = c(1,0.2,0.2,0.8,0.2,0.2), 
         original1L = c(0,0.8,0,0,0.6,0), 
         original2L = c(0,0,0.8,0,0,0.6))

#build a function that calculates proportion of remaining habitat 
#in each scenario 

prop_OG_fun <- function(x){
  
  #proportion of TOTAL landscape [1000 parcels] in different habitat type 
  x %>% group_by(index, production_target) %>% 
    #total OG
    mutate(propOG = sum(num_parcels[habitat == "primary"])/1000,
           propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future","eucalyptus_future")])/1000,   
           propEuc = sum(num_parcels[habitat %in% c("eucalyptus_current","eucalyptus_future")])/1000,   
           propAlb = sum(num_parcels[habitat %in% c("albizia_current", "albizia_future")])/1000, 
            #prop-1L in the scenario landscape
           prop1L = sum(num_parcels[habitat == "once-logged"])/1000,
           #proportion of 2-L in the scenario landscape
           prop2L = sum(num_parcels[habitat == "twice-logged"])/1000) %>%  
    
    #get starting landscape
    mutate(scenarioStart = scenarioName) %>% 
    mutate(scenarioStart = str_remove(scenarioStart, "_IY_ND.csv")) %>%
    mutate(scenarioStart = str_remove(scenarioStart, "_CY_ND.csv")) %>%
    mutate(scenarioStart = str_remove(scenarioStart, "_IY_D.csv")) %>%
    mutate(scenarioStart = str_remove(scenarioStart, "_CY_D.csv")) %>% 
    ungroup %>% 
    
    #get total amount of each habitat in STARTING landscape for a scenario
    left_join(habInStart, by = "scenarioStart") %>% 
    
    #calculate PROPORTION of REMAINING original habitat type 
    #(nb there can actually be more once-logged or twice-logged forest in scenario than scenarioStart, if primary forest is logged)
    mutate(remainingOG = propOG/originalOG, 
           remaining1L = prop1L/original1L, 
           remaining2L = prop2L/original2L) %>%  
    #correct for INF values for if dividing by 0
    mutate_at(vars(remainingOG, remaining1L, remaining2L), ~ ifelse(is.infinite(.) | is.nan(.), 0, .)) %>%
    
    select(index, production_target, scenarioName,scenarioStart,
           propOG, propPlant,propAlb,propEuc,prop1L,prop2L,
           remainingOG,remaining1L,remaining2L) %>% unique()
  
}

propOGcomp <- prop_OG_fun(scenario_composition) %>% ungroup %>% as.data.table()

#------ addd scenario composition for each performance outcome  -------
#for each scenario, add the proportion starting landscapes

# #if index is numeric make character
# geom_results <- geom_results[, index := as.character(index)]
# geom_results <- geom_results[, production_target := as.numeric(production_target)]
# propOGcomp_dt <- propOGcomp_dt[, index := as.character(index)]
# propOGcomp_dt <- propOGcomp_dt[, production_target := as.numeric(production_target)]
dungBeetles <- as.data.table(dungBeetles)
birds <- propOGcomp[birds, on = .(index, production_target)] 
dungBeetles <- propOGcomp[dungBeetles, on = .(index, production_target)] 
carbon <- propOGcomp[carbon, on = .(index, production_target)] 
megatrees <- propOGcomp[megatrees, on = .(index, production_target)] %>% select(-c(scenarioName, scenarioStart)) %>% 
  rename(scenarioName = i.scenarioName,
           scenarioStart = i.scenarioStart)
profits <- propOGcomp[profits, on = .(index, production_target)] 
protection <- propOGcomp[protection, on = .(index, production_target)] 


#---------- # add BIVARIATE PLOTTING PARAMETRES --------------------

COL <- "DkBlue2" # define colour pallete
COL <- "BlueOr"
#get colours for bivariate plotting
biv_pallete <- bi_pal(COL, dim =4 ) # for plotting
cols <- data.frame(bi_pal(COL, dim = 4, preview = FALSE))
colnames(cols) <- c("hex")
cols <- cols %>% mutate(bi_class = rownames(.))

textSize  <- 13

#make bivar legend
primary_legend <- bi_legend(pal = "BlueOr", dim = 4, 
                            xlab = "Old-growth", 
                            ylab = "Once-logged", size = textSize)

onceL_legend <- bi_legend(pal = "BlueOr", dim = 4, 
                          xlab = "            Remaining old-growth", 
                          ylab = " Remainng once-logged",size = textSize)

twiceL_legend <- bi_legend(pal = "BlueOr", dim = 4, 
                           xlab = "          Remaining old-growth", 
                           ylab = " Remainng twice-logged",size = textSize)

all_legend <- plot_grid(primary_legend,onceL_legend,twiceL_legend, ncol =3)

#assign scenarios the colours from the bivariate plot for primary start
bivariate_colours_PRIM <- function(X){
  X %>%  bi_class(x = propOG, y = prop1L, dim = 4, style = "equal") %>%  
    left_join(cols, by = "bi_class") # add hex colours
}

birds<- bivariate_colours_PRIM(birds) %>% rename(hexP = hex)
dungBeetles<- bivariate_colours_PRIM(dungBeetles) %>% rename(hexP = hex)
carbon<- bivariate_colours_PRIM(carbon) %>% rename(hexP = hex)
megatrees<- bivariate_colours_PRIM(megatrees) %>% rename(hexP = hex)
profits<- bivariate_colours_PRIM(profits) %>% rename(hexP = hex)
protection<- bivariate_colours_PRIM(protection) %>% rename(hexP = hex)


#assign scenarios the colours from the bivariate plot for mostly 1L start
bivariate_colours_1L <- function(X){
  X %>%  bi_class(x = remainingOG, y = remaining1L, dim = 4, style = "equal") %>%  
    left_join(cols, by = "bi_class") # add hex colours
}

birds<- bivariate_colours_1L(birds) %>% rename(hex1L = hex)
dungBeetles<- bivariate_colours_1L(dungBeetles) %>% rename(hex1L = hex)
carbon<- bivariate_colours_1L(carbon) %>% rename(hex1L = hex)
megatrees<- bivariate_colours_1L(megatrees) %>% rename(hex1L = hex)
profits<- bivariate_colours_1L(profits) %>% rename(hex1L = hex)
protection <- bivariate_colours_1L(protection) %>% rename(hex1L = hex)



#assign scenarios the colours from the bivariate plot for mostly 2L start
bivariate_colours_2L <- function(X){
  X %>%  bi_class(x = remainingOG, y = remaining2L, dim = 4, style = "equal") %>%  
    left_join(cols, by = "bi_class") # add hex colours
}

birds<- bivariate_colours_2L(birds) %>% rename(hex2L = hex)
dungBeetles<- bivariate_colours_2L(dungBeetles) %>% rename(hex2L = hex)
carbon<- bivariate_colours_2L(carbon) %>% rename(hex2L = hex)
megatrees<- bivariate_colours_2L(megatrees) %>% rename(hex2L = hex)
profits<- bivariate_colours_2L(profits) %>% rename(hex2L = hex)
protection<- bivariate_colours_2L(protection) %>% rename(hex2L = hex)


#crosses or triangles depending on if SR or LR plantation
plantation_type <- function(x) {
  x %>% mutate(
  is_cross = case_when(
    propAlb > 0 & propEuc > 0 ~ ifelse(propAlb > propEuc, "Triangle", "Cross"),
    propAlb > 0 ~ "Triangle",    # If propAlb > 0, assign "Triangle"
    propEuc > 0 ~ "Cross",       # If propEuc > 0, assign "Cross"
    TRUE ~ "Point"               # If none of the above conditions are met, assign "Point"
  ))
}
birds <- plantation_type(birds)
dungBeetles <- plantation_type(dungBeetles)
carbon <- plantation_type(carbon)
megatrees <- plantation_type(megatrees)
profits <- plantation_type(profits)
protection <- plantation_type(protection)

rename_ScenarioName_fun <- function(x){
  x %>% mutate(scenarioName = case_when(
    scenarioName == "all_primary_CY_D.csv" ~ "AllPrimary",
    scenarioName == "mostly_1L_CY_D.csv" ~ "Mostly1L",
    scenarioName == "mostly_2L_CY_D.csv" ~ "Mostly2L",
    
    scenarioName == "all_primary_CY_ND.csv" ~"AllPrimaryNoDef",
    scenarioName == "mostly_1L_CY_ND.csv" ~ "Mostly1LNoDef",
    scenarioName == "mostly_2L_CY_ND.csv" ~ "Mostly2LNoDef",
    
    scenarioName == "primary_deforested_CY_D.csv" ~ "MostlyPrimary+DL",
    scenarioName == "mostly_1L_deforested_CY_D.csv" ~ "Mostly1L+DL",
    scenarioName == "mostly_2L_deforested_CY_D.csv" ~ "Mostly2L+DL",
    
    scenarioName == "primary_deforested_CY_ND.csv" ~ "MostlyPrimaryNoDef+DL",
    scenarioName == "mostly_1L_deforested_CY_ND.csv" ~ "Mostly1LNoDef+DL",
    scenarioName == "mostly_2L_deforested_CY_ND.csv" ~ "Mostly2LNoDef+DL",
    TRUE ~ scenarioName)) %>%  
   
     mutate(scenarioName, fct_relevel(scenarioName,
      "AllPrimary",    "Mostly1L", "Mostly2L",
       "AllPrimaryNoDef", "Mostly1LNoDef",  "Mostly2LNoDef",
      "MostlyPrimary+DL",  "Mostly1L+DL", "Mostly2L+DL",
      "MostlyPrimaryNoDef+DL", "Mostly1LNoDef+DL","Mostly2LNoDef+DL"))
}

birds <- rename_ScenarioName_fun(birds)
dungBeetles <- rename_ScenarioName_fun(dungBeetles)
carbon <- rename_ScenarioName_fun(carbon)
megatrees <- rename_ScenarioName_fun(megatrees)
profits <- rename_ScenarioName_fun(profits)
protection <-  rename_ScenarioName_fun(protection)

#------Summary statistics for paper -------------------
#found at the bottom

#######################################################
#--------- PLOT RESULTS ---------------------------------
#######################################################


#================= build master plot ==========================================================

master_plot_fun <- function(x, y_ggplot, y_geom_point, ylab_text, scenarioFilter, ylims = NULL, jitterwidth, jitterheight){

  #Reorder scenarioName according to scenarioFilter
  x <- x %>%
   mutate(scenarioName = fct_relevel(scenarioName, {{scenarioFilter}}))
  
  
  plot <- x %>% ggplot(aes(x = production_target, y = {{ y_ggplot }})) +
    geom_point(aes(
      x = production_target,
      y = {{ y_geom_point }},
      colour = case_when(
        scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
        scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
        scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
      ),
     # shape = is_cross 
     
     shape = ifelse(propPlant > 0, "Cross", "Point"), 
     alpha = 0.2,
     stroke = 0,
 #    size = ifelse(propPlant > 0, 1, 1),  # Adjust sizes: larger for triangles ("Cross") and smaller for points
     
     ), position = position_jitter(width = jitterwidth, height = jitterheight)) +
    scale_colour_identity() +
    scale_shape_manual(values = c("Point" = 19, "Cross" = 2)) +
    # scale_shape_manual(values = c("Point" = 19, "Cross" = 3, "Triangle" = 2)) + # Define shape mapping
    xlim(0, 1) +
    xlab(element_blank()) +
    ylab(ylab_text) +
    
    facet_wrap(~scenarioName, ncol = 4) +
    theme_bw(base_size = textSize) +
    theme(legend.position = "none",
          panel.grid.major = element_line(color = "grey90"),  # Faint major gridlines
          panel.grid.minor = element_blank(),                # Remove minor gridlines
          strip.text = element_blank()                       # Removes facet labels
    )             
  if (!is.null(ylims)) {
    plot <- plot + ylim(ylims)
  }
  
  return(plot)
}

#----include outcome-specific plotting information ---------
#----- define plotting filters  -----
# birds_plot <- birds %>% filter(spp_category == "loser")
# dungBeetles_plot <- dungBeetles %>% filter(spp_category == "loser")
# carbon_plot <- carbon %>% filter(discount_rate == DR_filt)
# megatrees_plot <- megatrees
# profits_plot <- profits %>% filter(costType == "HarvestProfits", discount_rate == DR_filt)


# spCategory <- "loser"
# DR_filt <- DR_filt <- "6%"
# costType = "HarvestProfits"

plot_with_specifics <- function(scenarioFilter,bird_grp, spCategory,DR_filt, costType){

#birds 
B <- birds %>% 
  filter(scenarioName %in% {{scenarioFilter}}) %>% 
  filter(bird_grp == {{bird_grp}}) %>% 
  master_plot_fun(              y_ggplot = medianRelativeOccupancy, 
                                y_geom_point = medianRelativeOccupancy,
                                scenarioFilter = scenarioFilter,
                                ylab_text =  "Median Relative 
Occupancy",
#(across posterior draws)",
                               # ylims = c(-210,0), 
                                jitterwidth = 0.05, 
                                jitterheight = -0.03) 



#dung beetles 
DB <- dungBeetles %>% 
  filter(scenarioName %in% {{scenarioFilter}}) %>% 
  filter(spp_category == {{spCategory}} ) %>% 
  master_plot_fun(y_ggplot = medianRelativeOccupancy, 
                                    y_geom_point = medianRelativeOccupancy,
                                    scenarioFilter = scenarioFilter,
                                    ylab_text =  "Median Relative 
Abundance",
#(across posterior draws)",
                                    # ylims = c(-210,0), 
                                    jitterwidth = 0.05, 
                                    jitterheight = -0.03) 

#megatree
M <- megatrees %>% 
  filter(scenarioName %in% {{scenarioFilter}}) %>% 
  master_plot_fun(y_ggplot = landscape_prop, 
                  y_geom_point = landscape_prop,
                  ylab_text =  "Megatree 
years",
                  #ylims = c(-210,0), 
                  scenarioFilter = scenarioFilter,
                  jitterwidth = 0.05, 
                  jitterheight = -0.04) 

#carbon 
C <- carbon %>% 
  filter(discount_rate == {{DR_filt}}) %>% 
  filter(scenarioName %in% {{scenarioFilter}}) %>% 
  master_plot_fun(y_ggplot =  TOTcarbon_all_impact/1000000000, #TOTcarbon_ACD_impact/1000000000,
                                    y_geom_point = TOTcarbon_all_impact/1000000000,
                                    ylab_text =  "Social Carbon Cost 
(USD 1000M)",
                                   # ylims = c(-21.0,0), 
                                    scenarioFilter = scenarioFilter,
                  
                          jitterwidth = 0.05, 
                          jitterheight = -0.03) 

#profits 
P <- profits %>% 
  filter(scenarioName %in% {{scenarioFilter}}) %>% 
  filter(costType == {{costType}} & discount_rate == {{DR_filt}}) %>%  
  master_plot_fun(y_ggplot = NPV/100000000, 
                                 y_geom_point = NPV/100000000,
                                 ylab_text =  "Harvest NPV
(USD 100M)",
                                 scenarioFilter = scenarioFilter,
                                # ylims = c(-20,40), 
                                 jitterwidth = 0.05, 
                                 jitterheight = -0.04) 

#protection costs
#profits 
PC <- protection %>% 
  filter(scenarioName %in% {{scenarioFilter}}) %>% 
  filter(discount_rate == {{DR_filt}}) %>%  
  master_plot_fun(y_ggplot = NPV/100000000, 
                  y_geom_point = NPV/100000000,
                  ylab_text =  "Protection NPV
(USD 100M)",
                  scenarioFilter = scenarioFilter,
                  # ylims = c(-20,40), 
                  jitterwidth = 0.05, 
                  jitterheight = -0.04) 


#plot combined figure 
figure <- plot_grid(B, DB, M,C, P,PC, ncol = 1)

#add legend 
Figure1 <- plot_grid(figure, all_legend, nrow =2,rel_heights = c(1, 0.2))

return(Figure1)
}


#-----plot final figures with desired combinations -----

scenario_filters_D <- c("AllPrimary", "Mostly1L", "Mostly2L")
scenario_filter_ND <- c("AllPrimaryNoDef", "Mostly1LNoDef", "Mostly2LNoDef")
scenario_filters_D_DL <- c("MostlyPrimary+DL", "Mostly1L+DL", "Mostly2L+DL")
scenario_filters_ND_DL <- c("MostlyPrimaryNoDef+DL", "Mostly1LNoDef+DL", "Mostly2LNoDef+DL")
scenario_filters_primary <- c("AllPrimary","MostlyPrimary+DL")

print(unique(birds$bird_grp))
print(unique(dungBeetles$spp_category))
print(unique(carbon$discount_rate))
print(unique(profits$costType))

#------------ final figures --------------------------------------------
#Final figures - decide which combinations of the above you want to plot  

# ---- Variable discount rates  --------
loserB_loserDB_harvestProfits_6DR <-
  plot_with_specifics(scenarioFilter = scenario_filters_D, 
                    bird_grp = "loser", 
                    spCategory = "loser",
                    DR_filt = "6%",
                    costType = "HarvestProfits")
# ---- Variable discount rates  --------
loserB_loserDB_harvestProfits_6DR <-
  plot_with_specifics(scenarioFilter = scenario_filters_primary, 
                      bird_grp = "loser", 
                      spCategory = "loser",
                      DR_filt = "6%",
                      costType = "HarvestProfits")

loserB_loserDB_harvestProfits_4DR <-
  plot_with_specifics(scenarioFilter = scenario_filters_D, 
                      bird_grp = "loser", 
                      spCategory = "loser",
                      DR_filt = "4%",
                      costType = "HarvestProfits")


#at low discount rates, some logging scenarios do as well as plantations. THis is probably because 
#i am artificially inflating future logging carbon benefits by assuming that ten years after logging 
#belowground-ground carbon (1) stops offsetting aboveground gains (2)actually fully recovers as a pool holding 0.31 of above-ground carbon 
#these are both unlikely in the read world bc (1) it might take far longer for belowground to become a sink than ten years
#i just took this value from Mills and Riutta who show ongoing losses at yr 10, and (2) If ACD does recover, is won't  suddenly recover fully in year 11, but is likely to be far more gradual 
loserB_loserDB_harvestProfits_2DR <-
  plot_with_specifics(scenarioFilter = scenario_filters_D, 
                      bird_grp = "loser", 
                      spCategory = "loser",
                      DR_filt = "2%",
                      costType = "HarvestProfits")

# ----  Variable species classifications  ------
IUCNB_loserDB_harvestProfits_6DR  <-
  plot_with_specifics(scenarioFilter = scenario_filters_D, 
                   bird_grp = "Y",   # if using IUCN groupings for birds
                   spCategory = "loser", 
                   DR_filt = "6%",
                   costType = "HarvestProfits")

loserB_loserDB_harvestProfits_6DR <-
  plot_with_specifics(scenarioFilter = scenario_filters_D, 
                    bird_grp = "loser", 
                    spCategory = "loser", 
                    DR_filt = "6%",
                    costType = "HarvestProfits")

intermediate1LB_intermediate1LDB_harvestProfits_6DR <-
plot_with_specifics(scenarioFilter = scenario_filters_D, 
                    bird_grp = "intermediate1L", 
                    spCategory = "intermediate1L", 
                    DR_filt = "6%",
                    costType = "HarvestProfits")

intermediate2LB_intermediate2LDB_harvestProfits_6DR <-
plot_with_specifics(scenarioFilter = scenario_filters_D, 
                    bird_grp = "intermediate2L", 
                    spCategory = "intermediate2L", 
                    DR_filt = "6%",
                    costType = "HarvestProfits")

winnerB_winnerDB_harvestProfits_6DR <-
plot_with_specifics(scenarioFilter = scenario_filters_D, 
                    bird_grp = "winner", 
                    spCategory = "winner", 
                    DR_filt = "6%",
                    costType = "HarvestProfits")




#---- variable costs ------
# plot_with_specifics(scenarioFilter = scenario_filters_D, 
#                     spCategory = "loser", 
#                     DR_filt = "2%",          #the numbers are too big to visually detect differences. Better to focus on difference (e.g. protection costs)
#                     costType = "All_costs")

#revenues to harvesters 
loserB_loserDB_harvestProfits_2DR<- plot_with_specifics(scenarioFilter = scenario_filters_D, 
                    bird_grp = "loser", 
                    spCategory = "loser", 
                    DR_filt = "2%",
                    costType = "HarvestProfits")

#costs of protecting, in a scenario where loggers continue to protect forest after harvests (providing de facto protection)
loserB_loserDB_protectionCosts_6DR<-
plot_with_specifics(scenarioFilter = scenario_filters_D, 
                    bird_grp = "loser", 
                    spCategory = "loser", 
                    DR_filt = "6%",
                    costType = "ProtectionCosts")



#---- changing starting landscape and rules ------

NoDef__loserB_loserDB_harvestProfits_6DR <- 
  plot_with_specifics(scenarioFilter = scenario_filter_ND, 
                    bird_grp = "loser", 
                    spCategory = "loser", 
                    DR_filt = "6%",
                    costType = "HarvestProfits")

NoDef__loserB_loserDB_harvestProfits_4DR <- 
  plot_with_specifics(scenarioFilter = scenario_filter_ND, 
                      bird_grp = "loser", 
                      spCategory = "loser", 
                      DR_filt = "4%",
                      costType = "HarvestProfits")

DefDL__loserB_loserDB_harvestProfits_6DR <- 
plot_with_specifics(scenarioFilter = scenario_filters_D_DL, 
                    bird_grp = "loser", 
                    spCategory = "loser", 
                    DR_filt = "6%",
                    costType = "HarvestProfits")

DefDL__loserB_loserDB_harvestProfits_4DR <- 
  plot_with_specifics(scenarioFilter = scenario_filters_D_DL, 
                      bird_grp = "loser", 
                      spCategory = "loser", 
                      DR_filt = "4%",
                      costType = "HarvestProfits")

NoDefDL__loserB_loserDB_harvestProfits_6DR <- 
  plot_with_specifics(scenarioFilter = scenario_filters_ND_DL, 
                    bird_grp = "loser", 
                    spCategory = "loser", 
                    DR_filt = "6%",
                    costType = "HarvestProfits")

NoDefDL__loserB_loserDB_harvestProfits_4DR <- 
  plot_with_specifics(scenarioFilter = scenario_filters_ND_DL, 
                      bird_grp = "loser", 
                      spCategory = "loser", 
                      DR_filt = "4%",
                      costType = "HarvestProfits")
#--------------- Export figures -----------
#set figure export path
path = "Figures/GeomPointFigs/manuscript_figures"
# Set the dimensions for A4 size in inches
width <- 8.27
height <- 11.69


#BULK EXPORT 
# List of your defined object names for exporting as PDFs
object_names_to_export <- c(
#  "loserB_loserDB_harvestProfits_6DR",
  "loserB_loserDB_harvestProfits_4DR",  #Fig2 
#  "loserB_loserDB_harvestProfits_2DR",
#  "IUCNB_loserDB_harvestProfits_6DR",
#  "intermediate1LB_intermediate1LDB_harvestProfits_6DR",
# "intermediate2LB_intermediate2LDB_harvestProfits_6DR",
#  "winnerB_winnerDB_harvestProfits_6DR",
#  "loserB_loserDB_harvestProfits_2DR",
#  "loserB_loserDB_protectionCosts_2DR",
#  "NoDef__loserB_loserDB_harvestProfits_2DR",
#  "NoDef__loserB_loserDB_harvestProfits_6DR",
   "NoDef__loserB_loserDB_harvestProfits_4DR",  #Fig S3
  "DefDL__loserB_loserDB_harvestProfits_4DR"   #Fig s6
#  "NoDefDL__loserB_loserDB_harvestProfits_2DR"
  # Add more object names as per your defined plots...
)


# Function to export figures to PDFsn
export_plots_to_pdf <- function(object_list, path, width, height) {
  for (object_name in object_list) {
    plot_path <- file.path(path, paste0(object_name, ".pdf"))
    ggsave(get(object_name), filename = plot_path, width = width, height = height, units = "in")
  }
}

# Call the function to export the plots
export_plots_to_pdf(object_names_to_export, path, width, height)

#SINGLE PLOT EXPORTS

# #Figure 2 
# ggsave(DefDL__loserB_loserDB_harvestProfits_6DR, 
#        filename = paste0(path, "/DefDL__loserB_loserDB_harvestProfits_6DR.pdf"),
#        width =  width, #in pixels 
#        height = height,
#        units = "in")
# 
# ggsave(DefDL__loserB_loserDB_harvestProfits_4DR, 
#        filename = paste0(path, "/DefDL__loserB_loserDB_harvestProfits_4DR.pdf"),
#        width =  width, #in pixels 
#        height = height,
#        units = "in")
# 
# ggsave(loserB_loserDB_harvestProfits_6DR, 
#                filename = paste0(path, "/TESTloserB_loserDB_harvestProfits_6DR.pdf"),
#                width =  width, #in pixels 
#                height = height,
#                units = "in")
# 

# ggsave(IUCNB_loserDB_harvestProfits_6DR, 
#          filename = paste0(path, "//IUCNB_loserDB_harvestProfits_6DR.pdf"),
#          width =  width, #in pixels 
#          height = height,
#          units = "in")

# ggsave(IUCNB_loserDB_harvestProfits_6DR, 
#        filename = paste0(path, "//IUCNB_loserDB_harvestProfits_6DR.pdf"),
#        width =  width, #in pixels 
#        height = height,
#        units = "in")

# ggsave(NoDef__loserB_loserDB_harvestProfits_6DR, 
#        filename = paste0(path, "//NoDef__loserB_loserDB_harvestProfits_6DR.pdf"),
#        width =  width, #in pixels 
#        height = height,
#        units = "in")
# 
# ggsave(NoDef__loserB_loserDB_harvestProfits_4DR, 
#        filename = paste0(path, "//NoDef__loserB_loserDB_harvestProfits_4DR.pdf"),
#        width =  width, #in pixels 
#        height = height,
#        units = "in")


#-----hand build extra plots -----


#-------------------- VARYING Discount rate for carbon only  ---------------

rate <- c("2%", "4%", "6%")
scenarioFilter <- scenario_filters_D
print(scenarioFilter)

#vary carbon impacts by discount rate while changing y lims for each discount rate
plots <- list()

# Function to create a single plot for a given discount rate
create_carbon_plot <- function(data, rate, ylim, scenarioFilter) {
  data %>%
    filter(discount_rate == rate) %>%
    filter(scenarioName %in% scenarioFilter) %>%
    master_plot_fun(
      y_ggplot = TOTcarbon_all_impact / 1000000000,
      y_geom_point = TOTcarbon_all_impact / 1000000000,
      ylab_text = paste("Social Carbon Cost (USD 1000M) - Rate", rate),
      ylims = ylim,
      scenarioFilter = scenarioFilter,
      jitterwidth = 0.05,
      jitterheight = -0.03
    )
}
# Define ylims for each discount rate
ylims_list <- list(
  "2%" = c(-400, 0),
  "4%" = c(-30, 0),
  "6%" = c(-12, 10)
)


# Generate plots for each discount rate
plots <- map(rate, ~ create_carbon_plot(carbon, .x, ylims_list[[.x]], scenarioFilter))

# Combine the plots
variousDR <- plot_grid(plotlist = plots, ncol = 1)


#vary financial impacts by discount rates 
plots <- list()

for (i in rate) {
  plot <- profits %>%
    filter(discount_rate == i) %>%
    filter(costType == "HarvestProfits") %>% 
    filter(scenarioName %in% scenario_filters_D) %>%
    master_plot_fun(
      y_ggplot = NPV / 100000000,
      y_geom_point = NPV / 100000000,
      ylab_text = "Harvest NPV
(USD 1000M)",
      ylims = c(0,50),
      scenarioFilter = scenarioFilter,
      jitterwidth = 0.05,
      jitterheight = -0.03
    )
  plots[[i]] <- plot
}

profitsDR <- plot_grid(plotlist = plots, ncol = 1)

#add legend 
#profitsDR <- plot_grid(variousDR, all_legend, nrow =2,rel_heights = c(1, 0.2))






#----altering species affinities -----


#BIRDS ####
grp <- c("loser", "Y", "N","intermediate1L", "intermediate2L", "winner")
plots <- list()
#birds 


plots <- list()

for (i in grp ) {
  plot <- birds %>%
    filter(bird_grp  == i) %>%
    filter(scenarioName %in% {{scenarioFilter}}) %>%
    master_plot_fun(
      y_ggplot = medianRelativeOccupancy, 
      y_geom_point = medianRelativeOccupancy,
      ylab_text =  "Median Relative 
      Occupancy", 
      scenarioFilter = scenarioFilter,
      jitterwidth = 0.05,
      jitterheight = -0.03
    )
  plots[[i]] <- plot
}

bird_grps <- plot_grid(plotlist = plots, ncol = 1)

#add legend 
bird_grps <- plot_grid(bird_grps, all_legend, nrow =2,rel_heights = c(1, 0.2))

#DUNG BEETLES ####

db_grp <- c("loser", "intermediate1L", "intermediate2L", "winner")
plots <- list()

for (i in db_grp ) {
  plot <- dungBeetles %>%
    filter(spp_category  == i) %>%
    filter(scenarioName %in% {{scenarioFilter}}) %>%
    master_plot_fun(
      y_ggplot = medianRelativeOccupancy, 
      y_geom_point = medianRelativeOccupancy,
      ylab_text =  "Median Relaltive 
      Abundance", 
      scenarioFilter = scenarioFilter,
      jitterwidth = 0.05,
      jitterheight = -0.03
    )
  plots[[i]] <- plot
}

DB_grps <- plot_grid(plotlist = plots, ncol = 1)

#add legend [messing up for some reason-just export without legend]
#DB_grps <- plot_grid(DB_grps, all_legend, nrow =2,rel_heights = c(1, 0.2))
#DB_grps <- plot_grid(DB_grps, all_legend, nrow =2,rel_heights = c(1, 0.3))

#-----harvest revenues and profits with different starting starting landscape ----

scenario_filters_D <- c("AllPrimary", "Mostly1L", "Mostly2L")
scenario_filter_ND <- c("AllPrimaryNoDef", "Mostly1LNoDef", "Mostly2LNoDef")
scenario_filters_D_DL <- c("MostlyPrimary+DL", "Mostly1L+DL", "Mostly2L+DL")
scenario_filters_ND_DL <- c("MostlyPrimaryNoDef+DL", "Mostly1LNoDef+DL", "Mostly2LNoDef+DL")

Profits_no_deforested <- profits %>% 
  filter(scenarioName %in% scenario_filters_D) %>% 
  filter(costType == "HarvestProfits" & discount_rate == "6%") %>%  
  master_plot_fun(y_ggplot = NPV/100000000, 
                  y_geom_point = NPV/100000000,
                  ylab_text =  "Harvest NPV
(USD 100M)",       
                  jitterwidth = 0.05, 
                  jitterheight = -0.04) 

Profits_no_deforested_DL <- profits %>% 
  filter(scenarioName %in% scenario_filters_D_DL) %>% 
  filter(costType == "HarvestProfits" & discount_rate == "6%") %>%  
  master_plot_fun(y_ggplot = NPV/100000000, 
                  y_geom_point = NPV/100000000,
                  ylab_text =  "Harvest NPV
(USD 100M)",
                  scenarioFilter = scenario_filters_D_DL,
                  # ylims = c(-20,40), 
                  jitterwidth = 0.05, 
                  jitterheight = -0.04) 


Protection_no_deforested <- profits %>% 
  filter(scenarioName %in% scenario_filters_D) %>% 
  filter(costType == "ProtectionCosts" & discount_rate == "6%") %>%  
  master_plot_fun(y_ggplot = NPV/100000000, 
                  y_geom_point = NPV/100000000,
                  ylab_text =  "Protection NPV
(USD 100M)",       
                  jitterwidth = 0.05, 
                  jitterheight = -0.04) 

Protection_no_deforested_DL <- profits %>% 
  filter(scenarioName %in% scenario_filters_D_DL) %>% 
  filter(costType == "ProtectionCosts" & discount_rate == "6%") %>%  
  master_plot_fun(y_ggplot = NPV/100000000, 
                  y_geom_point = NPV/100000000,
                  ylab_text =  "Protection NPV
(USD 100M)",
                  scenarioFilter = scenario_filters_D_DL,
                  # ylims = c(-20,40), 
                  jitterwidth = 0.05, 
                  jitterheight = -0.04) 

plot_economic <- plot_grid(Profits_no_deforested,Profits_no_deforested_DL,Protection_no_deforested,Protection_no_deforested_DL, ncol = 1)

#add legend 
#plot_economic <- plot_grid(plot_economic, all_legend, nrow =2,rel_heights = c(1, 0.2))
#plot_economic <- plot_grid(plot_economic, all_legend, nrow =2,rel_heights = c(1, 0.3))


#change economic discount rate: 


# --- export extra figs -----
#fig S4
ggsave(variousDR, 
       filename = paste0(path, "//carbon_2_4_6_dr_allPrimary_Mostly1l_Mostly2l.pdf"),
       width =  width, #in pixels 
       height = height/2,
       units = "in")
# 
# ggsave(variousDR, 
#        filename = paste0(path, "//ACDcarbon_2_4_6_dr_allPrimary_Mostly1l_Mostly2l.pdf"),
#        width =  width, #in pixels 
#        height = height/2,
#        units = "in")

#Fig S5
ggsave(profitsDR, 
       filename = paste0(path, "//harvestProfits_2_4_6_dr_allPrimary_Mostly1l_Mostly2l.pdf.pdf"),
       width =  width, #in pixels 
       height = height/2,
       units = "in")

##Fig S7
ggsave(bird_grps, 
       filename = paste0(path, "//bird_Loser_Threatened_NotThreaten_Int1l_Int2L_winners_allPrimary_Mostly1l_Mostly2l.pdf"),
       width =  width, #in pixels 
       height = height,
       units = "in")


#Fig s8
ggsave(DB_grps, 
       filename = paste0(path, "//DB_Loser_Int1l_Int2L_winners_allPrimary_Mostly1l_Mostly2l.pdf"),
       width =  width, #in pixels 
       height = height/2,
       units = "in")

#Not in supp yet but interesting 
ggsave(plot_economic, 
       filename = paste0(path, "//HarvestRev_Protection_withAndWithout_deforestedLand_DR6.pdf"),
       width =  width, #in pixels 
       height = height,
       units = "in")

# -----SUMMARY STATISTISCS FOR PAPER  --------


#--------------- 0.38 production target ------

#FULLY-PRIMARY 
#birds
p38birds <-  birds %>% filter(production_target == 0.38 & 
                                scenarioStart == "all_primary" & 
                                bird_grp == "loser")
((max(p38birds$medianRelativeOccupancy) - 
    min(p38birds$medianRelativeOccupancy))/ min(p38birds$medianRelativeOccupancy))*100
#beetles
p38beetles <-  dungBeetles %>% filter(production_target == 0.38 & 
                                        scenarioStart == "all_primary" & 
                                        spp_category == "loser")
((max(p38beetles$medianRelativeOccupancy) - 
    min(p38beetles$medianRelativeOccupancy))/ min(p38beetles$medianRelativeOccupancy))*100


#trees
p38trees <-  megatrees %>% filter(production_target == 0.38 & 
                                    scenarioStart == "all_primary" )

((max(p38trees$landscape_prop ) - 
    min(p38trees$landscape_prop ))/ min(p38trees$landscape_prop))*100


#carbon
p38carbon <-  carbon %>% filter(production_target == 0.38 & 
                                  scenarioStart == "all_primary" &
                                  discount_rate == "4%") %>% mutate(
                                    TOTcarbon_all_impact = TOTcarbon_all_impact/1000000000
                                  )                                      
min(p38carbon$TOTcarbon_all_impact )/
  max(p38carbon$TOTcarbon_all_impact ) 

max(p38carbon$TOTcarbon_all_impact )-  min(p38carbon$TOTcarbon_all_impact ) 
#harvest revenues
p38profHARV <- profits %>% filter(production_target == 0.38 &
                                    costType == "HarvestProfits"& 
                                    discount_rate == "4%"&
                                    i.scenarioStart == "all_primary") %>%  
  mutate(
    NPV = NPV/100000000)

#X-fold difference
max(p38profHARV $NPV )/
  min(p38profHARV $NPV ) 

#$ difference
(max(p38profHARV $NPV )-
  min(p38profHARV $NPV ))*100  

#protection costs
p38profPROT <- profits %>% filter(production_target == 0.38 &
                                    costType == "ProtectionCosts"& 
                                    discount_rate == "4%"&
                                    i.scenarioStart == "all_primary") %>%  
  mutate(
    NPV = NPV/100000000)

max(abs(p38profPROT $NPV ))
min(abs(p38profPROT $NPV ))

#$ difference 
(max(abs(p38profPROT $NPV )) - min(abs(p38profPROT $NPV )))*100

((max(abs(p38profPROT $NPV )) - 
    min(abs(p38profPROT $NPV )))/
    min(abs(p38profPROT $NPV )))*100

(( 1.506792 -  1.01965)/1.01965)*100


p38 <- propOGcomp %>% filter(production_target == 0.38 &
                               scenarioStart=="all_primary")

#MOSTLY LOW-INTENSITY LOGGED
p38_1L <- propOGcomp %>% filter(production_target == 0.38 &
                                  scenarioStart=="mostly_1L")
scenario_composition %>% filter(production_target == 0.38 &
                                  scenarioStart=="mostly_1L")
#birds
p38birds <-  birds %>% filter(production_target == 0.38 & 
                                scenarioStart == "mostly_1L" & 
                                bird_grp == "loser")
((max(p38birds$medianRelativeOccupancy) - 
    min(p38birds$medianRelativeOccupancy))/ min(p38birds$medianRelativeOccupancy))*100
#beetles
p38beetles <-  dungBeetles %>% filter(production_target == 0.38 & 
                                        scenarioStart == "mostly_1L" & 
                                        spp_category == "loser")
((max(p38beetles$medianRelativeOccupancy) - 
    min(p38beetles$medianRelativeOccupancy))/ min(p38beetles$medianRelativeOccupancy))*100


#trees
p38trees <-  megatrees %>% filter(production_target == 0.38 & 
                                    scenarioStart == "mostly_1L" )

((max(p38trees$landscape_prop ) - 
    min(p38trees$landscape_prop ))/ min(p38trees$landscape_prop))*100


#carbon
p38carbon <-  carbon %>% filter(production_target == 0.38 & 
                                  scenarioStart == "mostly_1L" &
                                  discount_rate == "4%") %>% mutate(
                                    TOTcarbon_all_impact = TOTcarbon_all_impact/1000000000
                                  )                                      
min(p38carbon$TOTcarbon_all_impact )/
  max(p38carbon$TOTcarbon_all_impact ) 

# -1.210305 -> clearing primary forest for plantations at 0.38
# -0.4379518 -> clearing once-logged forest for plantation = ~3X different


#harvest revenues
p38profHARV <- profits %>% filter(production_target == 0.38 &
                                    costType == "HarvestProfits"& 
                                    discount_rate == "4%"&
                                    i.scenarioStart == "mostly_1L") %>%  
  mutate(
    NPV = NPV/100000000)

#X-fold difference
max(p38profHARV $NPV )/
  min(p38profHARV $NPV ) 

#$ difference
(max(p38profHARV $NPV )-
    min(p38profHARV $NPV ))*100  

#protection costs
p38profPROT <- profits %>% filter(production_target == 0.38 &
                                    costType == "ProtectionCosts"& 
                                    discount_rate == "4%"&
                                    i.scenarioStart == "mostly_1L") %>%  
  mutate(
    NPV = NPV/100000000)

max(abs(p38profPROT $NPV ))
min(abs(p38profPROT $NPV ))

#% difference 
(max(abs(p38profPROT $NPV )) - min(abs(p38profPROT $NPV )))*100

# (max(abs(p38profPROT $NPV )) - 
#     min(abs(p38profPROT $NPV ))/ min(abs(p38profPROT $NPV)))*100

#$ difference 
((max(abs(p38profPROT $NPV )) - 
    min(abs(p38profPROT $NPV )))/
    min(abs(p38profPROT $NPV )))*100

p38 <- propOGcomp %>% filter(production_target == 0.38 &
                               scenarioStart=="all_primary")




#Mostly 2L
#MOSTLY HIGH-INTENSITY LOGGED
p38_2L <- propOGcomp %>% filter(production_target == 0.38 &
                                  scenarioStart=="mostly_2L")
scenario_composition %>% filter(production_target == 0.38 &
                                  scenarioStart=="mostly_2L")
#birds
p38birds <-  birds %>% filter(production_target == 0.38 & 
                                scenarioStart == "mostly_2L" & 
                                bird_grp == "loser")
((max(p38birds$medianRelativeOccupancy) - 
    min(p38birds$medianRelativeOccupancy))/ min(p38birds$medianRelativeOccupancy))*100
#beetles
p38beetles <-  dungBeetles %>% filter(production_target == 0.38 & 
                                        scenarioStart == "mostly_2L" & 
                                        spp_category == "loser")
((max(p38beetles$medianRelativeOccupancy) - 
    min(p38beetles$medianRelativeOccupancy))/ min(p38beetles$medianRelativeOccupancy))*100


#trees
p38trees <-  megatrees %>% filter(production_target == 0.38 & 
                                    scenarioStart == "mostly_2L" )

((max(p38trees$landscape_prop ) - 
    min(p38trees$landscape_prop ))/ min(p38trees$landscape_prop))*100


#carbon
p38carbon <-  carbon %>% filter(production_target == 0.38 & 
                                  scenarioStart == "mostly_2L" &
                                  discount_rate == "4%") %>% mutate(
                                    TOTcarbon_all_impact = TOTcarbon_all_impact/1000000000
                                  )                                      
min(p38carbon$TOTcarbon_all_impact )/
  max(p38carbon$TOTcarbon_all_impact ) 

# -1.525234 -> clearing primary forest for plantation
# -0.3736017 -> clearing once-logged forest for plantation = 4X different


#harvest revenues
p38profHARV <- profits %>% filter(production_target == 0.38 &
                                    costType == "HarvestProfits"& 
                                    discount_rate == "4%"&
                                    i.scenarioStart == "mostly_2L") %>%  
  mutate(
    NPV = NPV/100000000)

#X-fold difference 
max(p38profHARV $NPV )/
  min(p38profHARV $NPV ) 

#$diff in 1
(max(p38profHARV $NPV )-
    min(p38profHARV $NPV ))

#% difference
(max(p38profHARV $NPV )-
    min(p38profHARV $NPV ))*100  

#protection costs
p38profPROT <- profits %>% filter(production_target == 0.38 &
                                    costType == "ProtectionCosts"& 
                                    discount_rate == "4%"&
                                    i.scenarioStart == "mostly_2L") %>%  
  mutate(
    NPV = NPV/100000000)

max(abs(p38profPROT $NPV ))
min(abs(p38profPROT $NPV ))

#$ difference 
(max(abs(p38profPROT $NPV )) - min(abs(p38profPROT $NPV )))*100

#% difference
((max(abs(p38profPROT $NPV )) - 
    min(abs(p38profPROT $NPV )))/
    min(abs(p38profPROT $NPV )))*100

p38 <- propOGcomp %>% filter(production_target == 0.38 &
                               scenarioStart=="all_primary")




#-------- COMPARING DIFFERENT PRODUCTION targets --------
P = 0.1
P= 0.99
 birds %>% filter(production_target == P & 
                                scenarioStart == "all_primary" & 
                                bird_grp == "loser")  %>%  
  mutate(percentage_difference = ((max(medianRelativeOccupancy ) - min(medianRelativeOccupancy )) / min(medianRelativeOccupancy )) * 100) 
 P = 0.1
 P= 0.99
dungBeetles %>% filter(production_target == P & 
                                         scenarioStart == "all_primary" & 
                                         spp_category == "loser") %>% 
  mutate(percentage_difference = ((max(medianRelativeOccupancy ) - min(medianRelativeOccupancy )) / min(medianRelativeOccupancy )) * 100)

P = 0.1
P= 0.99
megatrees %>% filter(production_target == P & 
                         scenarioStart == "all_primary" ) %>% 
  mutate(percentage_difference = ((max(landscape_prop ) - min(landscape_prop )) / min(landscape_prop )) * 100)

x <- carbon %>% filter(production_target == P & 
                         scenarioStart == "all_primary" & 
                         discount_rate == "4%") %>%  
  mutate(diff = max(TOTcarbon_all_impact/1000000000) - min(TOTcarbon_all_impact/1000000000 ))

#4%
#at P 0.1, tot carbon = differenceis (1183740116-135203961) = 2.025785
#at P 0.1 tot difference is 20.05527

#profits

P = 0.1
P= 0.99
 profits %>% filter(production_target == P &
                                    costType == "HarvestProfits"& 
                                    discount_rate == "4%"&
                                    i.scenarioStart == "all_primary") %>%  
  mutate(
    NPV = NPV/100000000) %>% 
   mutate(diff = max(NPV) - min(NPV))
 
#protection
 P = 0.1
 P= 0.99
 profits %>% filter(production_target == P &
                      costType == "ProtectionCosts"& 
                      discount_rate == "4%"&
                      i.scenarioStart == "all_primary") %>%  
   mutate(
     NPV = NPV/100000000) %>% 
   mutate(diff = max(NPV) - min(NPV))
 
##ZERO DEFORESTATION 
 #birds 
x <- birds %>% filter(scenarioName == "Mostly1LNoDef" & bird_grp == "loser") %>% 
  filter(production_target ==0.46) 
  
#0.7872712 = P = 0.46 (dont have enough at 0.5) leave once logged
#0.5793892 = P = 0.46 harvest primary  (DIFF = 21.6)

((0.7872712 - 0.5793892)/0.5793892)*100


 #megatrees

x <- megatrees %>% filter(production_target == 0.46, scenarioName == "Mostly1LNoDef") 
 #12.422657 = P = 0.5 leave once logged
 #5.82306% = P = 0.5 haevest primary  (DIFF = 21.6) = 150%
((12.422657- 5.82306)/5.82306)*100

#carbon =
x <- carbon %>% filter(production_target == 0.46 & 
                         scenarioName == "Mostly1LNoDef" & 
                         discount_rate == "4%") %>%  
  mutate(diff = max(TOTcarbon_all_impact/1000000000) - min(TOTcarbon_all_impact/1000000000 ))

unique(carbon$scenarioName)


 #-8% = P = 0.5 leave once logged
 #-19 = P = 0.5 haevest primary  (DIFF = 21.6) = 150%
 
#-----COMPARING DIFFERENT SPECIES AFINITIES -----
#primary SL, considering low-intensity logging winners  
 birds %>% filter(production_target == 0.75 &    
                    scenarioStart == "all_primary" & 
                    bird_grp == "intermediate1L")  %>%  
   mutate(percentage_difference = ((max(medianRelativeOccupancy ) - min(medianRelativeOccupancy )) / min(medianRelativeOccupancy )) * 100)


dungBeetles %>% filter(production_target == 0.75 & 
                    scenarioStart == "all_primary" & 
                    spp_category== "intermediate1L")  %>%  
   mutate(percentage_difference = ((max(medianRelativeOccupancy ) - min(medianRelativeOccupancy )) / min(medianRelativeOccupancy )) * 100)

#mostly 2L SL, considering low-intensity logging winners  

birds %>% filter(production_target == 0.75 &    #using 0.91 as don't have scenarios above this for comparison 
                   scenarioStart == "mostly_2L" & 
                   bird_grp == "intermediate1L")  %>%  
  mutate(percentage_difference = ((max(medianRelativeOccupancy ) - min(medianRelativeOccupancy )) / min(medianRelativeOccupancy )) * 100)


dungBeetles %>% filter(production_target == 0.75 & 
                         scenarioStart == "mostly_2L" & 
                         spp_category== "intermediate1L")  %>%  
  mutate(percentage_difference = ((max(medianRelativeOccupancy ) - min(medianRelativeOccupancy )) / min(medianRelativeOccupancy )) * 100)

 
#amount of land used under different scenarios
X <- birds %>% filter(scenarioStart == "primary_deforested" &
                                  production_target == 1 )
 
X <- birds %>% filter(scenarioStart == "mostly_2L_deforested" &
                        production_target == 1 )



 
half <- profits %>% filter(production_target == 0.5 &
                             costType == "ProtectionCosts"& 
                             discount_rate == "6%"&
                             i.scenarioStart == "all_primary")


#carbon
scenarioFilter <- scenario_filters_D
carbonSummary <- carbon %>% filter(production_target == 0.75 &
                                     scenarioStart == "all_primary" & 
                                     discount_rate == "6%") %>% 
  # filter(scenarioName %in% {{scenarioFilter}}) %>% 
  mutate(TOTcarbon_all_impact = TOTcarbon_all_impact/1000000000)
#bird groups:
sppCategories <- readRDS("R_code/AssessBiodiversityOutcomes/Outputs/sppCategories.rds")
sppCategories %>%  group_by(spp_category) %>% count()
IUCN_classification <- read.csv("R_code/AssessBiodiversityOutcomes/Inputs/AllBorneoSpeciesTraits.csv") %>% 
  select(spp, redlistCategory) %>% unique()%>% rename(species = spp) %>% as.data.table() %>%  
  mutate(threatened = case_when(
    redlistCategory != "Least Concern" & redlistCategory != "" ~ "Y",
    TRUE ~ "N"
  ))
threatened_but_logging_resilient <- sppCategories %>% 
  left_join(IUCN_classification, by = "species") 


#economic 
econ <- profits %>% filter(production_target == 0.5 & scenarioStart == "all_primary" 
                           & scenarioName == "AllPrimary" & costType == "ProtectionCosts"&
                             discount_rate == "6%")   
#148610893-62777152=  85,833,741 difference in NPV at 6% 

#Percentage less= -136.7277
#  (85833741/−62777152)*100 = -136.7277
