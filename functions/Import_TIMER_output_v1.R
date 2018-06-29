#pre: Timer_scenario and IMAGE_scenario are the names of the scenarios.
#     These scenarios should come from the TIMER outputlib and IMAGE Scenlib (do not change structure).
#     and must be located in TIMER_folder and IMAGE_folder (set in 'Settings.R')
#      
#post: this script imports TIMER and IMAGE output and adds each file as data frame to a list
# 1. Creates lables for TIMER output files
# 2. Imports files from TIMER_folder
# 3. Possible changes to TIMER output files
#    - If no total exists, this is added to the table
#    - Dummy region is removed and replaced by "EU", which is the sum of "WEU" and "CEU"
# 4. Create list

# TODO
# TPES

# IMAGE/<scenario>/output
# TIMER_2015/<scenario>/indicatoren
# TIMER_2015/<scenario>/tuss

# GHG EMISSIONS
# CO2Spec
# ENEMISCO2
# ENEMISCH4
# ENEMISN2O
# INDEMISCO2
# INDEMISCH4
# INDEMISN2O

# LUEMCO2
# LUEMCH4
# LUEMN2O

# ENERGY SUPPLY
# ElecProd
# EnergyProd
# FinalEnergy
# Residential_FinalEnergy

# ENERGY DEMAND

# DRIVERS
# POP
# GDP_MER
# GDP_PP
# IVA

#library(base)
ImportTimerScenario <- function(TIMER_scenario = 'SSP2', IMAGE_scenario = 'SSP2')
{ 
  
  source(paste('functions', 'mym2r.R', sep='/'))
  source(paste('functions', 'Settings.R', sep='/'))

  #TIMER_scenario = 'NPi_update'
  #IMAGE_scenario = 'NPi'
  
  if(!dir.exists(paste(TIMER_folder, TIMER_scenario, sep="/")))
  { print("The TIMER scenario is not recognised, is it located in the TIMER_folder (see Settings.r)?")
    stop()
  }
  if(!dir.exists(paste(IMAGE_folder, IMAGE_scenario, sep="/")))
  { print("The IMAGE scenario is not recognised, is it located in the TIMER_folder (see Settings.r)?")
    stop()
  }
#1.

# GHG EMISSIONS
#2, 3.
# prepare correct labels for mym file dimensions
#CO2Spec = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), TIMER_scenario, filename='CO2Spec.out', novarname = T)
CO2Spec = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                          filename='CO2Spec.out', varname=NULL, 
                          collist=list(regions28,sector2), 
                          namecols=c('region','sector'), novarname = TRUE)
CO2Spec <- subset(CO2Spec, region != "dummy")
CO2Spec$region = factor(CO2Spec$region, levels=regions28_EU)
EU <- inner_join(filter(CO2Spec, region=='WEU'), filter(CO2Spec, region=='CEU'), by=c("year", "sector"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, value)
EU$region = factor(EU$region, levels=regions28_EU)
CO2Spec <- rbind(CO2Spec, EU)
CO2Spec$region = factor(CO2Spec$region,levels=regions28_EU)
CO2Spec <- mutate(CO2Spec, unit="kg C")
                  
ENEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                            filename='ENEMISCO2.out', varname=NULL, 
                            collist=list(regions28,sector3,energy_carrier_emis), 
                            namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
#ENEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='ENEMISCO2.out', novarname = T)
ENEMISCO2 <- subset(ENEMISCO2, region != "dummy")
ENEMISCO2$region = factor(ENEMISCO2$region, levels=regions28_EU)
EU <- inner_join(filter(ENEMISCO2, region=='WEU'), filter(ENEMISCO2, region=='CEU'), by=c("year", "sector", "energy_carrier"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
EU$region = factor(EU$region, levels=regions28_EU)
#EU$region = factor(EU$region, levels=regions28)
ENEMISCO2 <- bind_rows(ENEMISCO2, EU)
ENEMISCO2$region = factor(ENEMISCO2$region,levels=regions28_EU)
ENEMISCO2 <- mutate(ENEMISCO2, unit="Gt C")

ENEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                            filename='ENEMISCH4.out', varname=NULL, 
                            collist=list(regions28,sector3,energy_carrier_emis), 
                            namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
#ENEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='ENEMISCH4.out', novarname = T)
ENEMISCH4 <- subset(ENEMISCH4, region != "dummy")
ENEMISCH4$region = factor(ENEMISCH4$region, levels=regions28_EU)
EU <- inner_join(filter(ENEMISCH4, region=='WEU'), filter(ENEMISCH4, region=='CEU'), by=c("year", "sector", "energy_carrier"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
EU$region = factor(EU$region, levels=regions28_EU)
ENEMISCH4 <- rbind(ENEMISCH4, EU)
ENEMISCH4$region = factor(ENEMISCH4$region,levels=regions28_EU)
ENEMISCH4 <- mutate(ENEMISCH4, unit="Mt")

ENEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                            filename='ENEMISN2O.out', varname=NULL, 
                            collist=list(regions28,sector3,energy_carrier_emis), 
                            namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
#ENEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""),filename='ENEMISN2O.out', novarname = T)
ENEMISN2O <- subset(ENEMISN2O, region != "dummy")
ENEMISN2O$region = factor(ENEMISN2O$region, levels=regions28_EU)
EU <- inner_join(filter(ENEMISN2O, region=='WEU'), filter(ENEMISN2O, region=='CEU'), by=c("year", "sector", "energy_carrier"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
EU$region = factor(EU$region, levels=regions28_EU)
ENEMISN2O <- rbind(ENEMISN2O, EU)
ENEMISN2O$region = factor(ENEMISN2O$region,levels=regions28_EU)
ENEMISN2O <- mutate(ENEMISN2O, unit="Mt N")

INDEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                             filename='INDEMISCO2.out', varname=NULL, 
                             collist=list(regions28,industrial_process_CO2), 
                             namecols=c('region','industrial_process'), novarname = TRUE)
#INDEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='INDEMISCO2.out', novarname = T)
INDEMISCO2 <- subset(INDEMISCO2, region != "dummy")
INDEMISCO2$region = factor(INDEMISCO2$region, levels=regions28_EU)
EU <- inner_join(filter(INDEMISCO2, region=='WEU'), filter(INDEMISCO2, region=='CEU'), by=c("year", "industrial_process"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, industrial_process, value)
EU$region = factor(EU$region, levels=regions28_EU)
INDEMISCO2 <- rbind(INDEMISCO2, EU)
INDEMISCO2$region = factor(INDEMISCO2$region,levels=regions28_EU)
INDEMISCO2 <- mutate(INDEMISCO2, unit="Gt C")

INDEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                             filename='INDEMISCH4.out', varname=NULL, 
                             collist=list(regions28,industrial_process_CH4), 
                             namecols=c('region','industrial_process'), novarname = TRUE)
#INDEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='INDEMISCH4.out', novarname = T)
INDEMISCH4 <- subset(INDEMISCH4, region != "dummy")
INDEMISCH4$region = factor(INDEMISCH4$region, levels=regions28_EU)
EU <- inner_join(filter(INDEMISCH4, region=='WEU'), filter(INDEMISCH4, region=='CEU'), by=c("year", "industrial_process"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, industrial_process, value)
EU$region = factor(EU$region, levels=regions28_EU)
INDEMISCH4 <- rbind(INDEMISCH4, EU)
INDEMISCH4$region = factor(INDEMISCH4$region,levels=regions28_EU)
INDEMISCH4 <- mutate(INDEMISCH4, unit="Mt")

INDEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                             filename='INDEMISN2O.out', varname=NULL, 
                             collist=list(regions28,industrial_process_N2O), 
                             namecols=c('region','industrial_process'), novarname = TRUE)
#INDEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='INDEMISN2O.out', novarname = T)
INDEMISN2O <- subset(INDEMISN2O, region != "dummy")
INDEMISN2O$region = factor(INDEMISN2O$region, levels=regions28_EU)
EU <- inner_join(filter(INDEMISN2O, region=='WEU'), filter(INDEMISN2O, region=='CEU'), by=c("year", "industrial_process"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, industrial_process, value)
EU$region = factor(EU$region, levels=regions28_EU)
INDEMISN2O <- rbind(INDEMISN2O, EU)
INDEMISN2O$region = factor(INDEMISN2O$region,levels=regions28_EU)
INDEMISN2O <- mutate(INDEMISN2O, unit="Mt N")

HFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                          filename='EMISHFC_reg.out', varname=NULL, 
                          collist=list(regions27,HFC), 
                          namecols=c('region','HFC_gas'), novarname = TRUE)
#HFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='EMISHFC_reg.out', novarname = T)
HFC_reg <- subset(HFC_reg, region != "dummy")
HFC_reg$region = factor(HFC_reg$region, levels=regions28_EU)
EU <- inner_join(filter(HFC_reg, region=='WEU'), filter(HFC_reg, region=='CEU'), by=c("year", "HFC_gas"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, HFC_gas, value)
EU$region = factor(EU$region, levels=regions28_EU)
HFC_reg <- rbind(HFC_reg, EU)
HFC_reg$region = factor(HFC_reg$region,levels=regions28_EU)
HFC_reg <- mutate(HFC_reg, unit="kt")

PFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                          filename='EMISPFC_reg.out', varname=NULL, 
                          collist=list(regions27,PFC), 
                          namecols=c('region','PFC_gas'), novarname = TRUE)
#PFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='EMISPFC_reg.out', novarname = T)
PFC_reg <- subset(PFC_reg, region != "dummy")
PFC_reg$region = factor(PFC_reg$region, levels=regions28_EU)
EU <- inner_join(filter(PFC_reg, region=='WEU'), filter(PFC_reg, region=='CEU'), by=c("year", "PFC_gas"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, PFC_gas, value)
EU$region = factor(EU$region, levels=regions28_EU)
PFC_reg <- rbind(PFC_reg, EU)
PFC_reg$region = factor(PFC_reg$region,levels=regions28_EU)
PFC_reg <- mutate(PFC_reg, unit="kt")

LUEMCO2 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, "/output", sep=""), 
                          filename='LUEMCO2.out', varname=NULL, 
                          collist=list(land_use_source_CO2, regions27), 
                          namecols=c('source','region'), novarname = TRUE)
#LUEMCO2 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario,"/output", sep=""), filename='LUEMCO2.out', novarname = T)
LUEMCO2$region = factor(LUEMCO2$region, levels=regions28_EU)
EU <- inner_join(filter(LUEMCO2, region=='WEU'), filter(LUEMCO2, region=='CEU'), by=c("year", "source"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, value)
EU$region = factor(EU$region, levels=regions28_EU)
LUEMCO2 <- rbind(LUEMCO2, EU)
LUEMCO2$region = factor(LUEMCO2$region,levels=regions28_EU)
LUEMCO2 <- mutate(LUEMCO2, unit="Gt C")

LUEMCH4 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, "/output", sep=""), 
                          filename='LUEMCH4.out', varname=NULL, 
                          collist=list(land_use_source_CH4, regions27), 
                          namecols=c('source','region'), novarname = TRUE)
#LUEMCH4 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario,"/output", sep=""), filename='LUEMCH4.out', novarname = T)
LUEMCH4$region = factor(LUEMCH4$region, levels=regions28_EU)
EU <- inner_join(filter(LUEMCH4, region=='WEU'), filter(LUEMCH4, region=='CEU'), by=c("year", "source"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, value)
EU$region = factor(EU$region, levels=regions28_EU)
LUEMCH4 <- rbind(LUEMCH4, EU)
LUEMCH4$region = factor(LUEMCH4$region,levels=regions28_EU)
LUEMCH4 <- mutate(LUEMCH4, unit="Mt")

LUEMN2O = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, "/output", sep=""), 
                          filename='LUEMN2O.out', varname=NULL, 
                          collist=list(land_use_source_N2O, regions27), 
                          namecols=c('source','region'), novarname = TRUE)
#LUEMN2O = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario,"/output", sep=""), filename='LUEMN2O.out', novarname = T)
LUEMN2O$region = factor(LUEMN2O$region, levels=regions28_EU)
EU <- inner_join(filter(LUEMN2O, region=='WEU'), filter(LUEMN2O, region=='CEU'), by=c("year", "source"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, value)
EU$region = factor(EU$region, levels=regions28_EU)
LUEMN2O <- rbind(LUEMN2O, EU)
LUEMN2O$region = factor(LUEMN2O$region,levels=regions28_EU)
LUEMN2O <- mutate(LUEMN2O, unit="Mt N")

# ENERGY SUPPLY
ElecProd = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                          filename='ElecProd.out', varname=NULL, 
                          collist=list(regions28,energy_carrier_energy), 
                          namecols=c('region','energy_carrier'), novarname = TRUE)
#ElecProd = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='ElecProd.out', novarname = T)
ElecProd <- subset(ElecProd, region != "dummy")
ElecProd$region = factor(ElecProd$region, levels=regions28_EU)
EU <- inner_join(filter(ElecProd, region=='WEU'), filter(ElecProd, region=='CEU'), by=c("year", "energy_carrier"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
EU$region = factor(EU$region, levels=regions28_EU)
ElecProd <- rbind(ElecProd, EU)
ElecProd$region = factor(ElecProd$region,levels=regions28_EU)
# Add total to ElecProd, and make sure order is the same as original file
# 1. First add total column to table
# 2. Then change order of rows to match original sort order of TIMER file
ElecProd$energy_carrier <- as.character(ElecProd$energy_carrier)
ElecProd_tmp <- ElecProd %>% group_by(year, region) %>% summarise(value=sum(value))
ElecProd_tmp <- ungroup(ElecProd_tmp)
ElecProd_tmp <- mutate(ElecProd_tmp, energy_carrier="Total")
ElecProd_tmp <- select(ElecProd_tmp, year, region, energy_carrier, value)
ElecProd <- bind_rows(ElecProd, ElecProd_tmp)
ElecProd <- mutate(ElecProd, region_nr=match(region, regions28_EU))
ElecProd <- mutate(ElecProd, energy_carrier_nr=match(energy_carrier, energy_carrier))
ElecProd <- select(ElecProd, year, region_nr, energy_carrier_nr, everything())
ElecProd <- arrange(ElecProd, year, region_nr, energy_carrier_nr)
ElecProd$energy_carrier <- as.factor(ElecProd$energy_carrier)
ElecProd$region_nr <- NULL
ElecProd$energy_carrier_nr <- NULL
ElecProd <- mutate(ElecProd, unit="PJ")

#Eprod
EnergyProd = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                           filename='eprod.out', varname=NULL, 
                           collist=list(regions28,energy_carrier_energy2), 
                           namecols=c('region','energy_carrier'), novarname = TRUE)
#EnergyProd = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='eprod.out', novarname = T)
EnergyProd <- subset(EnergyProd, region != "dummy")
EnergyProd$region = factor(EnergyProd$region, levels=regions28_EU)
EU <- inner_join(filter(EnergyProd, region=='WEU'), filter(EnergyProd, region=='CEU'), by=c("year", "energy_carrier"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
EU$region = factor(EU$region, levels=regions28_EU)
EnergyProd <- rbind(EnergyProd, EU)
#EnergyProd$region = factor(EnergyProd$region,levels=regions28_EU)
EnergyProd <- mutate(EnergyProd, unit="PJ")

#RSE
FinalEnergy = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                              filename='rse.out', varname=NULL, 
                              collist=list(regions28,sector,energy_carrier_demand), 
                              namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
#FinalEnergy = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/indicatoren", sep=""), filename='rse.out', novarname = T)
FinalEnergy <- subset(FinalEnergy, region != "dummy")
FinalEnergy$region = factor(FinalEnergy$region, levels=regions28_EU)
EU <- inner_join(filter(FinalEnergy, region=='WEU'), filter(FinalEnergy, region=='CEU'), by=c("year", "sector", "energy_carrier"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
EU$region = factor(EU$region, levels=regions28_EU)
FinalEnergy <- rbind(FinalEnergy, EU)
FinalEnergy$region = factor(FinalEnergy$region,levels=regions28_EU)
FinalEnergy <- mutate(FinalEnergy, unit="PJ")

#Residential Final Energy
FinalEnergy_Residential = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                          filename='res_EU.out', varname=NULL, 
                                          collist=list(regions28,population_groups,res_enduse_functions), 
                                          namecols=c('region','population_group', 'enduse_function'), novarname = TRUE)
#FinalEnergy_Residential = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/tuss", sep=""), filename='res_EU.out', novarname = T)
FinalEnergy_Residential <- subset(FinalEnergy_Residential, region != "dummy")
FinalEnergy_Residential$region = factor(FinalEnergy_Residential$region, levels=regions28_EU)
EU <- inner_join(filter(FinalEnergy_Residential, region=='WEU'), filter(FinalEnergy_Residential, region=='CEU'), by=c("year", "population_group", "enduse_function"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, population_group, enduse_function, value)
EU$region = factor(EU$region, levels=regions28_EU)
FinalEnergy_Residential <- rbind(FinalEnergy_Residential, EU)
FinalEnergy_Residential$region = factor(FinalEnergy_Residential$region,levels=regions28_EU)
# add total
FinalEnergy_Residential$population_group <- as.character(FinalEnergy_Residential$population_group)
FinalEnergy_Residential$enduse_function <- as.character(FinalEnergy_Residential$enduse_function)
FinalEnergy_Residential_tmp <- FinalEnergy_Residential %>% group_by(year, region, population_group) %>% summarise(value=sum(value))
FinalEnergy_Residential_tmp <- ungroup(FinalEnergy_Residential_tmp)
FinalEnergy_Residential_tmp <- mutate(FinalEnergy_Residential_tmp, enduse_function="Total")
FinalEnergy_Residential_tmp <- select(FinalEnergy_Residential_tmp, year, region, population_group, enduse_function, value)
FinalEnergy_Residential <- bind_rows(FinalEnergy_Residential, FinalEnergy_Residential_tmp)
FinalEnergy_Residential <- mutate(FinalEnergy_Residential, unit="GJ")

# Person Kilometers Travelled (Tera km)
PersonKilometers = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                   filename='trp_trvl_pkm.out', varname=NULL, 
                                   collist=list(regions28,travel_mode), 
                                   namecols=c('region','travel_mode'), novarname = TRUE)
#PersonKilometers = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/tuss", sep=""), filename='trp_trvl_pkm.out', novarname = T)
PersonKilometers <- subset(PersonKilometers, region != "dummy")
PersonKilometers$region = factor(PersonKilometers$region, levels=regions28_EU)
EU <- inner_join(filter(PersonKilometers, region=='WEU'), filter(PersonKilometers, region=='CEU'), by=c("year", "travel_mode"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, value)
EU$region = factor(EU$region, levels=regions28_EU)
PersonKilometers <- rbind(PersonKilometers, EU)
PersonKilometers$region = factor(PersonKilometers$region,levels=regions28_EU)
PersonKilometers <- mutate(PersonKilometers, unit="T pkm")

# Energy use travel fuels
FinalEnergy_Transport = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                        filename='trp_trvl_Energy.out', varname=NULL, 
                                        collist=list(regions28,travel_mode), 
                                        namecols=c('region','travel_mode'), novarname = TRUE)
#FinalEnergy_Transport = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/tuss", sep=""), filename='trp_trvl_Energy.out', novarname = T)
FinalEnergy_Transport <- subset(FinalEnergy_Transport, region != "dummy")
FinalEnergy_Transport$region = factor(FinalEnergy_Transport$region, levels=regions28_EU)
EU <- inner_join(filter(FinalEnergy_Transport, region=='WEU'), filter(FinalEnergy_Transport, region=='CEU'), by=c("year", "travel_mode"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, value)
EU$region = factor(EU$region, levels=regions28_EU)
FinalEnergy_Transport <- rbind(FinalEnergy_Transport, EU)
FinalEnergy_Transport$region = factor(FinalEnergy_Transport$region,levels=regions28_EU)
FinalEnergy_Transport <- mutate(FinalEnergy_Transport, unit="XJ")

# Vehicle share cars
VehicleShare_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                    filename='trp_trvl_Vshare_car.out', varname=NULL, 
                                    collist=list(regions27,car_type), 
                                    namecols=c('region','car_type'), novarname = TRUE)
#VehicleShare_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario,"/tuss", sep=""), filename='trp_trvl_Vshare_car.out', novarname = T)
VehicleShare_cars$region = factor(VehicleShare_cars$region, levels=regions28_EU)
EU <- inner_join(filter(VehicleShare_cars, region=='WEU'), filter(VehicleShare_cars, region=='CEU'), by=c("year", "car_type"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, car_type, value)
EU$region = factor(EU$region, levels=regions28_EU)
VehicleShare_cars <- rbind(VehicleShare_cars, EU)
VehicleShare_cars$region = factor(VehicleShare_cars$region,levels=regions28_EU)
VehicleShare_cars <- mutate(VehicleShare_cars, unit="")

#POP
# Unit: million people
POP = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                      filename='pop.scn', varname=NULL, 
                      collist=list(regions28), 
                      namecols=c('region'), novarname = TRUE)
#POP = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), TIMER_scenario, filename='pop.scn', novarname = T)
POP_keep = POP
POP <- subset(POP, region != "dummy")
POP$region = factor(POP$region, levels=regions28_EU)
EU <- inner_join(filter(POP, region=='WEU'), filter(POP, region=='CEU'), by=c("year"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
EU$region = factor(EU$region, levels=regions28_EU)
POP <- rbind(POP, EU)
POP$region = factor(POP$region,levels=regions28_EU)
POP <- mutate(POP, unit="million people")

#GDP
# Unit: milliln US(2005) dollar
GDP_MER = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                      filename='gdptot.out', varname=NULL, 
                      collist=list(regions28), 
                      namecols=c('region'), novarname = TRUE)
#GDP_MER = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), TIMER_scenario, filename='gdptot.out', novarname = T)
GDP_MER <- subset(GDP_MER, region != "dummy")
GDP_MER$region = factor(GDP_MER$region, levels=regions28_EU)
EU <- inner_join(filter(GDP_MER, region=='WEU'), filter(GDP_MER, region=='CEU'), by=c("year"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
EU$region = factor(EU$region, levels=regions28_EU)
GDP_MER <- rbind(GDP_MER, EU)
GDP_MER$region = factor(GDP_MER$region,levels=regions28_EU)
GDP_MER <- mutate(GDP_MER, unit="million. US$(2005)")

GDP_PPPpc = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                            filename='gpdppp.out', varname=NULL, 
                            collist=list(regions28), 
                            namecols=c('region'), novarname = TRUE)
#GDP_PPPpc = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), TIMER_scenario, filename='gpdppp.out', novarname = T)
GDP_PPPpc <- subset(GDP_PPPpc, region != "dummy")
GDP_PPPpc$region = factor(GDP_PPPpc$region, levels=regions28_EU)
EU <- inner_join(filter(GDP_PPPpc, region=='WEU'), filter(GDP_PPPpc, region=='CEU'), by=c("year"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
EU$region = factor(EU$region, levels=regions28_EU)
GDP_PPPpc <- rbind(GDP_PPPpc, EU)
GDP_PPPpc$region = factor(GDP_PPPpc$region,levels=regions28_EU)
GDP_PPPpc <- mutate(GDP_PPPpc, unit="US$(2005)/capita")

GDP_PPP <- inner_join(POP, GDP_PPPpc, by=c("year", "region"))
GDP_PPP <- mutate(GDP_PPP, value=value.x*value.y)
GDP_PPP <- select(GDP_PPP, year, region, value)
GDP_PPP <- mutate(GDP_PPP, unit="million US$(2005)")

#IVA
# Unit: milliln US(2005) dollar
IVA = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                      filename='iva_pc.scn', varname=NULL, 
                      collist=list(regions28), 
                      namecols=c('region'), novarname = TRUE)
#IVA = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), TIMER_scenario, filename='iva_pc.scn', novarname = T)
IVA <- inner_join(POP_keep, IVA, by=c('year', 'region'))
IVA <- mutate(IVA, value = value.x * value.y)
IVA <- select(IVA, year, region, value)
IVA <- subset(IVA, region != "dummy")
IVA$region = factor(IVA$region, levels=regions28_EU)
EU <- inner_join(filter(IVA, region=='WEU'), filter(IVA, region=='CEU'), by=c("year"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
EU$region = factor(EU$region, levels=regions28_EU)
IVA <- rbind(IVA, EU)
IVA$region = factor(IVA$region,levels=regions28_EU)
IVA <- mutate(IVA, unit="thousand US$(2005)")

FloorSpace = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                             filename='res_FloorSpace.out', varname=NULL, 
                             collist=list(regions27,population_groups), 
                             namecols=c('region','population_group'), novarname = TRUE)
#FloorSpace = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), TIMER_scenario, filename='res_FloorSpace.out', novarname = T)
# translate from floorspace per capita to total floorspace using population data
FloorSpace = filter(FloorSpace, population_group=="Total")
FloorSpace = select(FloorSpace, year, region, value)
FloorSpace <- inner_join(POP_keep, FloorSpace, by=c('year', 'region'))
FloorSpace <- mutate(FloorSpace, value = value.x * value.y)
FloorSpace <- select(FloorSpace, year, region, value)
# add world total
FloorSpace <- subset(FloorSpace, region != "World")
FloorSpace_world <- group_by(FloorSpace, year) %>% summarise(value=sum(value))
FloorSpace_world <- mutate(FloorSpace_world, region="World")
FloorSpace <- rbind(FloorSpace, FloorSpace_world)
#FloorSpace <- subset(FloorSpace, region != "dummy")
FloorSpace$region = factor(FloorSpace$region, levels=regions28_EU)
EU <- inner_join(filter(FloorSpace, region=='WEU'), filter(FloorSpace, region=='CEU'), by=c("year"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
EU$region = factor(EU$region, levels=regions28_EU)
FloorSpace <- rbind(FloorSpace, EU)
FloorSpace$region = factor(FloorSpace$region,levels=regions28_EU)
FloorSpace <- mutate(FloorSpace, unit="m2")

#4.
l <- list(CO2Spec=CO2Spec,ENEMISCO2=ENEMISCO2,ENEMISCH4=ENEMISCH4,ENEMISN2O=ENEMISN2O,INDEMISCO2=INDEMISCO2,
          INDEMISCH4=INDEMISCH4,INDEMISN2O=INDEMISN2O,HFC_reg=HFC_reg,PFC_reg=PFC_reg,
          LUEMCO2=LUEMCO2,LUEMCH4=LUEMCH4,LUEMN2O=LUEMN2O,
          ElecProd=ElecProd, EnergyProd=EnergyProd, FinalEnergy=FinalEnergy, 
          FinalEnergy_Residential=FinalEnergy_Residential, PersonKilometers=PersonKilometers, FinalEnergy_Transport=FinalEnergy_Transport,
          VehicleShare_cars=VehicleShare_cars,
          POP=POP, GDP_MER=GDP_MER, GDP_PPP=GDP_PPP, IVA=IVA, FloorSpace=FloorSpace)
#assign(Scenario, get("l"))

}