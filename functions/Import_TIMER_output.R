#pre: Make sure you are in the 1-click environment R-scripts directory
#     e.g. Y:\ontwapps\Timer\Users\Mark\CD_LINKSupdate\R-scripts/TIMER_output
#     Timer_scenario and IMAGE_scenario are the names of the scenarios.
#     These scenarios should come from the TIMER outputlib and IMAGE Scenlib (do not change structure).
#     and must be located in TIMER_folder and IMAGE_folder
# Example:
# Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mark", sep="")
# Project=paste("CD_LINKSupdate")
# TIMERGeneration = 'TIMER_2015'

#post: this script imports TIMER and IMAGE output and adds each file as data frame to a list
# 1. Creates lables for TIMER output files
# 2. Imports files from TIMER_folder
# 3. Possible changes to TIMER output files
#    - If no total exists, this is added to the table
#    - Dummy region is removed and replaced by "EU", which is the sum of "WEU" and "CEU"
# 4. Create list

# TODO
# DRIVERS
# VA_share, ecstructure.out, indicatoren (fraction) (value added share)
# SVA, sva_pc.scn, tuss (US$) (service value added)
# PRIVC, pcons_pc.scn, tuss (US$) (private consumption)
# FINAL ENERGY
# UEpc, uepc.out, tuss (GJ/capita) (useful energy per capita)
# IntensTot, Intens.out, tuss, (GJ/$) (useful energy intensity)
# PriceSecFuel[27, 5, 8], epric.out, indicatoren, ($/GJ) (secondary fuel prices)


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
ImportTimerScenario <- function(TIMER_scenario = 'SSP2', IMAGE_scenario = 'SSP2', Rundir, Project, TIMERGeneration)
{ 
  
  source(paste('functions', 'mym2r.R', sep='/'))
  source(paste('functions', 'Settings.R', sep='/'))
  
  TIMER_folder = paste(Rundir, Project, "2_TIMER/outputlib", TIMERGeneration, Project, sep="/")
  IMAGE_folder = paste(Rundir, Project, "3_IMAGE/Scenario_lib/scen", sep="/")

print(TIMER_folder)
print(IMAGE_folder)

  if(!dir.exists(paste(TIMER_folder, TIMER_scenario, sep="/")))
  { print(paste("The TIMER scenario ", TIMER_scenario, " in the folder", TIMER_folder, " is not recognised", sep=""))
    stop()
  }
  if(!dir.exists(paste(IMAGE_folder, IMAGE_scenario, sep="/")))
  { print(paste("The IMAGE scenario ", IMAGE_scenario, " in the folder", IMAGE_folder, " is not recognised", sep=""))
    stop()
  }
  #1.
  
  # GHG EMISSIONS
  #2, 3.
  # prepare correct labels for mym file dimensions
  CO2Spec = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                            filename='CO2Spec.out', varname=NULL, 
                            collist=list(regions28,sector2), 
                            namecols=c('region','sector'), novarname = TRUE)
  CO2Spec <- subset(CO2Spec, region != "dummy")
  EU <- inner_join(filter(CO2Spec, region=='WEU'), filter(CO2Spec, region=='CEU'), by=c("year", "sector"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, value)
  CO2Spec <- rbind(CO2Spec, EU)
  CO2Spec$region = factor(CO2Spec$region,levels=regions28_EU)
  CO2Spec <- mutate(CO2Spec, unit="kg C")
  
  ENEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                              filename='ENEMISCO2.out', varname=NULL, 
                              collist=list(regions28,sector3,energy_carrier_emis), 
                              namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
  ENEMISCO2 <- subset(ENEMISCO2, region != "dummy")
  EU <- inner_join(filter(ENEMISCO2, region=='WEU'), filter(ENEMISCO2, region=='CEU'), by=c("year", "sector", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28)
  ENEMISCO2 <- bind_rows(ENEMISCO2, EU)
  ENEMISCO2$region = factor(ENEMISCO2$region,levels=regions28_EU)
  ENEMISCO2 <- mutate(ENEMISCO2, unit="Gt C")
  
  ENEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                              filename='ENEMISCH4.out', varname=NULL, 
                              collist=list(regions28,sector3,energy_carrier_emis), 
                              namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
  ENEMISCH4 <- subset(ENEMISCH4, region != "dummy")
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
  ENEMISN2O <- subset(ENEMISN2O, region != "dummy")
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
  INDEMISCO2 <- subset(INDEMISCO2, region != "dummy")
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
  INDEMISCH4 <- subset(INDEMISCH4, region != "dummy")
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
  INDEMISN2O <- subset(INDEMISN2O, region != "dummy")
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
  HFC_reg <- subset(HFC_reg, region != "dummy")
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
  PFC_reg <- subset(PFC_reg, region != "dummy")
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
  ElecProd <- subset(ElecProd, region != "dummy")
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
  
  # Electricity capacity
  ElecCap = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss/EPG", sep=""), 
                             filename='GCap.out', varname=NULL, 
                             collist=list(regions28,energy_technology), 
                             namecols=c('region','energy_technology'), novarname = TRUE)
  ElecCap <- subset(ElecCap, region != "dummy")
  EU <- inner_join(filter(ElecCap, region=='WEU'), filter(ElecCap, region=='CEU'), by=c("year", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecCap <- rbind(ElecCap, EU)
  ElecCap$region = factor(ElecCap$region,levels=regions28_EU)
  ElecCap$unit <- "MW"
  
  # TPES (Primary energy)
  TPES = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                         filename='tpes.out', varname=NULL, 
                         collist=list(regions28,energy_carrier_energy2), 
                         namecols=c('region','energy_carrier'), novarname = TRUE)
  TPES <- subset(TPES, region != "dummy")
  EU <- inner_join(filter(TPES, region=='WEU'), filter(TPES, region=='CEU'), by=c("year", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  TPES <- rbind(TPES, EU)
  TPES$region = factor(TPES$region,levels=regions28_EU)
  TPES <- mutate(TPES, unit="PJ")
  
  #Eprod
  EnergyProd = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                               filename='eprod.out', varname=NULL, 
                               collist=list(regions28,energy_carrier_energy2), 
                               namecols=c('region','energy_carrier'), novarname = TRUE)
  EnergyProd <- subset(EnergyProd, region != "dummy")
  EU <- inner_join(filter(EnergyProd, region=='WEU'), filter(EnergyProd, region=='CEU'), by=c("year", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  EnergyProd <- rbind(EnergyProd, EU)
  EnergyProd$region = factor(EnergyProd$region,levels=regions28_EU)
  EnergyProd <- mutate(EnergyProd, unit="PJ")
  
  # Electricity access
  ElecAcc = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                            filename='res_Elec_access.out', varname=NULL, 
                            collist=list(regions28,population_groups3), 
                            namecols=c('region','population_group'), novarname = TRUE)
  ElecAcc <- subset(ElecAcc, region != "dummy")
  EU <- inner_join(filter(ElecAcc, region=='WEU'), filter(ElecAcc, region=='CEU'), by=c("year", "population_group"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, population_group, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecAcc <- rbind(ElecAcc, EU)
  ElecAcc$region = factor(ElecAcc$region,levels=regions28_EU)
  ElecAcc$unit <- "fraction"
  
  #RSE
  FinalEnergy = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                                filename='rse.out', varname=NULL, 
                                collist=list(regions28,sector,energy_carrier_demand), 
                                namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
  FinalEnergy <- subset(FinalEnergy, region != "dummy")
  EU <- inner_join(filter(FinalEnergy, region=='WEU'), filter(FinalEnergy, region=='CEU'), by=c("year", "sector", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy <- rbind(FinalEnergy, EU)
  FinalEnergy$region = factor(FinalEnergy$region,levels=regions28_EU)
  FinalEnergy <- mutate(FinalEnergy, unit="PJ")
  
  # RESIDENTIAL
  
  #Residential Final Energy
  FinalEnergy_Residential = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                            filename='res_EU.out', varname=NULL, 
                                            collist=list(regions28,population_groups,res_enduse_functions), 
                                            namecols=c('region','population_group', 'enduse_function'), novarname = TRUE)
  FinalEnergy_Residential <- subset(FinalEnergy_Residential, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_Residential, region=='WEU'), filter(FinalEnergy_Residential, region=='CEU'), by=c("year", "population_group", "enduse_function"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, population_group, enduse_function, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_Residential <- rbind(FinalEnergy_Residential, EU)
  # add total
  FinalEnergy_Residential$population_group <- as.character(FinalEnergy_Residential$population_group)
  FinalEnergy_Residential$enduse_function <- as.character(FinalEnergy_Residential$enduse_function)
  FinalEnergy_Residential_tmp <- FinalEnergy_Residential %>% group_by(year, region, population_group) %>% summarise(value=sum(value))
  FinalEnergy_Residential_tmp <- ungroup(FinalEnergy_Residential_tmp)
  FinalEnergy_Residential_tmp <- mutate(FinalEnergy_Residential_tmp, enduse_function="Total")
  FinalEnergy_Residential_tmp <- select(FinalEnergy_Residential_tmp, year, region, population_group, enduse_function, value)
  FinalEnergy_Residential <- bind_rows(FinalEnergy_Residential, FinalEnergy_Residential_tmp)
  FinalEnergy_Residential$region = factor(FinalEnergy_Residential$region,levels=regions28_EU)
  FinalEnergy_Residential <- mutate(FinalEnergy_Residential, unit="GJ")
  
  # TRANSPORT
  TransportCO2Emissions = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                           filename='trp_trvl_CO2.out', varname=NULL, 
                                           collist=list(regions28,travel_mode), 
                                           namecols=c('region','travel_mode'), novarname = TRUE)
  TransportCO2Emissions <- subset(TransportCO2Emissions, region != "dummy")
  EU <- filter(TransportCO2Emissions, region %in% c('WEU', 'CEU'))
  EU <- spread(EU, key=region, value=value)
  EU <- mutate(EU, region="EU")
  EU <- mutate(EU, value=WEU+CEU)
  EU <- select(EU, year, region, travel_mode, value)
  TransportCO2Emissions <- rbind(TransportCO2Emissions, EU)
  
  # Person Kilometers Travelled (Tera km)
  PersonKilometers = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                     filename='trp_trvl_pkm.out', varname=NULL, 
                                     collist=list(regions28,travel_mode), 
                                     namecols=c('region','travel_mode'), novarname = TRUE)
  PersonKilometers <- subset(PersonKilometers, region != "dummy")
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
  FinalEnergy_Transport <- subset(FinalEnergy_Transport, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_Transport, region=='WEU'), filter(FinalEnergy_Transport, region=='CEU'), by=c("year", "travel_mode"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_Transport$region = factor(FinalEnergy_Transport$region,levels=regions28_EU)
  FinalEnergy_Transport <- mutate(FinalEnergy_Transport, unit="XJ")
  
  # Vehicle share cars
  VehicleShare_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                      filename='trp_trvl_Vshare_car.out', varname=NULL, 
                                      collist=list(regions27,car_type), 
                                      namecols=c('region','car_type'), novarname = TRUE)
  EU <- inner_join(filter(VehicleShare_cars, region=='WEU'), filter(VehicleShare_cars, region=='CEU'), by=c("year", "car_type"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, car_type, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  VehicleShare_cars <- rbind(VehicleShare_cars, EU)
  VehicleShare_cars$region = factor(VehicleShare_cars$region,levels=regions28_EU)
  VehicleShare_cars <- mutate(VehicleShare_cars, unit="%")

  # Vehicle share busses
  VehicleShare_busses = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                      filename='trp_trvl_Vshare_bus.out', varname=NULL, 
                                      collist=list(regions27,bus_type), 
                                      namecols=c('region','bus_type'), novarname = TRUE)
  EU <- inner_join(filter(VehicleShare_busses, region=='WEU'), filter(VehicleShare_busses, region=='CEU'), by=c("year", "bus_type"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, bus_type, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  VehicleShare_busses <- rbind(VehicleShare_busses, EU)
  VehicleShare_busses$region = factor(VehicleShare_busses$region,levels=regions28_EU)
  VehicleShare_busses <- mutate(VehicleShare_busses, unit="%")
  
  # Vehicle share trains
  VehicleShare_trains = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                      filename='trp_trvl_Vshare_train.out', varname=NULL, 
                                      collist=list(regions27,train_type), 
                                      namecols=c('region','train_type'), novarname = TRUE)
  EU <- inner_join(filter(VehicleShare_trains, region=='WEU'), filter(VehicleShare_trains, region=='CEU'), by=c("year", "train_type"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, train_type, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  VehicleShare_trains <- rbind(VehicleShare_trains, EU)
  VehicleShare_trains$region = factor(VehicleShare_trains$region,levels=regions28_EU)
  VehicleShare_trains <- mutate(VehicleShare_trains, unit="%")

  # Vehicle share aircrafts
  VehicleShare_aircrafts = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                                           filename='trp_trvl_Vshare_air.out', varname=NULL, 
                                           collist=list(regions27,aircraft_type), 
                                           namecols=c('region','aircraft_type'), novarname = TRUE)
  EU <- inner_join(filter(VehicleShare_aircrafts, region=='WEU'), filter(VehicleShare_aircrafts, region=='CEU'), by=c("year", "aircraft_type"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, aircraft_type, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  VehicleShare_aircrafts <- rbind(VehicleShare_aircrafts, EU)
  VehicleShare_aircrafts$region = factor(VehicleShare_aircrafts$region,levels=regions28_EU)
  VehicleShare_aircrafts <- mutate(VehicleShare_aircrafts, unit="%")
  
  #POP
  # Unit: million people
  POP = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                        filename='pop.scn', varname=NULL, 
                        collist=list(regions28), 
                        namecols=c('region'), novarname = TRUE)
  POP_keep = POP
  POP <- subset(POP, region != "dummy")
  EU <- inner_join(filter(POP, region=='WEU'), filter(POP, region=='CEU'), by=c("year"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  POP <- rbind(POP, EU)
  POP$region = factor(POP$region,labels=regions28_EU)
  POP <- mutate(POP, unit="million people")

  #GDP
  # Unit: milliln US(2005) dollar
  GDP_MER = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                            filename='gdptot.out', varname=NULL, 
                            collist=list(regions28), 
                            namecols=c('region'), novarname = TRUE)
  GDP_MER <- subset(GDP_MER, region != "dummy")
  EU <- inner_join(filter(GDP_MER, region=='WEU'), filter(GDP_MER, region=='CEU'), by=c("year"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  GDP_MER <- rbind(GDP_MER, EU)
  GDP_MER$region = factor(GDP_MER$region,labels=regions28_EU)
  GDP_MER <- mutate(GDP_MER, unit="million. US$(2005)")
  
  GDP_PPPpc = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                              filename='gpdppp.out', varname=NULL, 
                              collist=list(regions28), 
                              namecols=c('region'), novarname = TRUE)
  GDP_PPPpc <- subset(GDP_PPPpc, region != "dummy")
  EU <- inner_join(filter(GDP_PPPpc, region=='WEU'), filter(GDP_PPPpc, region=='CEU'), by=c("year"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  GDP_PPPpc <- rbind(GDP_PPPpc, EU)
  GDP_PPPpc$region = factor(GDP_PPPpc$region,labels=regions28_EU)
  GDP_PPPpc <- mutate(GDP_PPPpc, unit="US$(2005)/capita")
  
  GDP_PPP <- inner_join(POP, GDP_PPPpc, by=c("year", "region"))
  GDP_PPP <- mutate(GDP_PPP, value=value.x*value.y)
  GDP_PPP <- select(GDP_PPP, year, region, value)
  GDP_PPP <- mutate(GDP_PPP, unit="million US$(2005)")

  #IVA
  # Unit: million US(2005) dollar
  IVA = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/indicatoren", sep=""), 
                        filename='iva_pc.scn', varname=NULL, 
                        collist=list(regions28), 
                        namecols=c('region'), novarname = TRUE)
  IVA <- inner_join(POP_keep, IVA, by=c('year', 'region'))
  IVA <- mutate(IVA, value = value.x * value.y)
  IVA <- select(IVA, year, region, value)
  IVA <- subset(IVA, region != "dummy")
  EU <- inner_join(filter(IVA, region=='WEU'), filter(IVA, region=='CEU'), by=c("year"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  IVA <- rbind(IVA, EU)
  IVA$region = factor(IVA$region,labels=regions28_EU)
  IVA <- mutate(IVA, unit="thousand US$(2005)")
  
  FloorSpace = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/tuss", sep=""), 
                               filename='res_FloorSpace.out', varname=NULL, 
                               collist=list(regions27,population_groups), 
                               namecols=c('region','population_group'), novarname = TRUE)
  # translate from floorspace per capita to total floorspace using population data
  FloorSpace = filter(FloorSpace, population_group=="Total")
  POP_keep$region = factor(POP_keep$region, levels=regions28_EU)
  FloorSpace$region = factor(FloorSpace$region, levels=regions28_EU)
  FloorSpace <- inner_join(POP_keep, FloorSpace, by=c('year', 'region'))
  FloorSpace <- mutate(FloorSpace, value = value.x * value.y)
  FloorSpace <- select(FloorSpace, year, region, value)
  # add world total
  FloorSpace <- subset(FloorSpace, region != "World")
  FloorSpace_world <- group_by(FloorSpace, year) %>% summarise(value=sum(value))
  FloorSpace_world <- mutate(FloorSpace_world, region="World")
  FloorSpace <- rbind(FloorSpace, FloorSpace_world)
  EU <- inner_join(filter(FloorSpace, region=='WEU'), filter(FloorSpace, region=='CEU'), by=c("year"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FloorSpace <- rbind(FloorSpace, EU)
  FloorSpace$region <- factor(FloorSpace$region, levels=regions28_EU)
  FloorSpace <- mutate(FloorSpace, unit="m2")
  
  #4.
  l <- list(CO2Spec=CO2Spec,ENEMISCO2=ENEMISCO2,ENEMISCH4=ENEMISCH4,ENEMISN2O=ENEMISN2O,INDEMISCO2=INDEMISCO2,
            INDEMISCH4=INDEMISCH4,INDEMISN2O=INDEMISN2O,HFC_reg=HFC_reg,PFC_reg=PFC_reg,
            LUEMCO2=LUEMCO2,LUEMCH4=LUEMCH4,LUEMN2O=LUEMN2O,
            ElecProd=ElecProd, ElecCap=ElecCap, EnergyProd=EnergyProd, FinalEnergy=FinalEnergy, 
            FinalEnergy_Residential=FinalEnergy_Residential, ElecAcc=ElecAcc,
            TransportCO2Emissions=TransportCO2Emissions, PersonKilometers=PersonKilometers, FinalEnergy_Transport=FinalEnergy_Transport,
            VehicleShare_cars=VehicleShare_cars, VehicleShare_busses=VehicleShare_busses, VehicleShare_trains=VehicleShare_trains, VehicleShare_aircrafts=VehicleShare_aircrafts,
            TPES=TPES,
            POP=POP, GDP_MER=GDP_MER, GDP_PPP=GDP_PPP, IVA=IVA, FloorSpace=FloorSpace)
  
  
}
