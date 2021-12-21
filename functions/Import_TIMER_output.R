#pre: Make sure you are in the 1-click environment R-scripts directory
#     e.g. Y:\ontwapps\Timer\Users\Mark\CD_LINKSupdate\6_R
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

# Version
# TIMER_version = 'TIMER_2015'
# TIMER_version = 'TIMER_3_11'

library(data.table)
library(tidyverse)
ImportTimerScenario <- function(TIMER_scenario = 'SSP2', TIMER_version = 'TIMER_2015', IMAGE_scenario = 'SSP2', Rundir, 
                                Project, TIMERGeneration, RDir, Projectoutputlib, TIMERFunctionsDir="Check_targets", Policy = FALSE)
{ source(paste(Rundir, Project, RDir, TIMERFunctionsDir, '/functions', 'mym2r.R', sep='/'))
  source(paste(Rundir, Project, RDir, TIMERFunctionsDir, '/functions', 'Settings.R', sep='/'))

  TIMER_folder = paste(Rundir, Project, "2_TIMER/outputlib", TIMERGeneration, Projectoutputlib, sep="/")
  if(!dir.exists(paste(TIMER_folder, TIMER_scenario, sep="/")))
  { print(paste("The TIMER scenario ", TIMER_scenario, " in the folder", TIMER_folder, " is not recognised", sep=""))
    stop()
  }
  print(TIMER_folder) 
  
  IMAGE_2015_folder = try({paste(Rundir, Project, "3_IMAGE/Scenario_lib/scen", sep="/")})
  IMAGE_3_11_folder = try({paste(Rundir, Project, "3_IMAGE_land/Scenario_lib/scen", sep="/")})
  
  IMAGE_folder = IMAGE_3_11_folder
  if(!dir.exists(paste(IMAGE_2015_folder, IMAGE_scenario, sep="/")))
  { IMAGE_folder = IMAGE_3_11_folder
    if(!dir.exists(paste(IMAGE_3_11_folder, IMAGE_scenario, sep="/")))
    {  print(paste("The IMAGE scenario ", IMAGE_scenario, " in the folder", IMAGE_folder, " is not recognised", sep=""))
       IMAGE_folder = ""
       stop()
    }
  }
  print(IMAGE_folder)
  
  # Initiliaze parameters
  if (TIMER_version %in% c('TIMER_3_11','TIMER_3_2'))
  { energy_technology=energy_technology_3_2
    energy_technology_28=energy_technology_30_3_2
    energy_technology_20=energy_technology_22_3_2
    sector_capture=sector_capture_3_2
    energy_carrier_ren_28=energy_carrier_ren_30_3_2
    energy_carrier_nf_28=energy_carrier_nf_30_3_2
    energy_technology_ren=energy_technology_ren_3_2
    energy_technology_solar=energy_technology_solar_3_2
  }
  else
    {energy_technology=energy_technology_2015
     energy_technology_28=energy_technology_28_2015
     energy_technology_20=energy_technology_20_2015
     sector_capture=sector_capture_2015
     energy_carrier_ren_28=energy_carrier_ren_28_2015
     energy_carrier_nf_28=energy_carrier_nf_28_2015
     energy_technology_ren=energy_technology_ren_2015
     energy_technology_solar=energy_technology_solar_2015
    }
  
  #1.
  
  # GHG EMISSIONS
  #2, 3.
  # prepare correct labels for mym file dimensions
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/tuss"}
  CO2Spec = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}
  ENEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                              filename='ENEMISCO2.out', varname=NULL, 
                              collist=list(regions28,sector3,energy_carrier_emis), 
                              namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
  ENEMISCO2 <- subset(ENEMISCO2, region != "dummy")
  EU <- inner_join(filter(ENEMISCO2, region=='WEU'), filter(ENEMISCO2, region=='CEU'), by=c("year", "sector", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ENEMISCO2 <- bind_rows(ENEMISCO2, EU)
  ENEMISCO2$region = factor(ENEMISCO2$region,levels=regions28_EU)
  ENEMISCO2 <- mutate(ENEMISCO2, unit="Gt C")

  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}  
  ENEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}
  ENEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}
  INDEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}
  INDEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}
  INDEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}
  HFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis"} else{data_dir="/indicatoren"}
  PFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version == 'TIMER_3_2') {dir="I2RT";luemco2_source=land_use_source_CO2_3_2} else{dir="output/emissions";luemco2_source=land_use_source_CO2} 
  LUEMCO2 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, "/", dir, sep=""), 
                            filename='LUEMCO2.out', varname=NULL, 
                            collist=list(luemco2_source, regions27), 
                            namecols=c('source','region'), novarname = TRUE)
  EU <- inner_join(filter(LUEMCO2, region=='WEU'), filter(LUEMCO2, region=='CEU'), by=c("year", "source"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  LUEMCO2 <- rbind(LUEMCO2, EU)
  LUEMCO2$region = factor(LUEMCO2$region,levels=regions28_EU)
  LUEMCO2 <- mutate(LUEMCO2, unit="Gt C")
  
  if(TIMER_version == 'TIMER_3_2') {dir="I2RT";luemch4_source=land_use_source_CH4_3_2} else{dir="output/emissions";luemch4_source=land_use_source_CH4} 
  if(TIMER_scenario%in%c("SSP2_SPA0_19I_D", "SSP2_SPA0_26I_D")) {luemch4_source=land_use_source_CH4}
  LUEMCH4 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, "/", dir, sep=""), 
                            filename='LUEMCH4.out', varname=NULL, 
                            collist=list(luemch4_source, regions27), 
                            namecols=c('source','region'), novarname = TRUE)
  EU <- inner_join(filter(LUEMCH4, region=='WEU'), filter(LUEMCH4, region=='CEU'), by=c("year", "source"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  LUEMCH4 <- rbind(LUEMCH4, EU)
  LUEMCH4$region = factor(LUEMCH4$region,levels=regions28_EU)
  LUEMCH4 <- mutate(LUEMCH4, unit="Mt")

  if(TIMER_version == 'TIMER_3_2') {dir="I2RT";luemn20_source=land_use_source_N2O_3_2} else{dir="output/emissions";luemn20_source=land_use_source_N2O} 
  if(TIMER_scenario%in%c("SSP2_SPA0_19I_D", "SSP2_SPA0_26I_D")) {luemn20_source=land_use_source_N2O}
  LUEMN2O = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, "/", dir, sep=""), 
                            filename='LUEMN2O.out', varname=NULL, 
                            collist=list(luemn20_source, regions27), 
                            namecols=c('source','region'), novarname = TRUE)
  EU <- inner_join(filter(LUEMN2O, region=='WEU'), filter(LUEMN2O, region=='CEU'), by=c("year", "source"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  LUEMN2O <- rbind(LUEMN2O, EU)
  LUEMN2O$region = factor(LUEMN2O$region,levels=regions28_EU)
  LUEMN2O <- mutate(LUEMN2O, unit="Mt N")
  
  # ENERGY SUPPLY
  # Electricity
  # CO2 emissions from electricity and heat production
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"}  
  ElecHeatCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                            filename='ElecHeatCO2.out', varname=NULL, 
                            collist=list(regions28,sector_energy_supply,energy_technology_energy_supply), 
                            namecols=c('region','energy_supply_sector','energy_technology'), novarname = TRUE)
  ElecHeatCO2 <- subset(ElecHeatCO2, region != "dummy")
  EU <- inner_join(filter(ElecHeatCO2, region=='WEU'), filter(ElecHeatCO2, region=='CEU'), by=c("year", "energy_supply_sector", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_supply_sector, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecHeatCO2 <- rbind(ElecHeatCO2, EU)
  ElecHeatCO2$region = factor(ElecHeatCO2$region,levels=regions28_EU)
  ElecHeatCO2$unit <- "Gt C"
  
  #TODO use newer ElecProd file from new TIMER version, adjust dimensions etc. accordingly
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/policy"} else{data_dir="/indicatoren"}  
  ElecProd <- NULL
  try({ElecProd = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  })
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"}  
  ElecProdSpec = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                             filename='ElecProdSpec.out', varname=NULL, 
                             collist=list(regions28,energy_technology_28), 
                             namecols=c('region','energy_carrier'), novarname = TRUE)
  ElecProdSpec <- subset(ElecProdSpec, region != "dummy")
  EU <- inner_join(filter(ElecProdSpec, region=='WEU'), filter(ElecProdSpec, region=='CEU'), by=c("year", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecProdSpec <- rbind(ElecProdSpec, EU)
  ElecProdSpec$region = factor(ElecProdSpec$region,levels=regions28_EU)
  # Add total to ElecProd, and make sure order is the same as original file
  # 1. First add total column to table
  # 2. Then change order of rows to match original sort order of TIMER file
  ElecProdSpec$energy_carrier <- as.character(ElecProdSpec$energy_carrier)
  ElecProdSpec_tmp <- ElecProdSpec %>% group_by(year, region) %>% summarise(value=sum(value))
  ElecProdSpec_tmp <- ungroup(ElecProdSpec_tmp)
  ElecProdSpec_tmp <- mutate(ElecProdSpec_tmp, energy_carrier="Total")
  ElecProdSpec_tmp <- select(ElecProdSpec_tmp, year, region, energy_carrier, value)
  ElecProdSpec <- bind_rows(ElecProdSpec, ElecProdSpec_tmp)
  ElecProdSpec <- mutate(ElecProdSpec, region_nr=match(region, regions28_EU))
  ElecProdSpec <- mutate(ElecProdSpec, energy_carrier_nr=match(energy_carrier, energy_carrier))
  ElecProdSpec <- select(ElecProdSpec, year, region_nr, energy_carrier_nr, everything())
  ElecProdSpec <- arrange(ElecProdSpec, year, region_nr, energy_carrier_nr)
  ElecProdSpec$energy_carrier <- as.factor(ElecProdSpec$energy_carrier)
  ElecProdSpec$region_nr <- NULL
  ElecProdSpec$energy_carrier_nr <- NULL
  ElecProdSpec <- mutate(ElecProdSpec, unit="GJ")
  
  # New Electricity capacity
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"}  
  ElecCap_new = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                filename='GCapNew.out', varname=NULL, 
                                collist=list(regions28,energy_technology_28), 
                                namecols=c('region','energy_technology'), novarname = TRUE)
  ElecCap_new <- subset(ElecCap_new, region != "dummy")
  EU <- inner_join(filter(ElecCap_new, region=='WEU'), filter(ElecCap_new, region=='CEU'), by=c("year", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecCap_new <- rbind(ElecCap_new, EU)
  ElecCap_new$region = factor(ElecCap_new$region,levels=regions28_EU)
  ElecCap_new$unit <- "MW"
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"}  
  # Electricity capacity
  ElecCap = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  # Primary energy electricity production
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"}  
  ElecFuelUseTot = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                            filename='ElecFuelUseTot.out', varname=NULL, 
                            collist=list(regions28,energy_technology_14), 
                            namecols=c('region','energy_technology'), novarname = TRUE)
  ElecFuelUseTot <- subset(ElecFuelUseTot, region != "dummy")
  EU <- inner_join(filter(ElecFuelUseTot, region=='WEU'), filter(ElecFuelUseTot, region=='CEU'), by=c("year", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecFuelUseTot <- rbind(ElecFuelUseTot, EU)
  ElecFuelUseTot$region = factor(ElecFuelUseTot$region,levels=regions28_EU)
  ElecFuelUseTot$unit <- "GJ"

  # CO2 standard new power plants
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"} 
  CO2EPG_new = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                            filename='CO2EPGSpecNew.out', varname=NULL, 
                            collist=list(regions26,energy_technology_20), 
                            namecols=c('region','energy_technology'), novarname = TRUE)
  CO2EPG_new <- subset(CO2EPG_new, region != "dummy")
  EU <- inner_join(filter(CO2EPG_new, region=='WEU'), filter(CO2EPG_new, region=='CEU'), by=c("year", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x+value.y)/2) %>% select(year, region, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  CO2EPG_new <- rbind(CO2EPG_new, EU)
  CO2EPG_new$region = factor(CO2EPG_new$region,levels=regions28_EU)
  CO2EPG_new$unit <- "gCO2/kWhe"
  
  # CO2 standard existing power plants
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"} 
  CO2EPG = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                           filename='CO2EPGSpecAvg.out', varname=NULL, 
                           collist=list(regions26,energy_technology_20), 
                           namecols=c('region','energy_technology'), novarname = TRUE)
  CO2EPG <- subset(CO2EPG, region != "dummy")
  EU <- inner_join(filter(CO2EPG, region=='WEU'), filter(CO2EPG, region=='CEU'), by=c("year", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x+value.y)/2) %>% select(year, region, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  CO2EPG <- rbind(CO2EPG, EU)
  CO2EPG$region = factor(CO2EPG$region,levels=regions28_EU)
  CO2EPG$unit <- "gCO2/kWhe"
  
  # Add total CO2-intensity per region
  if (Policy==TRUE)
  { CO2_tmp <- filter(ElecHeatCO2, energy_supply_sector=="Electricity", energy_technology=="Total") %>%
    select(-energy_supply_sector, -energy_technology, -unit)
  ElecProd_tmp <- filter(ElecProd, energy_carrier=="Total") %>%
    select(-energy_carrier, -unit)
    CO2EPG_total <- inner_join(CO2_tmp, ElecProd_tmp, by=c('year', 'region')) %>%
    mutate(value=10^15/(2.78*10^8)*value.x*CToCO2/value.y, energy_technology="Total", unit="gCO2/KWhe") %>% #GtCO2/PJ *(10^15/2.78*10^11)--> gCO2/KWh
    select(-value.x, -value.y) %>%
    select(year, region, energy_technology, value, unit)
  
  CO2EPG$energy_technology <- factor(CO2EPG$energy_technology, levels=energy_technology)
  CO2EPG_total$energy_technology <- factor(CO2EPG_total$energy_technology, levels=energy_technology)
  CO2EPG <- rbind(CO2EPG, CO2EPG_total) %>%
            arrange(year, region, energy_technology, unit)
  }
  
  # Efficiency new power plants
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"}  
  ElecEffPct_new = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                            filename='ElecEffNew.out', varname=NULL, 
                            collist=list(regions27,energy_technology_28), 
                            namecols=c('region','energy_technology'), novarname = TRUE)
  ElecEffPct_new <- subset(ElecEffPct_new, region != "dummy")
  # TO DO: for EU weighting needs to be applied with new capacity
  EU <- inner_join(filter(ElecEffPct_new, region=='WEU'), filter(ElecEffPct_new, region=='CEU'), by=c("year", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x+value.y)/2) %>% select(year, region, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecEffPct_new <- rbind(ElecEffPct_new, EU)
  ElecEffPct_new$region = factor(ElecEffPct_new$region,levels=regions28_EU)
  ElecEffPct_new$unit <- "%"
 
  # Efficiency power plants
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/enepg"} else{data_dir="/tuss/EPG"}  
  ElecEffPct = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                               filename='ElecEffAvg.out', varname=NULL, 
                               collist=list(regions26,energy_technology_28), 
                               namecols=c('region','energy_technology'), novarname = TRUE)
  ElecEffPct <- subset(ElecEffPct, region != "dummy")
  EU <- inner_join(filter(ElecEffPct, region=='WEU'), filter(ElecEffPct, region=='CEU'), by=c("year", "energy_technology"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x+value.y)/2) %>% select(year, region, energy_technology, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecEffPct <- rbind(ElecEffPct, EU)
  ElecEffPct$region = factor(ElecEffPct$region,levels=regions28_EU)
  ElecEffPct$unit <- "%"
  
  # Hydrogen
  # Fuel demand for hydrogen production (only exists in TIMER_2015, not in TIMER_3_11)
  # NOT IN TIMER_3_11
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = NA} else{data_dir="/tuss/hydrogen"} 
  H2FuelDem <- NULL
  tryCatch({H2FuelDem = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                              filename='FuelDem.out', varname=NULL, 
                              collist=list(regions28,energy_carrier_hydrogen), 
                              namecols=c('region','energy_carrier'), novarname = TRUE)
  H2FuelDem <- subset(H2FuelDem, region != "dummy")
  EU <- inner_join(filter(H2FuelDem, region=='WEU'), filter(H2FuelDem, region=='CEU'), by=c("year", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  H2FuelDem <- rbind(H2FuelDem, EU)
  H2FuelDem$region = factor(H2FuelDem$region,levels=regions28_EU)
  # Add total to ElecProd, and make sure order is the same as original file
  # 1. First add total column to table
  # 2. Then change order of rows to match original sort order of TIMER file
  H2FuelDem$energy_carrier <- as.character(H2FuelDem$energy_carrier)
  H2FuelDem_tmp <- H2FuelDem %>% group_by(year, region) %>% summarise(value=sum(value))
  H2FuelDem_tmp <- ungroup(H2FuelDem_tmp)
  H2FuelDem_tmp <- mutate(H2FuelDem_tmp, energy_carrier="Total")
  H2FuelDem_tmp <- select(H2FuelDem_tmp, year, region, energy_carrier, value)
  H2FuelDem <- bind_rows(H2FuelDem, H2FuelDem_tmp)
  H2FuelDem <- mutate(H2FuelDem, region_nr=match(region, regions28_EU))
  H2FuelDem <- mutate(H2FuelDem, energy_carrier_nr=match(energy_carrier, energy_carrier))
  H2FuelDem <- select(H2FuelDem, year, region_nr, energy_carrier_nr, everything())
  H2FuelDem <- arrange(H2FuelDem, year, region_nr, energy_carrier_nr)
  H2FuelDem$energy_carrier <- as.factor(H2FuelDem$energy_carrier)
  H2FuelDem$region_nr <- NULL
  H2FuelDem$energy_carrier_nr <- NULL
  H2FuelDem <- mutate(H2FuelDem, unit="GJ")
  }, # try
  warning = function(warning_condition)
  { cat("The file FuelDem.out does not exist in TIMER_3_11\n")
  },
  error = function(error_condition) 
  { cat("The file FuelDem.out does not exist in TIMER_3_11\n")
  }) # trycatch
  
  # Total H2 demand per region
  # NOT IN TIMER_3_11
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = NA} else{data_dir="/tuss/hydrogen"} 
  tryCatch({H2Prod = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                           filename='H2Dem.out', varname=NULL, 
                           collist=list(regions28,sector_hydrogen), 
                           namecols=c('region','sector'), novarname = TRUE)
  H2Prod <- subset(H2Prod, region != "dummy")
  EU <- inner_join(filter(H2Prod, region=='WEU'), filter(H2Prod, region=='CEU'), by=c("year", "sector"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  H2Prod <- rbind(H2Prod, EU)
  H2Prod$region = factor(H2Prod$region,levels=regions28_EU)
  }, # try
  warning = function(warning_condition)
  { cat("The file H2Dem.out does not exist in TIMER_3_11\n")
  },
  error = function(error_condition) 
  { cat("The file H2Dem.out does not exist in TIMER_3_11\n")
  }) # trycatch
  
  # Total H2 production per carrier
  H2Prod <- NULL
  if (Policy==TRUE)  {
  tryCatch({
  H2Prod = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                              filename='HProdAll.out', varname=NULL, 
                              collist=list(regions28,energy_carrier_hydrogen2), 
                              namecols=c('region','energy_carrier'), novarname = TRUE)
  H2Prod <- subset(H2Prod, region != "dummy")
  EU <- inner_join(filter(H2Prod, region=='WEU'), filter(H2Prod, region=='CEU'), by=c("year", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  H2Prod <- rbind(H2Prod, EU)
  H2Prod$region = factor(H2Prod$region,levels=regions28_EU)
  # Add total to ElecProd, and make sure order is the same as original file
  # 1. First add total column to table
  # 2. Then change order of rows to match original sort order of TIMER file
  H2Prod$energy_carrier <- as.character(H2Prod$energy_carrier)
  H2Prod_tmp <- H2Prod %>% group_by(year, region) %>% summarise(value=sum(value))
  H2Prod_tmp <- ungroup(H2Prod_tmp)
  H2Prod_tmp <- mutate(H2Prod_tmp, energy_carrier="Total")
  H2Prod_tmp <- select(H2Prod_tmp, year, region, energy_carrier, value)
  H2Prod <- bind_rows(H2Prod, H2Prod_tmp)
  H2Prod <- mutate(H2Prod, region_nr=match(region, regions28_EU))
  H2Prod <- mutate(H2Prod, energy_carrier_nr=match(energy_carrier, energy_carrier))
  H2Prod <- select(H2Prod, year, region_nr, energy_carrier_nr, everything())
  H2Prod <- arrange(H2Prod, year, region_nr, energy_carrier_nr)
  H2Prod$energy_carrier <- as.factor(H2Prod$energy_carrier)
  H2Prod$region_nr <- NULL
  H2Prod$energy_carrier_nr <- NULL
  H2Prod <- mutate(H2Prod, unit="GJ")
  },
  error = function(error_condition) 
  { cat("The file policy/HProdAll.out does not exist\n")
  }) # try
  } # if
  else {
    H2Prod <- NULL  
  }
  
  # TPES (Primary energy)
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss"} else{data_dir="/indicatoren"} 
  TPES = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  # Trade
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss"} else{data_dir="/indicatoren"} 
  NetTrade = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                         filename='nettrade.out', varname=NULL, 
                         collist=list(regions28,energy_carrier_trade), 
                         namecols=c('region','energy_carrier'), novarname = TRUE)
  NetTrade <- subset(NetTrade, region != "dummy")
  EU <- inner_join(filter(NetTrade, region=='WEU'), filter(NetTrade, region=='CEU'), by=c("year", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  NetTrade <- rbind(NetTrade, EU)
  NetTrade$region = factor(NetTrade$region,levels=regions28_EU)
  NetTrade <- mutate(NetTrade, unit="PJ")
  
  #Eprod
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss"} else{data_dir="/indicatoren"} 
  EnergyProd = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  # Captured CO2
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/emis";sec_cap=sector_capture_3_2} else{data_dir="/T2RT";sec_cap=sector_capture_2015} 
  CarbonCaptured = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                               filename='CarbonCapturedSpec2.out', varname=NULL, 
                               collist=list(regions28, sec_cap, energy_carrier_capture), 
                               namecols=c('region','sector', 'energy_carrier'), novarname = TRUE)
  CarbonCaptured <- subset(CarbonCaptured, region != "dummy")
  EU <- inner_join(filter(CarbonCaptured, region=='WEU'), filter(CarbonCaptured, region=='CEU'), by=c("year", "sector", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, sector, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  CarbonCaptured <- rbind(CarbonCaptured, EU)
  CarbonCaptured$region = factor(CarbonCaptured$region,levels=regions28_EU)
  CarbonCaptured <- mutate(CarbonCaptured, unit="kgC")
  
  # HEAT
  HeatProduction <- NULL
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/calib"} #else{} 
  tryCatch({HeatProduction = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                   filename='CalHeatProd.out', varname=NULL, 
                                   collist=list(regions28, heat_calibration, energy_carrier_heat), 
                                   namecols=c('region','source', 'energy_carrier'), novarname = TRUE)
  HeatProduction <- subset(HeatProduction, region != "dummy")
  EU <- inner_join(filter(HeatProduction, region=='WEU'), filter(HeatProduction, region=='CEU'), by=c("year", "source", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  HeatProduction <- rbind(HeatProduction, EU)
  HeatProduction$region = factor(HeatProduction$region,levels=regions28_EU)
  HeatProduction <- mutate(HeatProduction, unit="GJ")
  },
  error = function(error_condition) 
  { cat("The file policy/CalHeatProd.out does not exist\n")
  }) # try
  
  HeatDemand <- NULL
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/calib"} else{data_dir="/T2RT"} 
  tryCatch({HeatDemand = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                   filename='CalHeatDem.out', varname=NULL, 
                                   collist=list(regions28, heat_calibration), 
                                   namecols=c('region','source'), novarname = TRUE)
  HeatDemand <- subset(HeatDemand, region != "dummy")
  EU <- inner_join(filter(HeatDemand, region=='WEU'), filter(HeatDemand, region=='CEU'), by=c("year", "source"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, source, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  HeatDemand <- rbind(HeatDemand, EU)
  HeatDemand$region = factor(HeatDemand$region,levels=regions28_EU)
  HeatDemand <- mutate(HeatDemand, unit="GJ")
  },
  error = function(error_condition) 
  { cat("The file policy/CalHeatDem.out does not exist\n")
  }) # try

  # SDGs
  
  # Electricity access
  if(TIMER_version == 'TIMER_3_2') {data_dir = "/tuss/endem/residential"} else{data_dir="/tuss"}
  tryCatch({ElecAcc = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                            filename='res_Elec_access.out', varname=NULL, 
                            collist=list(regions28,population_groups3), 
                            namecols=c('region','population_group'), novarname = TRUE)
  ElecAcc <- subset(ElecAcc, region != "dummy")
  #EU <- inner_join(filter(ElecAcc, region=='WEU'), filter(ElecAcc, region=='CEU'), by=c("year", "population_group"))
  #EU$region <- "EU"
  #EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, population_group, value)
  EU_ElecAcc <- inner_join(filter(ElecAcc, region=='WEU'), filter(ElecAcc, region=='CEU'), by=c("year", "population_group"))
  ElecProd_Total <- filter(ElecProd, energy_carrier=='Total') %>% select(year, region, value)
  EU_ElecProd_Total <- inner_join(filter(ElecProd_Total, region=='WEU'), filter(ElecProd_Total, region=='CEU'), by=c("year"))
  EU <- inner_join(EU_ElecAcc, EU_ElecProd_Total, by=c('year'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, population_group, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElecAcc <- rbind(ElecAcc, EU)
  ElecAcc$region = factor(ElecAcc$region,levels=regions28_EU)
  ElecAcc$unit <- "%"
  },
  error = function(error_condition) 
  { cat("Error in variable ElecAcc\n")
  }) # try
  
  #RSE
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem"} else{data_dir="/indicatoren"}
  FinalEnergy = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  #Residential Final Energy per end use function
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/residential"} else{data_dir="/tuss"}
  FinalEnergy_Residential = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                            filename='res_EU.out', varname=NULL, 
                                            collist=list(regions28,population_groups, res_enduse_functions), 
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
  
  #Residential Final Energy per energy carrier
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/residential"} else{data_dir="/tuss"}
  FinalEnergy_Residential_energy_carrier = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                                    filename='res_EnergyUse.out', varname=NULL, 
                                                    collist=list(regions28,population_groups, energy_carrier_residential), 
                                                    namecols=c('region','population_group', 'energy_carrier'), novarname = TRUE)
  FinalEnergy_Residential_energy_carrier <- subset(FinalEnergy_Residential_energy_carrier, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_Residential_energy_carrier, region=='WEU'), filter(FinalEnergy_Residential_energy_carrier, region=='CEU'), by=c("year", "population_group", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, population_group, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_Residential_energy_carrier <- rbind(FinalEnergy_Residential_energy_carrier, EU)
  # add total
  FinalEnergy_Residential_energy_carrier$population_group <- as.character(FinalEnergy_Residential_energy_carrier$population_group)
  FinalEnergy_Residential_energy_carrier$energy_carrier <- as.character(FinalEnergy_Residential_energy_carrier$energy_carrier)
  FinalEnergy_Residential_energy_carrier_tmp <- FinalEnergy_Residential_energy_carrier %>% group_by(year, region, population_group) %>% summarise(value=sum(value))
  FinalEnergy_Residential_energy_carrier_tmp <- ungroup(FinalEnergy_Residential_energy_carrier_tmp)
  FinalEnergy_Residential_energy_carrier_tmp <- mutate(FinalEnergy_Residential_energy_carrier_tmp, energy_carrier="Total")
  FinalEnergy_Residential_energy_carrier_tmp <- select(FinalEnergy_Residential_energy_carrier_tmp, year, region, population_group, energy_carrier, value)
  FinalEnergy_Residential_energy_carrier <- bind_rows(FinalEnergy_Residential_energy_carrier, FinalEnergy_Residential_energy_carrier_tmp)
  FinalEnergy_Residential_energy_carrier$region = factor(FinalEnergy_Residential_energy_carrier$region,levels=regions28_EU)
  FinalEnergy_Residential_energy_carrier <- mutate(FinalEnergy_Residential_energy_carrier, unit="GJ")
  
  #Residential Final Energy per end use function
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/residential"} else{data_dir="/tuss"}
  FinalEnergy_Residential_Appliances = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                            filename='res_applianceEU.out', varname=NULL, 
                                            collist=list(regions27,population_groups, res_appliances), 
                                            namecols=c('region','population_group', 'appliance'), novarname = TRUE)
  FinalEnergy_Residential_Appliances <- subset(FinalEnergy_Residential_Appliances, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_Residential_Appliances, region=='WEU'), filter(FinalEnergy_Residential_Appliances, region=='CEU'), by=c("year", "population_group", "appliance"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, population_group, appliance, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_Residential_Appliances <- rbind(FinalEnergy_Residential_Appliances, EU)
  # add total
  FinalEnergy_Residential_Appliances$population_group <- as.character(FinalEnergy_Residential_Appliances$population_group)
  FinalEnergy_Residential_Appliances$appliance <- as.character(FinalEnergy_Residential_Appliances$appliance)
  FinalEnergy_Residential_Appliances_tmp <- FinalEnergy_Residential_Appliances %>% group_by(year, region, population_group) %>% summarise(value=sum(value))
  FinalEnergy_Residential_Appliances_tmp <- ungroup(FinalEnergy_Residential_Appliances_tmp)
  FinalEnergy_Residential_Appliances_tmp <- mutate(FinalEnergy_Residential_Appliances_tmp, appliance="Total")
  FinalEnergy_Residential_Appliances_tmp <- select(FinalEnergy_Residential_Appliances_tmp, year, region, population_group, appliance, value)
  FinalEnergy_Residential_Appliances <- bind_rows(FinalEnergy_Residential_Appliances, FinalEnergy_Residential_Appliances_tmp)
  FinalEnergy_Residential_Appliances$region = factor(FinalEnergy_Residential_Appliances$region,levels=regions28_EU)
  FinalEnergy_Residential_Appliances <- mutate(FinalEnergy_Residential_Appliances, unit="GJ")
  
  
  
  # TRANSPORT
  # CO2 emissions travel
  # TODO: Re-add CO2 emissions in travel/freight modules
  TransportTravelCO2Emissions <- NULL
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = ""} else{data_dir="/tuss"}
  tryCatch({TransportTravelCO2Emissions = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                           filename='trp_trvl_CO2.out', varname=NULL, 
                                           collist=list(regions28,travel_mode_travel), 
                                           namecols=c('region','travel_mode'), novarname = TRUE)
  TransportTravelCO2Emissions <- subset(TransportTravelCO2Emissions, region != "dummy")
  EU <- filter(TransportTravelCO2Emissions, region %in% c('WEU', 'CEU'))
  EU <- spread(EU, key=region, value=value)
  EU <- mutate(EU, region="EU")
  EU <- mutate(EU, value=WEU+CEU)
  EU <- select(EU, year, region, travel_mode, value)
  TransportTravelCO2Emissions <- rbind(TransportTravelCO2Emissions, EU)
  TransportTravelCO2Emissions$unit <- "MtCO2"
  }, # try
  warning = function(warning_condition)
  { cat("The file trp_trvl_CO2.out does not exist in TIMER_3_11\n")
  },
  error = function(error_condition) 
  { cat("The file trp_trvl_CO2.out does not exist in TIMER_3_11\n")
  }) # trycatch
 
  # CO2 emissions freight 
  # TODO: Re-add CO2 emissions in travel/freight modules
  TransportFreightCO2Emissions <- NULL
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = ""} else{data_dir="/tuss"}
  tryCatch({TransportFreightCO2Emissions = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                           filename='trp_frgt_CO2.out', varname=NULL, 
                                           collist=list(regions28,travel_mode_freight), 
                                           namecols=c('region','travel_mode'), novarname = TRUE)
  TransportFreightCO2Emissions <- subset(TransportFreightCO2Emissions, region != "dummy")
  EU <- filter(TransportFreightCO2Emissions, region %in% c('WEU', 'CEU'))
  EU <- spread(EU, key=region, value=value)
  EU <- mutate(EU, region="EU")
  EU <- mutate(EU, value=WEU+CEU)
  EU <- select(EU, year, region, travel_mode, value)
  TransportFreightCO2Emissions <- rbind(TransportFreightCO2Emissions, EU)
  TransportFreightCO2Emissions <- mutate(TransportFreightCO2Emissions, unit="MtCO2")
  }, # try
  warning = function(warning_condition)
  { cat("The file trp_frgt_CO2.out does not exist in TIMER_3_11\n")
  },
  error = function(error_condition) 
  { cat("The file trp_frgt_CO2.out does not exist in TIMER_3_11\n")
  }) # trycatch
  
  # Energy use travel fuels per mode
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  FinalEnergy_trvl_Transport = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                          filename='trp_trvl_Energy.out', varname=NULL, 
                                          collist=list(regions28,travel_mode_travel), 
                                          namecols=c('region','travel_mode'), novarname = TRUE)
  FinalEnergy_trvl_Transport <- subset(FinalEnergy_trvl_Transport, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_trvl_Transport, region=='WEU'), filter(FinalEnergy_trvl_Transport, region=='CEU'), by=c("year", "travel_mode"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_trvl_Transport <- rbind(FinalEnergy_trvl_Transport, EU)
  FinalEnergy_trvl_Transport$region = factor(FinalEnergy_trvl_Transport$region,levels=regions28_EU)
  FinalEnergy_trvl_Transport <- mutate(FinalEnergy_trvl_Transport, unit="XJ")

  # Energy use travel fuels per mode and per energy carrier
  FinalEnergy_carrier_trvl_Transport <- NULL
  if (Policy==TRUE) {
  tryCatch({
  FinalEnergy_carrier_trvl_Transport = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                                       filename='trp_trvl_fuel_use_fleet_tot.dat', varname=NULL, 
                                                       collist=list(regions28,travel_mode_travel_excl_total, energy_carrier_sec_fuel2), 
                                                       namecols=c('region','travel_mode', 'energy_carrier'), novarname = TRUE)
  FinalEnergy_carrier_trvl_Transport <- subset(FinalEnergy_carrier_trvl_Transport, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_carrier_trvl_Transport, region=='WEU'), filter(FinalEnergy_carrier_trvl_Transport, region=='CEU'), by=c("year", "travel_mode", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_carrier_trvl_Transport <- rbind(FinalEnergy_carrier_trvl_Transport, EU)
  FinalEnergy_carrier_trvl_Transport$region = factor(FinalEnergy_carrier_trvl_Transport$region,levels=regions28_EU)
  FinalEnergy_carrier_trvl_Transport <- mutate(FinalEnergy_carrier_trvl_Transport, unit="TJ")
  
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_fuel_use_fleet_tot.dat does not exist\n")
  }) # try
  } # if
  
  # Energy use freight fuels per mode
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  FinalEnergy_frgt_Transport = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                               filename='trp_frgt_Energy.out', varname=NULL, 
                                               collist=list(regions28,travel_mode_freight), 
                                               namecols=c('region','freight_mode'), novarname = TRUE)
  FinalEnergy_frgt_Transport <- subset(FinalEnergy_frgt_Transport, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_frgt_Transport, region=='WEU'), filter(FinalEnergy_frgt_Transport, region=='CEU'), by=c("year", "freight_mode"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, freight_mode, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_frgt_Transport <- rbind(FinalEnergy_frgt_Transport, EU)
  FinalEnergy_frgt_Transport$region = factor(FinalEnergy_frgt_Transport$region,levels=regions28_EU)
  FinalEnergy_frgt_Transport <- mutate(FinalEnergy_frgt_Transport, unit="XJ")
  
  # Energy use transport per energy carries for travel and freight
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  FinalEnergy_Transport = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                               filename='trp_SE.out', varname=NULL, 
                                               collist=list(regions28,energy_carrier_sec_fuel,transport_modus), 
                                               namecols=c('region','energy_carrier', 'transport_mode'), novarname = TRUE)
  FinalEnergy_Transport <- subset(FinalEnergy_Transport, region != "dummy")
  EU <- inner_join(filter(FinalEnergy_Transport, region=='WEU'), filter(FinalEnergy_Transport, region=='CEU'), by=c("year", "energy_carrier", "transport_mode"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, energy_carrier, transport_mode, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FinalEnergy_Transport <- rbind(FinalEnergy_Transport, EU)
  FinalEnergy_Transport$region = factor(FinalEnergy_Transport$region,levels=regions28_EU)
  FinalEnergy_Transport <- mutate(FinalEnergy_Transport, unit="XJ")  
  
  # Person Kilometers Travelled (Tera km)
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  PersonKilometers = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                     filename='trp_trvl_pkm.out', varname=NULL, 
                                     collist=list(regions28,travel_mode_travel), 
                                     namecols=c('region','travel_mode'), novarname = TRUE)
  PersonKilometers <- subset(PersonKilometers, region != "dummy")
  EU <- inner_join(filter(PersonKilometers, region=='WEU'), filter(PersonKilometers, region=='CEU'), by=c("year", "travel_mode"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  PersonKilometers <- rbind(PersonKilometers, EU)
  PersonKilometers$region = factor(PersonKilometers$region,levels=regions28_EU)
  PersonKilometers <- mutate(PersonKilometers, unit="T pkm")

  # Person Kilometers Travelled for new cars(Tera km)
  # First get share of new cars
  PersonKilometersNewCars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  f='trp_trvl_cars_VehNewCapTot.dat'
  ShareNewCars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                     filename=f, varname=NULL, 
                                     collist=list(regions26), 
                                     namecols=c('region'), novarname = TRUE)
  ShareNewCars <- subset(ShareNewCars, region != "dummy")
  PersonKilometersCars <- filter(PersonKilometers, travel_mode=="Car")
  PersonKilometersCars <- subset(PersonKilometersCars, region != "World")
  
  PersonKilometersNewCars <- inner_join(ShareNewCars, PersonKilometersCars, by=c("year", "region"))
  PersonKilometersNewCars <- mutate(PersonKilometersNewCars, value=value.x*value.y)
  PersonKilometersNewCars <- select(PersonKilometersNewCars, region, value, unit)
  },
  error=function(error_handler)
  { cat(paste0("File ", f, " not found"))
  }) # tryCath
  } # if
  else {
    PersonKilometersNewCars = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # Person Kilometers Travelled (Tera km)
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  TonneKilometers = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                     filename='trp_frgt_tkm.out', varname=NULL, 
                                     collist=list(regions28,travel_mode_freight), 
                                     namecols=c('region','travel_mode'), novarname = TRUE)
  TonneKilometers <- subset(TonneKilometers, region != "dummy")
  EU <- inner_join(filter(TonneKilometers, region=='WEU'), filter(TonneKilometers, region=='CEU'), by=c("year", "travel_mode"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  TonneKilometers <- rbind(TonneKilometers, EU)
  TonneKilometers$region = factor(TonneKilometers$region,levels=regions28_EU)
  TonneKilometers <- mutate(TonneKilometers, unit="T tkm")
  

  # Total new vehicles for cars (in %-share)
  VehicleNewCapacity_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  VehicleNewCapacity_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                                  filename='trp_trvl_cars_VehNewCapTot.dat', varname=NULL, 
                                                  collist=list(regions26), 
                                                  namecols=c('region'), novarname = TRUE)
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_VehNewCapTot.dat does not exist\n")
  }) # try
  } # if
  else {
    VehicleNewCapacity_cars = data.frame(matrix(ncol=0,nrow=0))
  }
  # TO DO: finish, add up for EU with personkilometers
  
  # Vehicle share NEW cars
  VehicleShare_new_cars <- NULL
  if (Policy==TRUE) {
  tryCatch({
    VehicleShare_new_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                      filename='trp_trvl_cars_V_share_new.dat', varname=NULL, 
                                      collist=list(regions27,car_type), 
                                      namecols=c('region','car_type'), novarname = TRUE)
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_V_share_new.dat does not exist\n")
  }) # try
  } # if
  else {
    VehicleShare_new_cars = data.frame(matrix(ncol=0,nrow=0))
  }

  # Vehicle share cars
  if(TIMER_version == 'TIMER_3_2') {data_dir = "/tuss/endem/transport";reg=regions26} else{data_dir="/tuss"; reg=regions27}
  tryCatch({
    VehicleShare_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                      filename='trp_trvl_Vshare_car.out', varname=NULL, 
                                      collist=list(reg,car_type), 
                                      namecols=c('region','car_type'), novarname = TRUE)
  
  EU_share <- inner_join(filter(VehicleShare_cars, region=='WEU'), filter(VehicleShare_cars, region=='CEU'), by=c("year", "car_type"))
  PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
  EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
  EU <- inner_join(EU_share, EU_pkm, by=c('year'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, car_type, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  VehicleShare_cars <- rbind(VehicleShare_cars, EU)
  VehicleShare_cars$region = factor(VehicleShare_cars$region,levels=regions28_EU)
  VehicleShare_cars <- mutate(VehicleShare_cars, unit="%")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_Vshare_car.out does not exist\n")
  }) # try
  
  # TO DO: for EU, the shares must be added using 'pkm' as weighting factor
  # Vehicle share busses
  if(TIMER_version == 'TIMER_3_2') {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  tryCatch({
    VehicleShare_busses = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                      filename='trp_trvl_Vshare_bus.out', varname=NULL, 
                                      collist=list(regions26,bus_type), 
                                      namecols=c('region','bus_type'), novarname = TRUE)
  #EU <- inner_join(filter(VehicleShare_busses, region=='WEU'), filter(VehicleShare_busses, region=='CEU'), by=c("year", "bus_type"))
  #EU$region <- "EU"
  #EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, bus_type, value)
  #EU$region = factor(EU$region, levels=regions28_EU)
  #VehicleShare_busses <- rbind(VehicleShare_busses, EU)
  VehicleShare_busses$region = factor(VehicleShare_busses$region,levels=regions28_EU)
  VehicleShare_busses <- mutate(VehicleShare_busses, unit="%")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_Vshare_bus.out does not exist\n")
  }) # try
  
  # Vehicle share train
  if(TIMER_version == 'TIMER_3_2') {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  tryCatch({
    VehicleShare_trains = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                      filename='trp_trvl_Vshare_train.out', varname=NULL, 
                                      collist=list(regions26,train_type), 
                                      namecols=c('region','train_type'), novarname = TRUE)
  #EU <- inner_join(filter(VehicleShare_trains, region=='WEU'), filter(VehicleShare_trains, region=='CEU'), by=c("year", "train_type"))
  #EU$region <- "EU"
  #EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, train_type, value)
  #EU$region = factor(EU$region, levels=regions28_EU)
  #VehicleShare_trains <- rbind(VehicleShare_trains, EU)
  VehicleShare_trains$region = factor(VehicleShare_trains$region,levels=regions28_EU)
  VehicleShare_trains <- mutate(VehicleShare_trains, unit="%")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_Vshare_train.out does not exist\n")
  }) # try
  
  # Vehicle share aircrafts
  if(TIMER_version == 'TIMER_3_2') {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  tryCatch({
    VehicleShare_aircrafts = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                                           filename='trp_trvl_Vshare_air.out', varname=NULL, 
                                           collist=list(regions26,aircraft_type), 
                                           namecols=c('region','aircraft_type'), novarname = TRUE)
  #EU <- inner_join(filter(VehicleShare_aircrafts, region=='WEU'), filter(VehicleShare_aircrafts, region=='CEU'), by=c("year", "aircraft_type"))
  #EU$region <- "EU"
  #EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, aircraft_type, value)
  #EU$region = factor(EU$region, levels=regions28_EU)
  #VehicleShare_aircrafts <- rbind(VehicleShare_aircrafts, EU)
  VehicleShare_aircrafts$region = factor(VehicleShare_aircrafts$region,levels=regions28_EU)
  VehicleShare_aircrafts <- mutate(VehicleShare_aircrafts, unit="%")
  },
    error = function(error_condition) 
  { cat("The file policy/trp_trvl_Vshare_air.out does not exist\n")
  }) # try
  
  # biofuels share for cars in existing fleet (in terms of personkilometers)
  BiofuelShare_existing_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      BiofuelShare_existing_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                                  filename='trp_trvl_cars_V_share_bio.dat', varname=NULL, 
                                                  collist=list(regions26), 
                                                  namecols=c('region'), novarname = TRUE)
      BiofuelShare_existing_cars <- subset(BiofuelShare_existing_cars, region != "dummy")
      EU_bio <- inner_join(filter(BiofuelShare_existing_cars, region=='WEU'), filter(BiofuelShare_existing_cars, region=='CEU'), by=c("year"))
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_bio, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      BiofuelShare_existing_cars <- rbind(BiofuelShare_existing_cars, EU)
      BiofuelShare_existing_cars$region = factor(BiofuelShare_existing_cars$region,levels=regions28_EU)
      BiofuelShare_existing_cars <- mutate(BiofuelShare_existing_cars, unit="%")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_V_share_bio.dat does not exist\n")
  }) # try
  } # if
  else {
    BiofuelShare_existing_cars = data.frame(matrix(ncol=0,nrow=0))
  }

  # biofuels share for cars in new fleet (in terms of personkilometers)
  BiofuelShare_new_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      BiofuelShare_new_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                                   filename='trp_trvl_cars_V_share_new_bio.dat', varname=NULL, 
                                                   collist=list(regions26), 
                                                   namecols=c('region'), novarname = TRUE)
      BiofuelShare_new_cars <- subset(BiofuelShare_new_cars, region != "dummy")
      EU_bio <- inner_join(filter(BiofuelShare_new_cars, region=='WEU'), filter(BiofuelShare_new_cars, region=='CEU'), by=c("year"))
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_bio, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      BiofuelShare_new_cars <- rbind(BiofuelShare_new_cars, EU)
      BiofuelShare_new_cars$region = factor(BiofuelShare_new_cars$region,levels=regions28_EU)
      BiofuelShare_new_cars <- mutate(BiofuelShare_new_cars, unit="%")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_trvl_cars_V_share_new_bio.dat does not exist\n")
    }) # try
  } # if
  else {
    BiofuelShare_new_cars = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # blending share biofuels for cars (in terms of personkilometers)
  BlendingShareBio_cars_pkm = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  BlendingShareBio_cars_pkm = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                              filename='trp_trvl_cars_V_Blending_share_bio_pkm.dat', varname=NULL, 
                                              collist=list(regions27), 
                                              namecols=c('region'), novarname = TRUE)
  BlendingShareBio_cars_pkm <- subset(BlendingShareBio_cars_pkm, region != "dummy")
  EU_bio <- inner_join(filter(BlendingShareBio_cars_pkm, region=='WEU'), filter(BlendingShareBio_cars_pkm, region=='CEU'), by=c("year"))
  PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
  EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
  EU <- inner_join(EU_bio, EU_pkm, by=c('year'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  BlendingShareBio_cars_pkm <- rbind(BlendingShareBio_cars_pkm, EU)
  BlendingShareBio_cars_pkm$region = factor(BlendingShareBio_cars_pkm$region,levels=regions28_EU)
  BlendingShareBio_cars_pkm <- mutate(BlendingShareBio_cars_pkm, unit="%")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_V_Blending_share_bio_pkm.dat does not exist\n")
  }) # try
  } # if
  else {
    BlendingShareBio_cars_pkm = data.frame(matrix(ncol=0,nrow=0))
  }

  # blending share biofuels for new cars (in terms of personkilometers)
  BlendingShareBio_new_cars_pkm = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      BlendingShareBio_new_cars_pkm = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                                  filename='trp_trvl_cars_V_Blending_share_new_bio_pkm.dat', varname=NULL, 
                                                  collist=list(regions27), 
                                                  namecols=c('region'), novarname = TRUE)
      BlendingShareBio_new_cars_pkm <- subset(BlendingShareBio_new_cars_pkm, region != "dummy")
      EU_bio <- inner_join(filter(BlendingShareBio_new_cars_pkm, region=='WEU'), filter(BlendingShareBio_new_cars_pkm, region=='CEU'), by=c("year"))
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_bio, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      BlendingShareBio_new_cars_pkm <- rbind(BlendingShareBio_new_cars_pkm, EU)
      BlendingShareBio_new_cars_pkm$region = factor(BlendingShareBio_new_cars_pkm$region,levels=regions28_EU)
      BlendingShareBio_new_cars_pkm <- mutate(BlendingShareBio_new_cars_pkm, unit="%")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_trvl_cars_V_Blending_share_new_bio_pkm.dat does not exist\n")
    }) # try
  } # if
  else {
    BlendingShareBio_new_cars_pkm = data.frame(matrix(ncol=0,nrow=0))
  } 
   # transport fuel use per region, mode and energy carrier (secondary fuel use)
  FuelUseFleet_trvl = data.frame(matrix(ncol=0,nrow=0))
  FuelUseFleet_frgt = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  # travel
  FuelUseFleet_trvl = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                            filename='trp_trvl_fuel_use_fleet_tot.dat', varname=NULL, 
                                            collist=list(regions28, travel_mode_travel_excl_total, energy_carrier_sec_fuel2), 
                                            namecols=c('region', 'travel_mode', 'energy_carrier'), novarname = TRUE)
  FuelUseFleet_trvl <- subset(FuelUseFleet_trvl, region != "dummy")
  EU <- inner_join(filter(FuelUseFleet_trvl, region=='WEU'), filter(FuelUseFleet_trvl, region=='CEU'), by=c("year", "travel_mode", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FuelUseFleet_trvl$region = factor(FuelUseFleet_trvl$region, levels=regions28_EU)
  FuelUseFleet_trvl <- rbind(FuelUseFleet_trvl, EU)
  FuelUseFleet_trvl <- mutate(FuelUseFleet_trvl, unit="TJ") # TJ?
  # add total fuel use
  FuelUseFleet_total_trvl <- FuelUseFleet_trvl %>% group_by(year, region, energy_carrier, unit) %>% 
                                                    summarize(value=sum(value), na.rm=TRUE) %>%
                                                    mutate(travel_mode='Total') %>%
                                                    select(year, region, travel_mode, energy_carrier, value, unit)
  FuelUseFleet_trvl <- as.data.frame(FuelUseFleet_trvl)
  FuelUseFleet_total_trvl <- as.data.frame(FuelUseFleet_total_trvl)
  FuelUseFleet_trvl <- rbind(FuelUseFleet_trvl, FuelUseFleet_total_trvl)
  # freight
  FuelUseFleet_frgt = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                      filename='trp_frgt_fuel_use_fleet_tot.dat', varname=NULL, 
                                      collist=list(regions28, travel_mode_freight_excl_total, energy_carrier_sec_fuel2), 
                                      namecols=c('region', 'travel_mode', 'energy_carrier'), novarname = TRUE)
  FuelUseFleet_frgt <- subset(FuelUseFleet_frgt, region != "dummy")
  EU <- inner_join(filter(FuelUseFleet_frgt, region=='WEU'), filter(FuelUseFleet_frgt, region=='CEU'), by=c("year", "travel_mode", "energy_carrier"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, travel_mode, energy_carrier, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  FuelUseFleet_frgt$region = factor(FuelUseFleet_frgt$region, levels=regions28_EU)
  FuelUseFleet_frgt <- rbind(FuelUseFleet_frgt, EU)
  FuelUseFleet_frgt <- mutate(FuelUseFleet_frgt, unit="TJ") # TJ?
  # add total fuel use
  FuelUseFleet_total_frgt <- FuelUseFleet_frgt %>% group_by(year, region, energy_carrier, unit) %>% 
                             summarize(value=sum(value), na.rm=TRUE) %>%
                             mutate(travel_mode='Total') %>%
                             select(year, region, travel_mode, energy_carrier, value, unit)
  FuelUseFleet_frgt <- as.data.frame(FuelUseFleet_frgt)
  FuelUseFleet_total_frgt <- as.data.frame(FuelUseFleet_total_frgt)
  FuelUseFleet_frgt <- rbind(FuelUseFleet_frgt, FuelUseFleet_total_frgt)
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_fuel_use_fleet_tot.dat or trp_frgt_fuel_use_fleet_tot.dat does not exist\n")
  }) # try
  } # if
  else {
    FuelUseFleet_trvl = data.frame(matrix(ncol=0,nrow=0))
    FuelUseFleet_frgt = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # blending share biofuels travel (in terms of energy))
  BlendingShareBio_energy_trvl = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  BlendingShareBio_energy_trvl = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                              filename='trp_trvl_Blending_share_bio_energy_trvl.dat', varname=NULL, 
                                              collist=list(regions26, travel_mode_travel_excl_total), 
                                              namecols=c('region', 'travel_mode'), novarname = TRUE)
  BlendingShareBio_energy_trvl <- subset(BlendingShareBio_energy_trvl, region != "dummy")
  EU_bio <- inner_join(filter(BlendingShareBio_energy_trvl, region=='WEU'), filter(BlendingShareBio_energy_trvl, region=='CEU'), by=c("year", "travel_mode"))
  FuelUseFleet_total <- filter(FuelUseFleet_trvl, energy_carrier == 'Total') %>%
                          group_by(year, region, travel_mode) %>%
                          summarise(value=sum(value, na.rm=TRUE))
  EU_energy <- inner_join(filter(FuelUseFleet_total, region=='WEU'), filter(FuelUseFleet_total, region=='CEU'), by=c("year", "travel_mode"))
  EU <- inner_join(EU_bio, EU_energy, by=c('year', 'travel_mode'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value, travel_mode)
  EU$region = factor(EU$region, levels=regions28_EU)
  BlendingShareBio_energy_trvl <- rbind(BlendingShareBio_energy_trvl, EU)
  BlendingShareBio_energy_trvl$region = factor(BlendingShareBio_energy_trvl$region,levels=regions28_EU)
  BlendingShareBio_energy_trvl <- mutate(BlendingShareBio_energy_trvl, unit="%")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_Blending_share_bio_energy_trvl.dat does not exist\n")
  }) # try
  } # if
  else {
    BlendingShareBio_energy_trvl = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # blending share biofuels for freight (in terms of energy))
  BlendingShareBio_energy_frgt = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    # HOW TO CALCULATE THIS? Not opssible based on current TIMER output
  } # if
  else {
    BlendingShareBio_energy_frgt = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # Share of electric cars
  ElectricShare_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      ElectricShare_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                               filename='trp_trvl_cars_V_share_electric.dat', varname=NULL, 
                                               collist=list(regions26), 
                                               namecols=c('region'), novarname = TRUE)
      ElectricShare_cars <- subset(ElectricShare_cars, region != "dummy")
      EU_elec <- inner_join(filter(ElectricShare_cars, region=='WEU'), filter(ElectricShare_cars, region=='CEU'), by=c("year"))
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_elec, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      ElectricShare_cars <- rbind(ElectricShare_cars, EU)
      ElectricShare_cars$region = factor(ElectricShare_cars$region,levels=regions28_EU)
      ElectricShare_cars <- mutate(ElectricShare_cars, unit="%")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_trvl_cars_V_share_electric.dat does not exist\n")
    }) # try
  } # if
  else{
    ElectricShare_cars = data.frame(matrix(ncol=0,nrow=0))
  }
  
  
  # Share of new electric cars
  ElectricShare_new_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  ElectricShare_new_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                           filename='trp_trvl_cars_V_share_new_electric.dat', varname=NULL, 
                                           collist=list(regions26), 
                                           namecols=c('region'), novarname = TRUE)
  ElectricShare_new_cars <- subset(ElectricShare_new_cars, region != "dummy")
  EU_elec <- inner_join(filter(ElectricShare_new_cars, region=='WEU'), filter(ElectricShare_new_cars, region=='CEU'), by=c("year"))
  PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
  EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
  EU <- inner_join(EU_elec, EU_pkm, by=c('year'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ElectricShare_new_cars <- rbind(ElectricShare_new_cars, EU)
  ElectricShare_new_cars$region = factor(ElectricShare_new_cars$region,levels=regions28_EU)
  ElectricShare_new_cars <- mutate(ElectricShare_new_cars, unit="%")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_V_share_new_electric.dat does not exist\n")
  }) # try
  } # if
  else{
    ElectricShare_new_cars = data.frame(matrix(ncol=0,nrow=0))
  }
  
  
  
  # Share of electric heavy trucks
  ElectricShare_HvyT = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      ElectricShare_HvyT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                           filename='trp_frgt_HvyT_V_share_electric.dat', varname=NULL, 
                                           collist=list(regions26), 
                                           namecols=c('region'), novarname = TRUE)
      ElectricShare_HvyT <- subset(ElectricShare_HvyT, region != "dummy")
      EU_elec <- inner_join(filter(ElectricShare_HvyT, region=='WEU'), filter(ElectricShare_HvyT, region=='CEU'), by=c("year"))
      TonneKilometers_HvyT <- filter(TonneKilometers, travel_mode=='Heavy truck') %>% select(year, region, value)
      EU_pkm <- inner_join(filter(TonneKilometers_HvyT, region=='WEU'), filter(TonneKilometers_HvyT, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_elec, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      ElectricShare_HvyT <- rbind(ElectricShare_HvyT, EU)
      ElectricShare_HvyT$region = factor(ElectricShare_HvyT$region,levels=regions28_EU)
      ElectricShare_HvyT <- mutate(ElectricShare_HvyT, unit="%")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_frgt_HvyT_V_share_electric.dat does not exist\n")
    }) # try
  } # if
  else{
    ElectricShare_HvyT = data.frame(matrix(ncol=0,nrow=0))
  }
  
  
  # Share of new electric heavy trucks
  ElectricShare_new_HvyT = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      ElectricShare_new_HvyT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""), 
                                               filename='trp_frgt_HvyT_V_share_new_electric.dat', varname=NULL, 
                                               collist=list(regions26), 
                                               namecols=c('region'), novarname = TRUE)
      ElectricShare_new_HvyT <- subset(ElectricShare_new_HvyT, region != "dummy")
      EU_elec <- inner_join(filter(ElectricShare_new_HvyT, region=='WEU'), filter(ElectricShare_new_HvyT, region=='CEU'), by=c("year"))
      TonneKilometers_HvyT <- filter(TonneKilometers, travel_mode=='Heavy truck') %>% select(year, region, value)
      EU_pkm <- inner_join(filter(TonneKilometers_HvyT, region=='WEU'), filter(TonneKilometers_HvyT, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_elec, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      ElectricShare_new_HvyT <- rbind(ElectricShare_new_HvyT, EU)
      ElectricShare_new_HvyT$region = factor(ElectricShare_new_HvyT$region,levels=regions28_EU)
      ElectricShare_new_HvyT <- mutate(ElectricShare_new_HvyT, unit="%")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_frgt_HvyT_V_share_new_electric.dat does not exist\n")
    }) # try
  } # if
  else{
    ElectricShare_new_HvyT = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # Efficiency travel
  EfficiencyTravel = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      EfficiencyTravel = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                             filename='trp_trvl_eff_base.dat', varname=NULL, 
                                             collist=list(regions26, travel_mode_travel_excl_total, car_type), 
                                             namecols=c('region', 'mode', 'travel_type'), novarname = TRUE)
      EfficiencyTravel <- subset(EfficiencyTravel, region != "dummy")
      #EU_fleet <- inner_join(filter(EfficiencyTravel, region=='WEU'), filter(EfficiencyTravel, region=='CEU'), by=c("year"))
      #PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      ## weight factor should be New person kilometers, but as the share of new cars is constant and the same for each region, also PersonKilometers can be used
      #EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      #EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
      #EU$region <- "EU"
      #EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      #U$region = factor(EU$region, levels=regions28_EU)
      #EfficiencyTravel <- rbind(EfficiencyTravel, EU)
      EfficiencyTravel$region = factor(EfficiencyTravel$region,levels=regions28_EU)
      EfficiencyTravel <- mutate(EfficiencyTravel, unit="MJ/pkm")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_trvl_eff_base.dat does not exist\n")
    }) # try
  } # if
  else {
    EfficiencyTravel = data.frame(matrix(ncol=0,nrow=0))
  }
 
  # Efficiency freight
  EfficiencyFreight = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      EfficiencyFreight = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                         filename='trp_frgt_eff_base.dat', varname=NULL, 
                                         collist=list(regions26, travel_mode_freight_excl_total, car_type), 
                                         namecols=c('region', 'mode', 'travel_type'), novarname = TRUE)
      EfficiencyFreight <- subset(EfficiencyFreight, region != "dummy")
      #EU_fleet <- inner_join(filter(EfficiencyTravel, region=='WEU'), filter(EfficiencyTravel, region=='CEU'), by=c("year"))
      #PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      ## weight factor should be New person kilometers, but as the share of new cars is constant and the same for each region, also PersonKilometers can be used
      #EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      #EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
      #EU$region <- "EU"
      #EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      #U$region = factor(EU$region, levels=regions28_EU)
      #EfficiencyTravel <- rbind(EfficiencyTravel, EU)
      EfficiencyFreight$region = factor(EfficiencyFreight$region,levels=regions28_EU)
      EfficiencyFreight <- mutate(EfficiencyFreight, unit="MJ/pkm")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_frgt_eff_base.dat does not exist\n")
    }) # try
  } # if
  else {
    EfficiencyFreight = data.frame(matrix(ncol=0,nrow=0))
  } 
  
  # Efficiency total fleet for  cars
  EfficiencyFleet_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
    EfficiencyFleet_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                               filename='trp_trvl_cars_Eff.dat', varname=NULL, 
                                               collist=list(regions27), 
                                               namecols=c('region'), novarname = TRUE)
    EfficiencyFleet_cars <- subset(EfficiencyFleet_cars, region != "dummy")
    EU_fleet <- inner_join(filter(EfficiencyFleet_cars, region=='WEU'), filter(EfficiencyFleet_cars, region=='CEU'), by=c("year"))
    PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
    # weight factor should be New person kilometers, but as the share of new cars is constant and the same for each region, also PersonKilometers can be used
    EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
    EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
    EU$region <- "EU"
    EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
    EU$region = factor(EU$region, levels=regions28_EU)
    EfficiencyFleet_cars <- rbind(EfficiencyFleet_cars, EU)
    EfficiencyFleet_cars$region = factor(EfficiencyFleet_cars$region,levels=regions28_EU)
    EfficiencyFleet_cars <- mutate(EfficiencyFleet_cars, unit="MJ/pkm")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_Eff.dat does not exist\n")
  }) # try
  } # if
  else {
    EfficiencyFleet_cars = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # Efficiency total fleet for new cars
  EfficiencyFleet_new_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  EfficiencyFleet_new_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                         filename='trp_trvl_cars_Eff_new.dat', varname=NULL, 
                                         collist=list(regions27), 
                                         namecols=c('region'), novarname = TRUE)
  EfficiencyFleet_new_cars <- subset(EfficiencyFleet_new_cars, !region%in%c("dummy", "World"))
  # add world
  PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car', !(region%in%c('EU', 'World'))) %>% select(year, region, value)
  PersonKilometers_cars$region <- factor(PersonKilometers_cars$region, levels=regions28)
  PersonKilometers_cars_World <- filter(PersonKilometers, travel_mode=='Car', region%in%c('World')) %>% select(year, region, value)
  PersonKilometers_cars$region <- factor(PersonKilometers_cars$region, levels=regions28)
  World = inner_join(EfficiencyFleet_new_cars, PersonKilometers_cars, by=c('year', 'region')) %>%
          inner_join(PersonKilometers_cars_World, by=c('year')) %>%
          mutate(value=value.x*value.y/value) %>%
          select(-value.x, -value.y, -region.x, -region.y) %>%
          group_by(year) %>%
          summarise(value=sum(value)) %>%
          mutate(region="World")
  World$region = factor(World$region, levels=regions28_EU)
  EU_fleet <- inner_join(filter(EfficiencyFleet_new_cars, region=='WEU'), filter(EfficiencyFleet_new_cars, region=='CEU'), by=c("year"))
  PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
  # weight factor should be New person kilometers, but as the share of new cars is constant and the same for each region, also PersonKilometers can be used
  EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
  EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  EfficiencyFleet_new_cars <- rbind(EfficiencyFleet_new_cars, EU) %>% rbind(World)
  EfficiencyFleet_new_cars$region = factor(EfficiencyFleet_new_cars$region,levels=regions28_EU)
  EfficiencyFleet_new_cars <- mutate(EfficiencyFleet_new_cars, unit="MJ/pkm")
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_Eff_new.dat does not exist\n")
  }) # try
  } # if
  else {
    EfficiencyFleet_new_cars = data.frame(matrix(ncol=0,nrow=0))
  }

  # Efficiency total fleet for new cars (excl EV)
  EfficiencyFleet_new_cars_exclEV = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      EfficiencyFleet_new_cars_exclEV = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                                        filename='trp_trvl_cars_Eff_new_exclEV.dat', varname=NULL, 
                                                        collist=list(regions27), 
                                                        namecols=c('region'), novarname = TRUE)
      EfficiencyFleet_new_cars_exclEV <- subset(EfficiencyFleet_new_cars_exclEV, !region%in%c("dummy", "World"))
      # add world
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car', !(region%in%c('EU', 'World'))) %>% select(year, region, value)
      PersonKilometers_cars$region <- factor(PersonKilometers_cars$region, levels=regions28)
      PersonKilometers_cars_World <- filter(PersonKilometers, travel_mode=='Car', region%in%c('World')) %>% select(year, region, value)
      PersonKilometers_cars$region <- factor(PersonKilometers_cars$region, levels=regions28)
      World = inner_join(EfficiencyFleet_new_cars_exclEV, PersonKilometers_cars, by=c('year', 'region')) %>%
              inner_join(PersonKilometers_cars_World, by=c('year')) %>%
              mutate(value=value.x*value.y/value) %>%
              select(-value.x, -value.y, -region.x, -region.y) %>%
              group_by(year) %>%
              summarise(value=sum(value)) %>%
              mutate(region="World")
      World$region = factor(World$region, levels=regions28_EU)
      # add EU
      EU_fleet <- inner_join(filter(EfficiencyFleet_new_cars_exclEV, region=='WEU'), filter(EfficiencyFleet_new_cars_exclEV, region=='CEU'), by=c("year"))
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      # weight factor should be New person kilometers (excl EV), but as the share of new cars is constant and the same for each region, also PersonKilometers can be used
      EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      EfficiencyFleet_new_cars_exclEV <- rbind(EfficiencyFleet_new_cars_exclEV, EU) %>% rbind(World)
      EfficiencyFleet_new_cars_exclEV$region = factor(EfficiencyFleet_new_cars_exclEV$region,levels=regions28_EU)
      EfficiencyFleet_new_cars_exclEV <- mutate(EfficiencyFleet_new_cars_exclEV, unit="MJ/pkm")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_trvl_cars_Eff_new_exclEV.dat does not exist\n")
    }) # try
  } # if
  else {
    EfficiencyFleet_new_cars_exclEV = data.frame(matrix(ncol=0,nrow=0))
  }
 
  # Efficiency total fleet for new cars (only EV)
  EfficiencyFleet_new_cars_EV = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
    tryCatch({
      EfficiencyFleet_new_cars_EV = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                                        filename='trp_trvl_cars_Eff_new_EV.dat', varname=NULL, 
                                                        collist=list(regions27), 
                                                        namecols=c('region'), novarname = TRUE)
      EfficiencyFleet_new_cars_EV <- subset(EfficiencyFleet_new_cars_EV, !region%in%c("dummy", "World"))
      # add world
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car', !(region%in%c('EU', 'World'))) %>% select(year, region, value)
      PersonKilometers_cars$region <- factor(PersonKilometers_cars$region, levels=regions28)
      PersonKilometers_cars_World <- filter(PersonKilometers, travel_mode=='Car', region%in%c('World')) %>% select(year, region, value)
      PersonKilometers_cars$region <- factor(PersonKilometers_cars$region, levels=regions28)
      World = inner_join(EfficiencyFleet_new_cars_EV, PersonKilometers_cars, by=c('year', 'region')) %>%
        inner_join(PersonKilometers_cars_World, by=c('year')) %>%
        mutate(value=value.x*value.y/value) %>%
        select(-value.x, -value.y, -region.x, -region.y) %>%
        group_by(year) %>%
        summarise(value=sum(value)) %>%
        mutate(region="World")
      World$region = factor(World$region, levels=regions28_EU)
      # add EU
      EU_fleet <- inner_join(filter(EfficiencyFleet_new_cars_EV, region=='WEU'), filter(EfficiencyFleet_new_cars_EV, region=='CEU'), by=c("year"))
      PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car') %>% select(year, region, value)
      # weight factor should be New person kilometers (excl EV), but as the share of new cars is constant and the same for each region, also PersonKilometers can be used
      EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
      EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
      EU$region <- "EU"
      EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
      EU$region = factor(EU$region, levels=regions28_EU)
      EfficiencyFleet_new_cars_EV <- rbind(EfficiencyFleet_new_cars_EV, EU) %>% rbind(World)
      EfficiencyFleet_new_cars_EV$region = factor(EfficiencyFleet_new_cars_EV$region,levels=regions28_EU)
      EfficiencyFleet_new_cars_EV <- mutate(EfficiencyFleet_new_cars_EV, unit="MJ/pkm")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_trvl_cars_Eff_new_EV.dat does not exist\n")
    }) # try
  } # if
  else {
    EfficiencyFleet_new_cars_EV = data.frame(matrix(ncol=0,nrow=0))
  }  
  # Efficiency total fleet for new busses
  EfficiencyFleet_new_busses = NULL
  if (Policy==TRUE) {
    tryCatch({
    EfficiencyFleet_new_busses = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                               filename='trp_trvl_busses_Eff_new.dat', varname=NULL, 
                                               collist=list(regions27), 
                                               namecols=c('region'), novarname = TRUE)
    EfficiencyFleet_new_busses <- subset(EfficiencyFleet_new_busses, region != "dummy")
    EU_fleet <- inner_join(filter(EfficiencyFleet_new_busses, region=='WEU'), filter(EfficiencyFleet_new_busses, region=='CEU'), by=c("year"))
    PersonKilometers_busses <- filter(PersonKilometers, travel_mode=='Bus') %>% select(year, region, value)
    # weight factor should be New person kilometers, but as the share of new busses is constant and the same for each region, also PersonKilometers can be used
    EU_pkm <- inner_join(filter(PersonKilometers_busses, region=='WEU'), filter(PersonKilometers_busses, region=='CEU'), by=c("year"))
    EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
    EU$region <- "EU"
    EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
    EU$region = factor(EU$region, levels=regions28_EU)
    EfficiencyFleet_new_busses <- rbind(EfficiencyFleet_new_busses, EU)
    EfficiencyFleet_new_busses$region = factor(EfficiencyFleet_new_busses$region,levels=regions28_EU)
    EfficiencyFleet_new_busses <- mutate(EfficiencyFleet_new_busses, unit="MJ/pkm")
    },
    error = function(error_condition) 
    { cat("The file policy/trp_trvl_busses_Eff_new.dat does not exist\n")
    }) # try
  } # if
  else {
    EfficiencyFleet_new_busses = data.frame(matrix(ncol=0,nrow=0))
  }

  # Efficiency total fleet for new medium trucks
  EfficiencyFleet_new_MedT = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  EfficiencyFleet_new_MedT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                             filename='trp_frgt_MedT_Eff_new.dat', varname=NULL, 
                                             collist=list(regions27), 
                                             namecols=c('region'), novarname = TRUE)
  EfficiencyFleet_new_MedT <- subset(EfficiencyFleet_new_MedT, region != "dummy")
  EU_fleet <- inner_join(filter(EfficiencyFleet_new_MedT, region=='WEU'), filter(EfficiencyFleet_new_MedT, region=='CEU'), by=c("year"))
  TonneKilometers_MedT <- filter(TonneKilometers, travel_mode=='Medium truck') %>% select(year, region, value)
  EU_tkm <- inner_join(filter(TonneKilometers_MedT, region=='WEU'), filter(TonneKilometers_MedT, region=='CEU'), by=c("year"))
  EU <- inner_join(EU_fleet, EU_tkm, by=c('year'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  EfficiencyFleet_new_MedT <- rbind(EfficiencyFleet_new_MedT, EU)
  EfficiencyFleet_new_MedT$region = factor(EfficiencyFleet_new_MedT$region,levels=regions28_EU)
  EfficiencyFleet_new_MedT <- mutate(EfficiencyFleet_new_MedT, unit="MJ/tkm") 
  },
  error = function(error_condition) 
  { cat("The file policy/trp_frgt_MedT_Eff_new.dat does not exist\n")
  }) # try
  } # if
  else {
    EfficiencyFleet_new_MedT = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # Efficiency total fleet for  heavy trucks
  EfficiencyFleet_HvyT = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({  
    EfficiencyFleet_HvyT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                               filename='trp_frgt_HvyT_Eff.dat', varname=NULL, 
                                               collist=list(regions27), 
                                               namecols=c('region'), novarname = TRUE)
    EfficiencyFleet_HvyT <- subset(EfficiencyFleet_HvyT, region != "dummy")
    EU_fleet <- inner_join(filter(EfficiencyFleet_HvyT, region=='WEU'), filter(EfficiencyFleet_HvyT, region=='CEU'), by=c("year"))
    TonneKilometers_HvyT <- filter(TonneKilometers, travel_mode=='Heavy truck') %>% select(year, region, value)
    EU_tkm <- inner_join(filter(TonneKilometers_HvyT, region=='WEU'), filter(TonneKilometers_HvyT, region=='CEU'), by=c("year"))
    EU <- inner_join(EU_fleet, EU_tkm, by=c('year'))
    EU$region <- "EU"
    EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
    EU$region = factor(EU$region, levels=regions28_EU)
    EfficiencyFleet_HvyT <- rbind(EfficiencyFleet_HvyT, EU)
    EfficiencyFleet_HvyT$region = factor(EfficiencyFleet_HvyT$region,levels=regions28_EU)
    EfficiencyFleet_HvyT <- mutate(EfficiencyFleet_HvyT, unit="MJ/tkm") 
  },
  error = function(error_condition) 
  { cat("The file policy/trp_frgt_HvyT_Eff.dat does not exist\n")
  }) # try
  } # if
  else {
    EfficiencyFleet_HvyT = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # Efficiency total fleet for new heavy trucks
  EfficiencyFleet_new_HvyT = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  EfficiencyFleet_new_HvyT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                             filename='trp_frgt_HvyT_Eff_new.dat', varname=NULL, 
                                             collist=list(regions27), 
                                             namecols=c('region'), novarname = TRUE)
  EfficiencyFleet_new_HvyT <- subset(EfficiencyFleet_new_HvyT, region != "dummy")
  EU_fleet <- inner_join(filter(EfficiencyFleet_new_HvyT, region=='WEU'), filter(EfficiencyFleet_new_HvyT, region=='CEU'), by=c("year"))
  TonneKilometers_HvyT <- filter(TonneKilometers, travel_mode=='Heavy truck') %>% select(year, region, value)
  EU_tkm <- inner_join(filter(TonneKilometers_HvyT, region=='WEU'), filter(TonneKilometers_HvyT, region=='CEU'), by=c("year"))
  EU <- inner_join(EU_fleet, EU_tkm, by=c('year'))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  EfficiencyFleet_new_HvyT <- rbind(EfficiencyFleet_new_HvyT, EU)
  EfficiencyFleet_new_HvyT$region = factor(EfficiencyFleet_new_HvyT$region,levels=regions28_EU)
  EfficiencyFleet_new_HvyT <- mutate(EfficiencyFleet_new_HvyT, unit="MJ/tkm") 
  },
  error = function(error_condition) 
  { cat("The file policy/trp_frgt_HvyT_Eff_new.dat does not exist\n")
  }) # try
  } # if
  else {
    EfficiencyFleet_new_HvyT = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # Cost per km for cars
  # CostPerPkm_car
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/transport"} else{data_dir="/tuss"}
  CostPerKm_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""),   
                                                 filename='trp_trvl_CostPerPkm_car.out', varname=NULL, 
                                                 collist=list(regions26, car_type), 
                                                 namecols=c('region', 'travel_type'), novarname = TRUE)
  CostPerKm_cars <- subset(CostPerKm_cars, region != "dummy")
  #EU_fleet <- inner_join(filter(CostPerKm_cars, region=='WEU'), filter(CostPerKm_cars, region=='CEU'), by=c("year"))
  #PersonKilometers_cars <- filter(PersonKilometers, travel_mode=='Car', region != 'World') %>% select(year, region, value)
  #EU_pkm <- inner_join(filter(PersonKilometers_cars, region=='WEU'), filter(PersonKilometers_cars, region=='CEU'), by=c("year"))
  #EU <- inner_join(EU_fleet, EU_pkm, by=c('year'))
  #U$region <- "EU"
  #U <- EU %>% mutate(value=(value.x.x*value.x.y+value.y.x*value.y.y)/(value.x.y+value.y.y)) %>% select(year, region, value)
  #U$region = factor(EU$region, levels=regions28_EU)
  #CostPerKm_cars <- rbind(CostPerKm_cars, EU)
  CostPerKm_cars$region = factor(CostPerKm_cars$region,levels=regions28_EU)
  CostPerKm_cars <- mutate(CostPerKm_cars, unit="MJ/pkm")
  
  # energy tax cars
  EnergyTax_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
    EnergyTax_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                     filename='trp_trvl_cars_energy_tax.dat', varname=NULL, 
                                     collist=list(regions26, energy_carrier_sec_fuel), 
                                     namecols=c('region', 'energy_carrier'), novarname = TRUE)
    EnergyTax_cars <- subset(EnergyTax_cars, region != "dummy")
    EnergyTax_cars$region = factor(EnergyTax_cars$region,levels=regions28)
    EnergyTax_cars$region = factor(EnergyTax_cars$region,levels=regions28_EU)
    EnergyTax_cars <- mutate(EnergyTax_cars, unit="MJ/tkm") 
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_energy_tax.dat does not exist\n")
  }) # try
  } # if
  else {
    EnergyTax_cars = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # energy tax heavy trucks
  EnergyTax_HvyT = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
  EnergyTax_HvyT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                             filename='trp_frgt_HvyT_energy_tax.dat', varname=NULL, 
                                             collist=list(regions26, energy_carrier_sec_fuel), 
                                             namecols=c('region', 'energy_carrier'), novarname = TRUE)
  EnergyTax_HvyT <- subset(EnergyTax_HvyT, region != "dummy")
  EnergyTax_HvyT$region = factor(EnergyTax_HvyT$region,levels=regions28)
  EnergyTax_HvyT$region = factor(EnergyTax_HvyT$region,levels=regions28_EU)
  EnergyTax_HvyT <- mutate(EnergyTax_HvyT, unit="US$2005/MJ") 
  },
  error = function(error_condition) 
  { cat("The file policy/trp_frgt_HvyT_energy_tax.dat does not exist\n")
  }) # try
  } # if
  else {
    EnergyTax_HvyT = data.frame(matrix(ncol=0,nrow=0))
  }

  # factor cars
  Factor_cars = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
    Factor_cars = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                  filename='trp_trvl_cars_energy_tax_factor_ST.dat', varname=NULL, 
                                  collist=list(regions26, travel_mode_travel_excl_total, energy_carrier_sec_fuel), 
                                  namecols=c('region', 'travel_mode', 'energy_carrier'), novarname = TRUE)
    Factor_cars <- subset(Factor_cars, region != "dummy")
    Factor_cars$region = factor(Factor_cars$region,levels=regions28)
    Factor_cars$region = factor(Factor_cars$region,levels=regions28_EU)
    Factor_cars <- mutate(Factor_cars, unit="") 
  },
  error = function(error_condition) 
  { cat("The file policy/trp_trvl_cars_energy_tax_factor_ST.dat does not exist\n")
  }) # try
  } # if
  else {
    Factor_cars = data.frame(matrix(ncol=0,nrow=0))
  }
  
  # factor heavy trucks
  Factor_HvyT = data.frame(matrix(ncol=0,nrow=0))
  if (Policy==TRUE) {
  tryCatch({
    Factor_HvyT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, "/policy", sep=""),   
                                     filename='trp_frgt_HvyT_energy_tax_factor_ST.dat', varname=NULL, 
                                     collist=list(regions26, travel_mode_freight_excl_total, energy_carrier_sec_fuel), 
                                     namecols=c('region', 'freight_mode', 'energy_carrier'), novarname = TRUE)
    Factor_HvyT <- subset(Factor_HvyT, region != "dummy")
    Factor_HvyT$region = factor(Factor_HvyT$region,levels=regions28)
    Factor_HvyT$region = factor(Factor_HvyT$region,levels=regions28_EU)
    Factor_HvyT <- mutate(Factor_HvyT, unit="") 
  },
  error = function(error_condition) 
  { cat("The file policy/trp_frgt_HvyT_energy_tax_factor_ST.dat does not exist\n")
  }) # try
  } # if
  else {
    Factor_HvyT = data.frame(matrix(ncol=0,nrow=0))
  }
 
  #POP
  # Unit: million people
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/global";reg=regions27} else{data_dir="/tuss";reg=regions28}
  POP = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                        filename='pop.scn', varname=NULL, 
                        collist=list(reg), 
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
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/global"} else{data_dir="/indicatoren"}
  GDP_MER = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/global";reg=regions27;f='GDP_ppp.scn'} else{data_dir="/indicatoren";reg=regions28;f='gpdppp.out'}
  GDP_PPPpc = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                              filename=f, varname=NULL, 
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
  if(TIMER_version == 'TIMER_3_2') {data_dir = "/tuss/global";reg=regions28} else{data_dir="/indicatoren";reg=regions28}
  IVA = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
                        filename='iva_pc.scn', varname=NULL, 
                        collist=list(reg), 
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
  
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/tuss/endem/residential"} else{data_dir="/tuss"}
  FloorSpace = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_scenario, data_dir, sep=""), 
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
  
  # IMAGE
  ForestArea = NULL
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/output/land_use"} else{data_dir="/output"}
  tryCatch({
  ForestArea = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, data_dir, sep=""), 
                               filename='FORAREA.OUT', varname=NULL, 
                               collist=list(forest_type, regions28), 
                               namecols=c('forest_type','region'), novarname = TRUE)
  ForestArea <- subset(ForestArea, region != "dummy")
  EU <- inner_join(filter(ForestArea, region=='WEU'), filter(ForestArea, region=='CEU'), by=c("year", "forest_type"))
  EU$region <- "EU"
  EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, forest_type, value)
  EU$region = factor(EU$region, levels=regions28_EU)
  ForestArea <- rbind(ForestArea, EU)
  ForestArea$region = factor(ForestArea$region,levels=regions28_EU)  
  ForestArea <- mutate(ForestArea, unit="km2")
  ForestArea=data.table(ForestArea)
  yy=seq(1970,2100)
  ForestArea = ForestArea[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('forest_type','region','unit')]
  setnames(ForestArea,"V1","value")
  setnames(ForestArea,"V2","year")
  setcolorder(ForestArea,c("year","region","forest_type","value","unit"))
  },
  error = function(error_condition) 
  { cat("The IMAGE file FORAREA.OUT does not exist\n")
  }) # try
  
  AddDeforestation <- NULL
  tryCatch({
    if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/output/forestry_wood"} else{data_dir="/output"}
    AddDeforestation = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, data_dir, sep=""), 
                                 filename='TOTADDDEFORAREA.OUT', varname=NULL, 
                                 collist=list(deforest_type, regions27), 
                                 namecols=c('deforest_type','region'), novarname = TRUE)
    AddDeforestation <- subset(AddDeforestation, region != "dummy")
    EU <- inner_join(filter(AddDeforestation, region=='WEU'), filter(AddDeforestation, region=='CEU'), by=c("year", "deforest_type"))
    EU$region <- "EU"
    EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, deforest_type, value)
    EU$region = factor(EU$region, levels=regions28_EU)
    AddDeforestation <- rbind(AddDeforestation, EU)
    AddDeforestation$region = factor(AddDeforestation$region,levels=regions28_EU)  
    AddDeforestation <- mutate(AddDeforestation, unit="km2")
    AddDeforestation=data.table(AddDeforestation)
    yy=seq(1970,2100)
    AddDeforestation = AddDeforestation[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('deforest_type','region','unit')]
    setnames(AddDeforestation,"V1","value")
    setnames(AddDeforestation,"V2","year")
    setcolorder(AddDeforestation,c("year","region","deforest_type","value","unit"))
  },
  error = function(error_condition) 
  { cat("The IMAGE file TOTADDDEFORAREA.OUT does not exist\n")
  }) # try

  LandCover <- NULL
  if(TIMER_version %in% c('TIMER_3_11','TIMER_3_2')) {data_dir = "/output/land_use"} else{data_dir="/output"}
  tryCatch({
    LandCover = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_scenario, data_dir, sep=""), 
                                       filename='LANDCOV.OUT', varname=NULL, 
                                       collist=list(landcover_type, regions27), 
                                       namecols=c('landcover_type','region'), novarname = TRUE)
    LandCover <- subset(LandCover, region != "dummy")
    EU <- inner_join(filter(LandCover, region=='WEU'), filter(LandCover, region=='CEU'), by=c("year", "landcover_type"))
    EU$region <- "EU"
    EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, landcover_type, value)
    EU$region = factor(EU$region, levels=regions28_EU)
    LandCover <- rbind(LandCover, EU)
    LandCover$region = factor(LandCover$region,levels=regions28_EU)  
    LandCover <- mutate(LandCover, unit="km2")
    LandCover=data.table(LandCover)
    yy=seq(1970,2100)
    LandCover = LandCover[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('landcover_type','region','unit')]
    setnames(LandCover,"V1","value")
    setnames(LandCover,"V2","year")
    setcolorder(LandCover,c("year","region","landcover_type","value","unit"))
  },
  error = function(error_condition) 
  { cat("The IMAGE file LANDCOV.OUT does not exist\n")
  }) # try

  #4.
  l <- list(CO2Spec=CO2Spec,ENEMISCO2=ENEMISCO2,ENEMISCH4=ENEMISCH4,ENEMISN2O=ENEMISN2O,INDEMISCO2=INDEMISCO2,
            INDEMISCH4=INDEMISCH4,INDEMISN2O=INDEMISN2O,HFC_reg=HFC_reg,PFC_reg=PFC_reg,
            LUEMCO2=LUEMCO2,LUEMCH4=LUEMCH4,LUEMN2O=LUEMN2O,
            # energy supply
            ElecFuelUseTot=ElecFuelUseTot, ElecProd=ElecProd, ElecCap=ElecCap, ElecCap_new=ElecCap_new,
            ElecProdSpec=ElecProdSpec, EnergyProd=EnergyProd, FinalEnergy=FinalEnergy, 
            CO2EPG_new=CO2EPG_new,ElecEffPct_new=ElecEffPct_new,ElecEffPct=ElecEffPct,
            H2FuelDem=H2FuelDem, H2Prod=H2Prod,
            ElecHeatCO2=ElecHeatCO2, ElecFuelUseTot=ElecFuelUseTot, ElecProd=ElecProd, ElecCap=ElecCap, EnergyProd=EnergyProd, FinalEnergy=FinalEnergy, 
            CO2EPG=CO2EPG, CO2EPG_new=CO2EPG_new,ElecEffPct_new=ElecEffPct_new,ElecEffPct=ElecEffPct,
            NetTrade=NetTrade,
            # buildings
            FloorSpace=FloorSpace,
            FinalEnergy_Residential=FinalEnergy_Residential, FinalEnergy_Residential_energy_carrier=FinalEnergy_Residential_energy_carrier,
            FinalEnergy_Residential_Appliances=FinalEnergy_Residential_Appliances,
            # transport
            TransportTravelCO2Emissions=TransportTravelCO2Emissions, TransportFreightCO2Emissions=TransportFreightCO2Emissions,
            FinalEnergy_Transport=FinalEnergy_Transport, FinalEnergy_trvl_Transport=FinalEnergy_trvl_Transport, FinalEnergy_frgt_Transport=FinalEnergy_frgt_Transport, FinalEnergy_carrier_trvl_Transport=FinalEnergy_carrier_trvl_Transport,
            PersonKilometers=PersonKilometers, 
            VehicleShare_cars=VehicleShare_cars, VehicleShare_busses=VehicleShare_busses, VehicleShare_trains=VehicleShare_trains, VehicleShare_aircrafts=VehicleShare_aircrafts,
            BiofuelShare_new_cars=BiofuelShare_new_cars, BiofuelShare_existing_cars=BiofuelShare_existing_cars,
            BlendingShareBio_cars_pkm=BlendingShareBio_cars_pkm, BlendingShareBio_new_cars_pkm=BlendingShareBio_new_cars_pkm, FuelUseFleet_trvl=FuelUseFleet_trvl, FuelUseFleet_frgt=FuelUseFleet_frgt,
            BlendingShareBio_energy_trvl=BlendingShareBio_energy_trvl, BlendingShareBio_energy_frgt=BlendingShareBio_energy_frgt, 
            ElectricShare_new_cars=ElectricShare_new_cars, ElectricShare_cars=ElectricShare_cars,
            ElectricShare_new_HvyT=ElectricShare_new_HvyT, ElectricShare_HvyT=ElectricShare_HvyT,
            EfficiencyTravel=EfficiencyTravel, EfficiencyFreight=EfficiencyFreight,
            EfficiencyFleet_new_cars=EfficiencyFleet_new_cars, EfficiencyFleet_new_cars_exclEV=EfficiencyFleet_new_cars_exclEV, EfficiencyFleet_new_cars_EV=EfficiencyFleet_new_cars_EV,
            EfficiencyFleet_cars=EfficiencyFleet_cars, 
            EfficiencyFleet_new_MedT=EfficiencyFleet_new_MedT, 
            EfficiencyFleet_new_HvyT=EfficiencyFleet_new_HvyT, EfficiencyFleet_HvyT=EfficiencyFleet_HvyT, 
            EfficiencyFleet_new_busses=EfficiencyFleet_new_busses, 
            CostPerKm_cars=CostPerKm_cars,
            # other
            CarbonCaptured=CarbonCaptured,
            EnergyTax_HvyT=EnergyTax_HvyT, EnergyTax_cars=EnergyTax_cars,
            Factor_HvyT=Factor_HvyT, Factor_cars=Factor_cars,
            HeatProduction=HeatProduction, HeatDemand=HeatDemand,
            # SDG
            ElecAcc=ElecAcc,
            # drivers
            TPES=TPES,
            POP=POP, GDP_MER=GDP_MER, GDP_PPP=GDP_PPP, IVA=IVA, 
            # AFOU
            ForestArea=ForestArea,
            AddDeforestation=AddDeforestation,
            LandCover=LandCover)
  
  
}
