library(gridExtra)
library(grid)
library(rmarkdown)  # render pdf
library(stringr)
library(rlang)
library(ggpubr)
# import functions to read in and manipulate TIMER functions
source('functions/Settings.R')
source('functions/Import_TIMER_output.R')
source('functions/Process_TIMER_output.R')

ProjectDir = "~/disks/y/ontwapps/Timer/Users/Mark"
Project = 'ClimatePolicies'
R_dir = paste(ProjectDir, Project, "R-scripts/TIMER_output", sep="/")
setwd(R_dir)

# This module makes graphs for the 'good practice policy' paper
# The first graph first creates three different result sets
# (1) Emission reductions per sector in the good practice policy scenario
# (2) Emission reduction per individual measure, each measure is a seperate scenario
# (3) Emission reductions per measure, but based on the total good practice policy scenario. So overlap
#    in these reductions is attributed to the different measures (assumptions)
# Definition of names
# Measures
name_Electricity_demand = "Electricity buildings and industry"
name_RENElectricty = "Renewable electricity" 
name_OilGas="Flaring and venting"
name_Industry="Industry efficiency"
name_FGases="F-gases"
name_Buildings="Buildings envelope"
name_Appliances="Appliances and lighting"
name_CAFEStandards="Fuel efficiency cars" 
name_ElectricCars="Electric cars" 
name_Deforestation="Deforestation"

Measures <- c(name_Electricity_demand, name_RENElectricty, name_OilGas, 
              name_Industry, name_FGases, name_Buildings, name_Appliances,
              name_CAFEStandards, name_ElectricCars, name_Deforestation,  " ")

Scenarios <- c("Current policies", "Good practice policies", "NDC range (2030)", "2-degree scenario (2.6 W/m2) with 66% probability")

# First import TIMER files for each scenario (current policies, good practice policies, 
#   and one scenario for each individual measure)
CurrentPolicies <- ImportTimerScenario('GPP_CurrentPolicies','SSP2')
GPPPolicies <- ImportTimerScenario('GPP_Total_fromCPS','SSP2')
GPPRENElectricity <- ImportTimerScenario('GPP_RENElectricity_fromCPS','SSP2')
GPPIndustry <- ImportTimerScenario('GPP_IndustryEfficiency_fromCPS','SSP2')
GPPBuildingCodes <- ImportTimerScenario('GPP_BuildingCodes_fromCPS','SSP2')
GPPAppliances <- ImportTimerScenario('GPP_AppliancesLight_fromCPS','SSP2')
GPPOilGas <- ImportTimerScenario('GPP_OilGas_fromCPS','SSP2')
GPPCAFEStandards <- ImportTimerScenario('GPP_CAFEStandards_fromCPS','SSP2')
GPPElectricCars_inclREN <- ImportTimerScenario('GPP_NewElectricCars_inclREN_fromCPS','SSP2')

# Create indicators based on TIMER files
CurrentPolicies_indicators <- ProcessTimerScenario(CurrentPolicies)
GPPPolicies_indicators <- ProcessTimerScenario(GPPPolicies)
GPPRENElectricity_indicators <- ProcessTimerScenario(GPPRENElectricity)
GPPIndustry_indicators <- ProcessTimerScenario(GPPIndustry)
GPPBuildingCodes_indicators <- ProcessTimerScenario(GPPBuildingCodes)
GPPAppliances_indicators <- ProcessTimerScenario(GPPAppliances)
GPPOilGas_indicators <- ProcessTimerScenario(GPPOilGas)
GPPCAFEStandards_indicators <- ProcessTimerScenario(GPPCAFEStandards)
GPPElectricCars_inclREN_indicators <- ProcessTimerScenario(GPPElectricCars_inclREN)

# replace f-gas emissions indicator for GPP policy scenario
GPPFGas_index <- read.table('data/FGases_index_GPP.csv', sep=";", header=TRUE, row.names=NULL)
tmp <- filter(CurrentPolicies_indicators$FGas_Reduction_index, region=="World", year>1990, year<=2030)
tmp <- inner_join(tmp, GPPFGas_index, by=c('year', 'region'))
tmp <- mutate(tmp, value=pmin(value.x, value.y))
GPPFGas_index <- select(tmp, year, region, value)

# read in IIASA data for deforestation
Deforestation_data <- read.table('data/IIASA deforestation results.csv', sep=";", header=TRUE, row.names=NULL)
names(Deforestation_data)[names(Deforestation_data) == 'scenario'] <- 'Scenario'
Deforestation_data$Scenario <- str_trim(Deforestation_data$Scenario)
Deforestation_data$indicator <- str_trim(Deforestation_data$indicator)
CurrentPolicies_Deforestation_index <- filter(Deforestation_data, Scenario=="Current policies", indicator=="Reduction of deforestation rate relative to 2010")
CurrentPolicies_Deforestation_index <- select(CurrentPolicies_Deforestation_index, year, region, value)
GPPPolicies_Deforestation_index <- filter(Deforestation_data, Scenario=="Good practice policies", indicator=="Reduction of deforestation rate relative to 2010")
GPPPolicies_Deforestation_index <- select(GPPPolicies_Deforestation_index, year, region, value)

# read in SSP2 2.6 (2C) scenario
SSP2_2_6 <- read.table('data/SSP2 2.6 scenario.csv', sep=";", header=TRUE, row.names=NULL)

# PREPARE DATA for graphs: 
# 1. select region, time period
# 2. outside model adjustments
# 1. select ghg emissions between 2010 and 2030 for current policies and good practice policies
CPS_Total <-filter(CurrentPolicies_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_Total <-filter(GPPPolicies_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
CPS_Total <- mutate(CPS_Total, Scenario="Current policies")
GPP_Total <- mutate(GPP_Total, Scenario="Good practice policies")
CPS_Total <- select(CPS_Total, year, value, Scenario)
GPP_Total <- select(GPP_Total, year, value, Scenario)

# keep unadjusted scenario projections
CPS_Total_before <- CPS_Total
GPP_Total_before <- GPP_Total

# 2a. change LULUCF CO2 emissions from IMAGE to IIASA projections
# - For IIASA: filter 2010-2030 emissions and add scenario names
CPS_LULUCFCO2_IIASA <- filter(Deforestation_data, indicator=="CO2 emissions LULUCF", Scenario=="Current policies")
GPP_LULUCFCO2_IIASA <- filter(Deforestation_data, indicator=="CO2 emissions LULUCF", Scenario=="Good practice policies")
CPS_LULUCFCO2_IIASA <- select(CPS_LULUCFCO2_IIASA, year, value, Scenario)
GPP_LULUCFCO2_IIASA <- select(GPP_LULUCFCO2_IIASA, year, value, Scenario)
CPS_LULUCFCO2_IIASA <- mutate(CPS_LULUCFCO2_IIASA, Scenario="Current policies IIASA")
GPP_LULUCFCO2_IIASA <- mutate(GPP_LULUCFCO2_IIASA, Scenario="Good practice policies IIASA")
# - PBL results from current policies list, including translation to CO2eq
CPS_LULUCFCO2_PBL <- filter(CurrentPolicies$LUEMCO2, year>=2010, year<=2030, source=="Total", region=="World")
CPS_LULUCFCO2_PBL$value <- (44/12)*10^3*CPS_LULUCFCO2_PBL$value
CPS_LULUCFCO2_PBL <- mutate(CPS_LULUCFCO2_PBL, Scenario="Current policies PBL")
CPS_LULUCFCO2_PBL <- select(CPS_LULUCFCO2_PBL, year, value, Scenario)
GPP_LULUCFCO2_PBL <- filter(GPPPolicies$LUEMCO2, year>=2010, year<=2030, source=="Total", region=="World")
GPP_LULUCFCO2_PBL$value <- (44/12)*10^3*GPP_LULUCFCO2_PBL$value
GPP_LULUCFCO2_PBL <- mutate(GPP_LULUCFCO2_PBL, Scenario="Good practice policies PBL")
GPP_LULUCFCO2_PBL <- select(GPP_LULUCFCO2_PBL, year, value, Scenario)
# - determine difference between IIASA and PBL LULUCF CO2 emissions for CPS and GPP
CPS_diff <- rbind(CPS_LULUCFCO2_IIASA, CPS_LULUCFCO2_PBL)
CPS_diff <- spread(CPS_diff, key=Scenario, value=value)
CPS_diff <- mutate(CPS_diff,value=`Current policies IIASA`-`Current policies PBL`)
CPS_diff <- mutate(CPS_diff, Scenario="Current policies LULUCF diff")
CPS_diff <- select(CPS_diff, year, value, Scenario)
GPP_diff <- rbind(GPP_LULUCFCO2_IIASA, GPP_LULUCFCO2_PBL)
GPP_diff <- spread(GPP_diff, key=Scenario, value=value)
GPP_diff <- mutate(GPP_diff,value=`Good practice policies IIASA`-`Good practice policies PBL`)
GPP_diff <- mutate(GPP_diff, Scenario="Good practice policies LULUCF diff")
GPP_diff <- select(GPP_diff, year, value, Scenario)
# - add the difference from total CPS and GPP emissions
CPS_tmp <- rbind(CPS_Total, CPS_diff)
CPS_tmp <- spread(CPS_tmp, Scenario, value) 
CPS_tmp <- mutate(CPS_tmp, value=`Current policies`+`Current policies LULUCF diff`)
CPS_Total <- select(CPS_tmp, year, value)
CPS_Total <- mutate(CPS_Total, Scenario="Current policies")
GPP_tmp <- rbind(GPP_Total, GPP_diff)
GPP_tmp <- spread(GPP_tmp, Scenario, value) 
GPP_tmp <- mutate(GPP_tmp, value=`Good practice policies`+`Good practice policies LULUCF diff`)
GPP_Total <- select(GPP_tmp, year, value)
GPP_Total <- mutate(GPP_Total, Scenario="Good practice policies")

# 2b. substract f-gases from good practice policies (outside model adjustments)
GPP_FGases_old <- filter(GPPPolicies_indicators$FGases, year>=2010, year<=2030, region=="World")
GPP_FGases_old <- mutate(GPP_FGases_old, Scenario='Good practice policies FGases old')
GPP_FGases_old <- select(GPP_FGases_old, year, value, Scenario)
GPP_FGases_new <- read.table('data/FGases_GPP.csv', sep=";", header=TRUE, row.names=NULL)
GPP_FGases_new <- filter(GPP_FGases_new, year>=2010, year<=2030)
GPP_FGases_new$Scenario <- "Good practice policies FGases new"
GPP_FGases_new <- select(GPP_FGases_new, year, value, Scenario)
GPP_tmp <- rbind(GPP_Total, GPP_FGases_old)
GPP_tmp <- rbind(GPP_tmp, GPP_FGases_new)
GPP_tmp <- spread(GPP_tmp, Scenario, value) 
GPP_Total <- mutate(GPP_tmp, value=`Good practice policies`-`Good practice policies FGases old`+`Good practice policies FGases new`)
GPP_Total <- select(GPP_Total, year, value)
GPP_Total <- mutate(GPP_Total, Scenario="Good practice policies")



# (1) PREPARE SECTOR REDUCTIONS
# Note: sum of emission levels do not add up to totals as some sector without reductions (e.g. bunkers) are left out
# 1. energy supply sector
# ENEMISCO2, ENEMISCH4, ENEMISN2O (energy_carrier="Total", sector="Energy transformation", "Power generation")
# ignornig ch4 and n2o (small)
CPS_elec <- filter(CurrentPolicies$ENEMISCO2, region=="World", year>=2015, year<=2030, energy_carrier=="Total", sector=="Energy transformation" | sector=="Power generation") %>% group_by(year) %>% summarise(value=sum(value))
CPS_elec$value <- CPS_elec$value*(44/12)*10^3
CPS_elec <- select(CPS_elec, year, value)
CPS_elec <- mutate(CPS_elec, sector="Electricity generation") %>% mutate(Scenario="Current policies")
GPP_elec <- filter(GPPPolicies$ENEMISCO2, region=="World", year>=2015, year<=2030, energy_carrier=="Total", sector=="Energy transformation" | sector=="Power generation") %>% group_by(year) %>% summarise(value=sum(value))
GPP_elec$value <- GPP_elec$value*(44/12)*10^3
GPP_elec <- select(GPP_elec, year, value)
GPP_elec <- mutate(GPP_elec, sector="Electricity generation") %>% mutate(Scenario="Good practice policies")
SectorResults <- rbind(CPS_elec, GPP_elec)
# 2. losses and leakages
# ENEMISCO2, ENEMISCH4, ENEMISN2O (energy_carrier="Total", sector="Losses/leakages")
# ignoring co2 and n2o (small)
CPS_FF <- filter(CurrentPolicies$ENEMISCH4, region=="World", year>=2015, year<=2030, energy_carrier=="Total", sector=="losses/leakages")
CPS_FF$value <- CPS_FF$value*GWP_CH4
CPS_FF <- select(CPS_FF, year, value)
CPS_FF <- mutate(CPS_FF, sector="Fossil fuel production") %>% mutate(Scenario="Current policies")
SectorResults <- rbind(SectorResults, CPS_FF)
GPP_FF <- filter(GPPPolicies$ENEMISCH4, region=="World", year>=2015, year<=2030, energy_carrier=="Total", sector=="losses/leakages")
GPP_FF$value <- GPP_FF$value*GWP_CH4
GPP_FF <- select(GPP_FF, year, value)
GPP_FF <- mutate(GPP_FF, sector="Fossil fuel production") %>% mutate(Scenario="Good practice policies")
SectorResults <- rbind(SectorResults, GPP_FF)
# 3. industry sector
# ENEMISCO2, ENEMISCH4, ENEMISN2O (energy_carrier="Total", sector="End-use industry", "End-use other")
# INDEMISCO2, INDEMISCH4, INDEMISN2O (energy_carrier="Total", industrial_process="total")
CPS_IN <- filter(CurrentPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030, main_sector=="Industry") %>% group_by(year, main_sector) %>% summarise(value=sum(value))
CPS_IN <- select(CPS_IN, year, value) %>% mutate(sector="Industry") %>% mutate(Scenario="Current policies")
CPS_IN <- as.data.frame(CPS_IN)
SectorResults <- rbind(SectorResults, CPS_IN)
GPP_IN <- filter(GPPPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030, main_sector=="Industry") %>% group_by(year, main_sector) %>% summarise(value=sum(value))
GPP_IN <- select(GPP_IN, year, value) %>% mutate(sector="Industry") %>% mutate(Scenario="Good practice policies")
GPP_IN <- as.data.frame(GPP_IN)
SectorResults <- rbind(SectorResults, GPP_IN)
# 4. buildings sector
# ENEMISCO2, ENEMISCH4, ENEMISN2O (energy_carrier="Total", sector="End-use residential", "End-use services")
CPS_B <- filter(CurrentPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030, main_sector=="Buildings") %>% group_by(year, main_sector) %>% summarise(value=sum(value))
CPS_B <- select(CPS_B, year, value) %>% mutate(sector="Buildings") %>% mutate(Scenario="Current policies")
CPS_B <- as.data.frame(CPS_B)
SectorResults <- rbind(SectorResults, CPS_B)
GPP_B <- filter(GPPPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030, main_sector=="Buildings") %>% group_by(year, main_sector) %>% summarise(value=sum(value))
GPP_B <- select(GPP_B, year, value) %>% mutate(sector="Buildings") %>% mutate(Scenario="Good practice policies")
GPP_B <- as.data.frame(GPP_B)
SectorResults <- rbind(SectorResults, GPP_B)
# 5. transport sector
# ENEMISCO2, ENEMISCH4, ENEMISN2O (energy_carrier="Total", sector="End-use transport")
CPS_T <- filter(CurrentPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030, main_sector=="Transport") %>% group_by(year, main_sector) %>% summarise(value=sum(value))
CPS_T <- select(CPS_T, year, value) %>% mutate(sector="Transport") %>% mutate(Scenario="Current policies")
CPS_T <- as.data.frame(CPS_T)
SectorResults <- rbind(SectorResults, CPS_T)
GPP_T <- filter(GPPPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030, main_sector=="Transport") %>% group_by(year, main_sector) %>% summarise(value=sum(value))
GPP_T <- select(GPP_T, year, value) %>% mutate(sector="Transport") %>% mutate(Scenario="Good practice policies")
GPP_T <- as.data.frame(GPP_T)
SectorResults <- rbind(SectorResults, GPP_T)
# 6. f-gases
CPS_FG <- filter(CurrentPolicies_indicators$FGases, region=="World", year>=2015, year<=2030)
CPS_FG <- select(CPS_FG, year, value) %>% mutate(sector="F-gases") %>% mutate(Scenario="Current policies")
CPS_FG <- as.data.frame(CPS_FG) 
SectorResults <- rbind(SectorResults, CPS_FG)
GPP_FG <- read.table('data/FGases_GPP.csv', sep=";", header=TRUE, row.names=NULL)
GPP_FG <- filter(GPP_FG, year>=2015, year <=2030)
GPP_FG <- select(GPP_FG, year, value) %>% mutate(sector="F-gases") %>% mutate(Scenario="Good practice policies")
GPP_FG <- as.data.frame(GPP_FG) 
SectorResults <- rbind(SectorResults, GPP_FG)
# 7. lulucf sector
LULUCF <- filter(Deforestation_data, year>=2015, year<=2030, indicator=="CO2 emissions LULUCF")
LULUCF <- mutate(LULUCF, sector="LULUCF") %>% select(year, value, sector, Scenario)
SectorResults <- rbind(SectorResults, LULUCF)
# Calculate reductions per sector
SectorResults <- spread(SectorResults, key=Scenario, value=value)
SectorResults <- mutate(SectorResults, Reduction=`Current policies`-`Good practice policies`)
SectorResults <- gather(SectorResults, `Current policies`, `Good practice policies`, `Reduction`, key=Scenario, value=value)

# (2) PREPARE individual measure REDUCTIONS
# - first select period 2010-2030 and add scenario
CPS <- filter(CPS_Total_before, year>=2010, year<=2030)
GPP <- filter(GPP_Total_before, year>=2010, year<=2030)
RENElectricity <-filter(GPPRENElectricity_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
Industry <-filter(GPPIndustry_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
BuildingCodes <-filter(GPPBuildingCodes_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
AppliancesLight <-filter(GPPAppliances_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
OilGas <-filter(GPPOilGas_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
CAFEStandards <-filter(GPPCAFEStandards_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
ElectricCars <-filter(GPPElectricCars_inclREN_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
FGases <- read.table('data/FGases_GPP.csv', sep=";", header=TRUE, row.names=NULL)
FGases <- filter(FGases, year>=2010, year<=2030)
Deforestation <- filter(Deforestation_data, indicator=="CO2 emissions LULUCF") %>% select(year, value, Scenario)
CPS <- mutate(CPS, Scenario="Current policies")
GPP <- mutate(GPP, Scenario="Good practice policies")
RENElectricity <- mutate(RENElectricity, Scenario="Renewable electricity production") %>% select(year, value, Scenario)
OilGas <- mutate(OilGas, Scenario="Oil and gas production") %>% select(year, value, Scenario)
Industry <- mutate(Industry, Scenario="Industry") %>% select(year, value, Scenario)
BuildingCodes <- mutate(BuildingCodes, Scenario="Building codes") %>% select(year, value, Scenario)
AppliancesLight <- mutate(AppliancesLight, Scenario="Appliances and light") %>% select(year, value, Scenario)
CAFEStandards <- mutate(CAFEStandards, Scenario="CAFE Standards") %>% select(year, value, Scenario)
ElectricCars <- mutate(ElectricCars, Scenario="Electric cars") %>% select(year, value, Scenario)
FGases <- mutate(FGases, Scenario="F-Gases") %>% select(year, value, Scenario)

red_RENElectricity <- rbind(CPS, RENElectricity)
red_RENElectricity <- spread(red_RENElectricity, key=Scenario, value=value)
red_RENElectricity <- mutate(red_RENElectricity, reduction=`Current policies`- `Renewable electricity production`)
red_RENElectricity <- gather(red_RENElectricity, `Current policies`, `Renewable electricity production`, `reduction`, key=`Scenario`, value=`value`)#FGases <-mutate

red_OilGas <- rbind(CPS, OilGas)
red_OilGas <- spread(red_OilGas, key=Scenario, value=value)
red_OilGas <- mutate(red_OilGas, reduction=`Current policies`- `Oil and gas production`)
red_OilGas <- gather(red_OilGas, `Current policies`, `Oil and gas production`, `reduction`, key=`Scenario`, value=`value`)

red_Industry <- rbind(CPS, Industry)
red_Industry <- spread(red_Industry, key=Scenario, value=value)
red_Industry <- mutate(red_Industry, reduction=`Current policies`- `Industry`)
red_Industry <- gather(red_Industry, `Current policies`, `Industry`, `reduction`, key=`Scenario`, value=`value`)

red_BuildingCodes <- rbind(CPS, BuildingCodes)
red_BuildingCodes <- spread(red_BuildingCodes, key=Scenario, value=value)
red_BuildingCodes <- mutate(red_BuildingCodes, reduction=`Current policies`- `Building codes`)
red_BuildingCodes <- gather(red_BuildingCodes, `Current policies`, `Building codes`, `reduction`, key=`Scenario`, value=`value`)

red_AppliancesLight <- rbind(CPS, AppliancesLight)
red_AppliancesLight <- spread(red_AppliancesLight, key=Scenario, value=value)
red_AppliancesLight <- mutate(red_AppliancesLight, reduction=`Current policies`- `Appliances and light`)
red_AppliancesLight <- gather(red_AppliancesLight, `Current policies`, `Appliances and light`, `reduction`, key=`Scenario`, value=`value`)

red_CAFEStandards <- rbind(CPS, CAFEStandards)
red_CAFEStandards <- spread(red_CAFEStandards, key=Scenario, value=value)
red_CAFEStandards <- mutate(red_CAFEStandards, reduction=`Current policies`- `CAFE Standards`)
red_CAFEStandards <- gather(red_CAFEStandards, `Current policies`, `CAFE Standards`, `reduction`, key=`Scenario`, value=`value`)

red_ElectricCars <- rbind(RENElectricity, ElectricCars)
red_ElectricCars <- spread(red_ElectricCars, key=Scenario, value=value)
red_ElectricCars <- mutate(red_ElectricCars, reduction=`Renewable electricity production`- `Electric cars`)
red_ElectricCars <- gather(red_ElectricCars, `Renewable electricity production`, `Electric cars`, `reduction`, key=`Scenario`, value=`value`)

CPS_FGases <- filter(CurrentPolicies_indicators$FGases, year>=2010, year<=2030, region=="World") %>% select(year, value)
CPS_FGases <- mutate(CPS_FGases, Scenario="Current policies F-Gases")
red_FGases <- rbind(CPS_FGases, FGases)                     
red_FGases <- spread(red_FGases, key=Scenario, value=value)
red_FGases <- mutate(red_FGases, reduction=`Current policies F-Gases`- `F-Gases`)
red_FGases <- gather(red_FGases, `Current policies F-Gases`, `F-Gases`, `reduction`, key=`Scenario`, value=`value`)

red_Deforestation <- spread(Deforestation, key=Scenario, value=value)
red_Deforestation <- mutate(red_Deforestation, reduction=`Current policies`-`Good practice policies`)
red_Deforestation <- gather(red_Deforestation, `Current policies`, `Good practice policies`, `reduction`, key="Scenario", value="value")

CPS_adj <- filter(CPS_Total, year>=2010, year<=2030)
GPP_adj <- filter(GPP_Total, year>=2010, year<=2030)
red_Total <- rbind(CPS_adj, GPP_adj)
red_Total <- spread(red_Total, key=Scenario, value=value)
red_Total <- mutate(red_Total, reduction=`Current policies`- `Good practice policies`)
red_Total <- gather(red_Total, `Current policies`, `Good practice policies`, `reduction`, key=`Scenario`, value=`value`)

red_Total_before_overlap <- rbind(red_RENElectricity, red_OilGas) %>% rbind(red_Industry) %>%
                            rbind(red_BuildingCodes) %>% rbind(red_AppliancesLight) %>%
                            rbind(red_CAFEStandards) %>% rbind(red_ElectricCars) %>%
                            rbind(red_FGases) %>% rbind(red_Deforestation) %>%
                            filter(Scenario=="reduction")
red_Total_before_overlap <- filter(red_Total_before_overlap, year>=2015, year<=2030) %>%
                            group_by(year) %>% 
                            summarise(value=sum(value))
red_Total_before_overlap <- mutate(red_Total_before_overlap, Scenario="reduction")

# (2) PREPARE measure REDUCTIONS using the total good practice policy scenario for figure 1a
#     The overlap (= Sum individual emissions reductions (relative to CPS) from measures (scenarios) 
#                   -/- emission reductions from good practice policy scenario )


#
# 1. electricity reductions from demand sector
CPS_elec <- filter(CurrentPolicies$ENEMISCO2, year>=2015, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total") %>% select(year, value)
CPS_elec <- mutate(CPS_elec, measure="Current policies")
Industry_elec <- filter(GPPIndustry$ENEMISCO2, year>=2015, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total") %>% select(year, value)
Industry_elec <- mutate(Industry_elec, measure="Industry efficiency")
Appliances_elec <- filter(GPPAppliances$ENEMISCO2, year>=2015, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total") %>% select(year, value)
Appliances_elec <- mutate(Appliances_elec, measure="Appliances")
BuildingCodes_elec <- filter(GPPBuildingCodes$ENEMISCO2, year>=2015, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total") %>% select(year, value)
BuildingCodes_elec <- mutate(BuildingCodes_elec, measure="Building codes")
red_elec_demand <- rbind(CPS_elec, Industry_elec) %>% rbind(Appliances_elec) %>% rbind(BuildingCodes_elec)
red_elec_demand <- spread(red_elec_demand, key=measure, value=value)
red_elec_demand <- mutate(red_elec_demand, reduction=(`Current policies`-`Industry efficiency`)+(`Current policies`-`Appliances`)+(`Current policies`-`Building codes`))
red_elec_demand <- gather(red_elec_demand, `Current policies`,`Industry efficiency`,`Appliances`,`Building codes`, `reduction`, key="measure", value="value")
red_elec_demand$value <- red_elec_demand$value*(44/12)*10^3
red_elec_demand <- filter(red_elec_demand, measure=="reduction")
red_elec_demand$measure <- name_Electricity_demand
# Oil and gas production: methane reductions energy/industry: CPS -/- OilGas
if (file.exists('tmp1'))
{ rm(tmp1); rm(tmp2)
}
tmp1 <- filter(CurrentPolicies$ENEMISCH4, region=="World", 
               (sector %in% c('Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure="Current policies")
tmp2 <- filter(GPPOilGas$ENEMISCH4, region=="World", 
               (sector %in% c('Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure=name_OilGas)
red_OilGas_cor_overlap <- rbind(tmp1, tmp2)
red_OilGas_cor_overlap <- spread(red_OilGas_cor_overlap, key=measure, value=value)
red_OilGas_cor_overlap <- setNames(red_OilGas_cor_overlap, c('year', 'CPS', 'GPP'))
red_OilGas_cor_overlap <- mutate(red_OilGas_cor_overlap, reduction=`CPS`- `GPP`)
red_OilGas_cor_overlap <- gather(red_OilGas_cor_overlap, `CPS`,`GPP`, `reduction`, key="measure", value="value")
red_OilGas_cor_overlap <- filter(red_OilGas_cor_overlap, measure=="reduction")
red_OilGas_cor_overlap$measure <- name_OilGas
red_OilGas_cor_overlap$value <- red_OilGas_cor_overlap$value*GWP_CH4
# Industry efficiency: demand CO2: CPS -/- Industry
rm(tmp1); rm(tmp2)
tmp1 <- filter(CurrentPolicies$ENEMISCO2, region=="World", 
               #!(sector %in% c('Power generation', 'Total')),
               sector %in% c('Total'),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure="Current policies")
tmp2 <- filter(GPPIndustry$ENEMISCO2, region=="World", 
               #!(sector %in% c('Power generation', 'Total')),
               sector %in% c('Total'),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure=name_Industry)
red_Industry_cor_overlap <- rbind(tmp1, tmp2)
red_Industry_cor_overlap <- spread(red_Industry_cor_overlap, key=measure, value=value)
red_Industry_cor_overlap <- setNames(red_Industry_cor_overlap, c('year', 'CPS', 'GPP'))
red_Industry_cor_overlap <- mutate(red_Industry_cor_overlap, reduction=`CPS`- `GPP`)
red_Industry_cor_overlap <- gather(red_Industry_cor_overlap, `CPS`,`GPP`, `reduction`, key="measure", value="value")
red_Industry_cor_overlap <- filter(red_Industry_cor_overlap, measure=="reduction")
red_Industry_cor_overlap$measure <- name_Industry
red_Industry_cor_overlap$value <- red_Industry_cor_overlap$value*(44/12)*10^3
# F-gases: f-gases: CPS-/-FGases
red_FGases_cor_overlap <- filter(red_FGases, year>=2015, year<=2030, Scenario=="reduction")
red_FGases_cor_overlap <- setNames(red_FGases_cor_overlap, c('year', 'measure', 'value'))
red_FGases_cor_overlap$measure <- name_FGases
# Building envelope: demand CO2: CPS -/- BuildingCodes
#tmp1 <- filter(CurrentPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030,
rm(tmp1); rm(tmp2)
tmp1 <- filter(CurrentPolicies$ENEMISCO2, region=="World", 
               !(sector %in% c('Power generation', 'Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
        group_by(year) %>% 
        summarise(value=sum(value)) %>% 
        mutate(measure="Current policies")
tmp2 <- filter(GPPBuildingCodes$ENEMISCO2, region=="World", 
               !(sector %in% c('Power generation', 'Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
        group_by(year) %>% 
        summarise(value=sum(value)) %>% 
        mutate(measure=name_Buildings)
red_BuildingCodes_cor_overlap <- rbind(tmp1, tmp2)
red_BuildingCodes_cor_overlap <- spread(red_BuildingCodes_cor_overlap, key=measure, value=value)
red_BuildingCodes_cor_overlap <- setNames(red_BuildingCodes_cor_overlap, c('year', 'GPP', 'CPS'))
red_BuildingCodes_cor_overlap <- mutate(red_BuildingCodes_cor_overlap, reduction=`CPS`- `GPP`)
red_BuildingCodes_cor_overlap <- gather(red_BuildingCodes_cor_overlap, `CPS`,`GPP`, `reduction`, key="measure", value="value")
red_BuildingCodes_cor_overlap <- filter(red_BuildingCodes_cor_overlap, measure=="reduction")
red_BuildingCodes_cor_overlap$measure <- name_Buildings
red_BuildingCodes_cor_overlap$value <- red_BuildingCodes_cor_overlap$value*(44/12)*10^3
# Appliances: supply CO2: CPS -/- Appliances
#tmp1 <- filter(CurrentPolicies_indicators$EMISCO2EQ, region=="World", year>=2015, year<=2030,
rm(tmp1); rm(tmp2)
tmp1 <- filter(CurrentPolicies$ENEMISCO2, region=="World", 
               sector %in% c('Power generation'),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure="Current policies")
tmp2 <- filter(GPPAppliances$ENEMISCO2, region=="World", 
               sector %in% c('Power generation'),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure=name_Appliances)
red_Appliances_cor_overlap <- rbind(tmp1, tmp2)
red_Appliances_cor_overlap <- spread(red_Appliances_cor_overlap, key=measure, value=value)
red_Appliances_cor_overlap <- setNames(red_Appliances_cor_overlap, c('year', 'GPP','CPS'))
red_Appliances_cor_overlap <- mutate(red_Appliances_cor_overlap, reduction=`CPS`- `GPP`)
red_Appliances_cor_overlap <- gather(red_Appliances_cor_overlap, `CPS`,`GPP`, `reduction`, key="measure", value="value")
red_Appliances_cor_overlap <- filter(red_Appliances_cor_overlap, measure=="reduction")
red_Appliances_cor_overlap$measure <- name_Appliances
red_Appliances_cor_overlap$value <- red_Appliances_cor_overlap$value*(44/12)*10^3
# CAFE standards: electricity/demand CO2: CPS -/- CAFEStandards
rm(tmp1); rm(tmp2)
tmp1 <- filter(CurrentPolicies$ENEMISCO2, region=="World", 
               (sector %in% c('Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure="Current policies")
tmp2 <- filter(GPPCAFEStandards$ENEMISCO2, region=="World", 
               !(sector %in% c('Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure=name_CAFEStandards)
red_CAFEStandards_cor_overlap <- rbind(tmp1, tmp2)
red_CAFEStandards_cor_overlap <- spread(red_CAFEStandards_cor_overlap, key=measure, value=value)
red_CAFEStandards_cor_overlap <- setNames(red_CAFEStandards_cor_overlap, c('year', 'CPS', 'GPP'))
red_CAFEStandards_cor_overlap <- mutate(red_CAFEStandards_cor_overlap, reduction=`CPS`- `GPP`)
red_CAFEStandards_cor_overlap <- gather(red_CAFEStandards_cor_overlap, `CPS`,`GPP`, `reduction`, key="measure", value="value")
red_CAFEStandards_cor_overlap <- filter(red_CAFEStandards_cor_overlap, measure=="reduction")
red_CAFEStandards_cor_overlap$measure <- name_CAFEStandards
red_CAFEStandards_cor_overlap$value <- red_CAFEStandards_cor_overlap$value*(44/12)*10^3
# Electric vehicles: electricity/demand GHG: RENElectricity -/- ElectricCars
rm(tmp1); rm(tmp2)
tmp1 <- filter(GPPRENElectricity$ENEMISCO2, region=="World", 
               (sector %in% c('Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure="Current policies")
tmp2 <- filter(GPPElectricCars_inclREN$ENEMISCO2, region=="World", 
               !(sector %in% c('Total')),
               energy_carrier=="Total",
               year>=2015, year<=2030) %>%
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  mutate(measure=name_ElectricCars)
red_ElectricCars_cor_overlap <- rbind(tmp1, tmp2)
red_ElectricCars_cor_overlap <- spread(red_ElectricCars_cor_overlap, key=measure, value=value)
red_ElectricCars_cor_overlap <- setNames(red_ElectricCars_cor_overlap, c('year', 'CPS', 'GPP'))
red_ElectricCars_cor_overlap <- mutate(red_ElectricCars_cor_overlap, reduction=`CPS`- `GPP`)
red_ElectricCars_cor_overlap <- gather(red_ElectricCars_cor_overlap, `CPS`,`GPP`, `reduction`, key="measure", value="value")
red_ElectricCars_cor_overlap <- filter(red_ElectricCars_cor_overlap, measure=="reduction")
red_ElectricCars_cor_overlap$measure <- name_ElectricCars
red_ElectricCars_cor_overlap$value <- red_ElectricCars_cor_overlap$value*(44/12)*10^3
# Deforestation
red_Deforestation_cor_overlap <- filter(red_Deforestation, year>=2015, year<=2030, Scenario=="reduction")
red_Deforestation_cor_overlap <- setNames(red_Deforestation_cor_overlap, c('year', 'measure', 'value'))
red_Deforestation_cor_overlap$measure <- name_Deforestation
# Renewable electricity
red_RENElectricity_cor_overlap <- rbind(red_OilGas_cor_overlap, red_Industry_cor_overlap) %>% 
                                  rbind(red_FGases_cor_overlap) %>% 
                                  rbind(red_BuildingCodes_cor_overlap) %>% rbind(red_Appliances_cor_overlap) %>%
                                  rbind(red_CAFEStandards_cor_overlap) %>% 
                                  rbind(red_ElectricCars_cor_overlap) %>% rbind(red_Deforestation_cor_overlap)
red_RENElectricity_cor_overlap <- group_by(red_RENElectricity_cor_overlap, year) %>% 
                                  summarise(value=sum(value)) %>% 
                                  mutate(measure="sum other") %>%
                                  filter(measure=="sum other")
red_GPP_total_tmp <- red_Total %>% filter(year>=2015, year<=2030, Scenario=="reduction") %>% setNames(c('year', 'measure', 'value'))
red_GPP_total_tmp$measure <- "total"
red_RENElectricity_cor_overlap <- rbind(red_RENElectricity_cor_overlap, red_GPP_total_tmp) 
red_RENElectricity_cor_overlap <- spread(red_RENElectricity_cor_overlap, key=measure, value=value) %>%
                                  mutate(reduction=`total`-`sum other`) %>%
                                  gather(`sum other`, `total`, `reduction`, key=measure, value=value) %>%
                                  filter(measure=="reduction")
red_RENElectricity_cor_overlap$measure <- name_RENElectricty 



# **************** FIGURE 1a *****************************
  # PREPARE DATA FOR FIGURE 1a
  # graphs consists of two lines: current policies and good practice policies
  # and nine areas of reductions
# PREPARE data for figure 1a
CPS_fig1a_1 <- select(CPS_Total, year, value, Scenario)
GPP_fig1a_1 <- select(GPP_Total, year, value, Scenario)
NDC_fig1a_1 <- CPS_fig1a_1
NDC_fig1a_1$value <- 0
NDC_fig1a_1$Scenario <- "NDC range (2030)"
TwoC_fig1a_1 <- SSP2_2_6 %>% mutate(Scenario="2-degree scenario (2.6 W/m2) with 66% probability") %>%
                             filter(year>=2010, year<=2030)
TwoC_fig1a_1$value[1:9]=GPP_fig1a_1$value[1:9]

# collect data for lines graph
data_fig1a_1 <- rbind(CPS_fig1a_1, GPP_fig1a_1) %>% rbind(NDC_fig1a_1) %>% rbind(TwoC_fig1a_1)
data_fig1a_1$Scenario = factor(data_fig1a_1$Scenario, levels=Scenarios)
data_fig1a_1$value <- data_fig1a_1$value*10^-3
# collect data for areas graph
data_fig1a_2 <- #red_elec_demand %>% rbind(red_RENElectricity_cor_overlap) %>% 
                red_RENElectricity_cor_overlap %>% 
                rbind(red_OilGas_cor_overlap) %>% rbind(red_Industry_cor_overlap) %>% 
                rbind(red_FGases_cor_overlap) %>% rbind(red_BuildingCodes_cor_overlap) %>% 
                rbind(red_Appliances_cor_overlap) %>%
                rbind(red_CAFEStandards_cor_overlap) %>% rbind(red_ElectricCars_cor_overlap) %>%
                rbind(red_Deforestation_cor_overlap)
# total reductions
#GPP_fig1a_2 <-  mutate(GPP_Total, scenario=replace(Scenario, Scenario=="Good practice policies", " ")) %>% as.data.frame()
tmp_GPP_total <- GPP_Total %>% filter(year>=2015, year<=2030, Scenario=="Good practice policies") %>% setNames(c('year', 'value', 'measure'))
tmp_GPP_total$measure <- " "
data_fig1a_2 <- rbind(data_fig1a_2, tmp_GPP_total)
#data_fig1a_2$value <- lapply(data_fig1a_2$value, function(x) ifelse(x<0, 0, x))
min <- 0
data_fig1a_2$value[data_fig1a_2$value < min] <- min

# show data in GtCO2eq
data_fig1a_2$value <- data_fig1a_2$value*10^-3

#FIGURE 1a graph
data_fig1a_2$measure = factor(data_fig1a_2$measure, levels=Measures)
fig1a_left <- ggplot() +
  geom_area(data=data_fig1a_2, aes(x=year, y=value, fill=measure)) +
  geom_line(data=data_fig1a_1, aes(x=year, y=value, linetype=Scenario, colour=Scenario), size=1.5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  # use blues palete (brewer), but white for total GPP policies
  scale_fill_manual("Reduction", values = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", 
                                             "#4292c6", "#2171b5", "#08519c", "#08306b", "white")) +
  scale_colour_manual("Scenario", values = c("black","black", "white", "darkolivegreen4")) + # colour lines 
  scale_y_continuous(breaks=seq(0,60,10)) +
  labs(x = "year", y = "GtCO2eq") +
  theme(axis.title = element_text(face="bold", size=30)) +
  theme(axis.text = element_text(face="bold", size=30)) +
  theme(legend.text=element_text(size=25)) +
  theme(legend.title=element_text(size=25))
  #scale_fill_brewer(palette="Blues")

# https://magesblog.com/post/2015-04-14-plotting-tables-alsongside-charts-in-r/
SummaryTable <- data.frame(
  Policy_Action=c(name_RENElectricty, name_OilGas, name_Industry, name_FGases,
                  name_Buildings, name_Appliances,  name_CAFEStandards, name_ElectricCars, 
                  name_Deforestation, " ", "Total"),
  Reduction_1=c(#filter(red_elec_demand, year==2030)$value*10^-3,
                filter(red_RENElectricity_cor_overlap, year==2030)$value*10^-3, 
                filter(red_OilGas_cor_overlap, year==2030)$value*10^-3, 
                filter(red_Industry_cor_overlap, year==2030)$value*10^-3,
                filter(red_FGases_cor_overlap, year==2030)$value*10^-3,
                filter(red_BuildingCodes_cor_overlap, year==2030)$value*10^-3,
                filter(red_AppliancesLight, Scenario=="reduction", year==2030)$value*10^-3,
                filter(red_CAFEStandards_cor_overlap, year==2030)$value*10^-3, 
                filter(red_ElectricCars_cor_overlap, year==2030)$value*10^-3, 
                filter(red_Deforestation_cor_overlap, year==2030)$value*10^-3,
                0,
                filter(red_Total, Scenario=="reduction", year==2030)$value*10^-3),
  Reduction_2=c(#0,
    filter(red_RENElectricity, Scenario=="reduction", year==2030)$value*10^-3, 
    filter(red_OilGas, Scenario=="reduction", year==2030)$value*10^-3, 
    filter(red_Industry, Scenario=="reduction", year==2030)$value*10^-3,
    filter(red_FGases, Scenario=="reduction", year==2030)$value*10^-3,
    filter(red_BuildingCodes, Scenario=="reduction", year==2030)$value*10^-3,
    filter(red_AppliancesLight, Scenario=="reduction", year==2030)$value*10^-3,
    filter(red_CAFEStandards, Scenario=="reduction", year==2030)$value*10^-3, 
    filter(red_ElectricCars, Scenario=="reduction", year==2030)$value*10^-3, 
    filter(red_Deforestation, Scenario=="reduction", year==2030)$value*10^-3,
    0,
    filter(red_Total_before_overlap, Scenario=="reduction", year==2030)$value*10^-3)
)
SummaryTable$Reduction_1 <- format(SummaryTable$Reduction_1, digits=1)
SummaryTable$Reduction_2 <- format(SummaryTable$Reduction_2, digits=1)
#SummaryTable$Reduction_1[1] <- ""
SummaryTable$Reduction_1[10] <- ""
SummaryTable$Reduction_2[10] <- ""


names(SummaryTable) <- c("Reductions\nper policy", "Corrected\nfor overlap", "Individual\npolicies")
#hj <- matrix(c(0, 1, 1), ncol=3, nrow=11, byrow=TRUE)
#x <- matrix(c(0, 1, 1), ncol=3, nrow=11, byrow=TRUE)
#tt <- ttheme_default(colhead=list(fg_params = list(hjust=0, x=0.01)), 
#                     core=list(fg_params=list(hjust = as.vector(hj), 
#                                              x = as.vector(x))))
#tbl <- tableGrob(SummaryTable, rows=NULL, theme=tt)
#lay<-rbind(c(1,1,1,1,2,2), c(1,1,1,1,2,2))
#fig1a <- grid.arrange(fig1a_left, tbl, nrow=2, as.table=TRUE, layout_matrix=lay) #+ theme(panel.background = element_blank())
#+ theme_bw()


#tbl <- ggtexttable(SummaryTable, rows = NULL, theme = ttheme("mOrange"))
tbl <- ggtexttable(SummaryTable, rows = NULL, theme = ttheme("classic"))
fig1a <- ggarrange(fig1a_left, tbl,ncol = 2, nrow = 1, widths = c(1.2, 0.4))
plot.new()
plot(fig1a)
arrows(1, 1.1, 5, 5.1, length=1, code=3)
ggsave(file=paste("graphs/figure1.png",sep=""),fig1a,width=25,height=10,dpi=400)

# FIGURE 2

# add scenario name as column
CurrentPolicies_indicators$RenElecShare <- mutate(CurrentPolicies_indicators$RenElecShare, Scenario="Current policies")
CurrentPolicies_indicators$OilGas_Intensity <- mutate(CurrentPolicies_indicators$OilGas_Intensity, Scenario="Current policies")
CurrentPolicies_indicators$IndustryEfficiency <- mutate(CurrentPolicies_indicators$IndustryEfficiency, Scenario="Current policies")
CurrentPolicies_indicators$FGas_Reduction_index <- mutate(CurrentPolicies_indicators$FGas_Reduction_index, Scenario="Current policies")
CurrentPolicies_indicators$Residential_FinalEnergy_m2 <- mutate(CurrentPolicies_indicators$Residential_FinalEnergy_m2, Scenario="Current policies")
CurrentPolicies_indicators$Appliances_FinalEnergy_capita <- mutate(CurrentPolicies_indicators$Appliances_FinalEnergy_capita, Scenario="Current policies")
CurrentPolicies_indicators$FuelUse_pkm_cars <- mutate(CurrentPolicies_indicators$FuelUse_pkm_cars, Scenario="Current policies")
CurrentPolicies_indicators$ElectricCars_share <- mutate(CurrentPolicies_indicators$ElectricCars_share, Scenario="Current policies")
CurrentPolicies_Deforestation_index <- mutate(CurrentPolicies_Deforestation_index, Scenario="Current policies")

# add scenario name as column
GPPPolicies_indicators$RenElecShare <- mutate(GPPPolicies_indicators$RenElecShare, Scenario="Good practice policies")
GPPPolicies_indicators$OilGas_Intensity <- mutate(GPPPolicies_indicators$OilGas_Intensity, Scenario="Good practice policies")
GPPPolicies_indicators$IndustryEfficiency <- mutate(GPPPolicies_indicators$IndustryEfficiency, Scenario="Good practice policies")
GPPFGas_index <- mutate(GPPFGas_index, Scenario="Good practice policies")
GPPPolicies_indicators$Residential_FinalEnergy_m2 <- mutate(GPPPolicies_indicators$Residential_FinalEnergy_m2, Scenario="Good practice policies")
GPPPolicies_indicators$Appliances_FinalEnergy_capita <- mutate(GPPPolicies_indicators$Appliances_FinalEnergy_capita, Scenario="Good practice policies")
GPPPolicies_indicators$FuelUse_pkm_cars <- mutate(GPPPolicies_indicators$FuelUse_pkm_cars, Scenario="Good practice policies")
GPPPolicies_indicators$ElectricCars_share <- mutate(GPPPolicies_indicators$ElectricCars_share, Scenario="Good practice policies")
GPPPolicies_Deforestation_index <- mutate(GPPPolicies_Deforestation_index, Scenario="Good practice policies")

# merge current and good practice policies
CPS_RenElecShare <- CurrentPolicies_indicators$RenElecShare
CPS_RenElecShare$value <- CPS_RenElecShare$value*100
GPP_RenElecShare <- GPPPolicies_indicators$RenElecShare
GPP_RenElecShare$value <- GPP_RenElecShare$value*100
RenElecShare <- rbind(CPS_RenElecShare, GPP_RenElecShare)
OilGas_Intensity <- rbind(CurrentPolicies_indicators$OilGas_Intensity, GPPPolicies_indicators$OilGas_Intensity)
IndustryEfficiency <- rbind(CurrentPolicies_indicators$IndustryEfficiency, GPPPolicies_indicators$IndustryEfficiency)
FGas_Reduction_index <- rbind(CurrentPolicies_indicators$FGas_Reduction_index, GPPFGas_index)
Residential_FinalEnergy_m2 <- rbind(CurrentPolicies_indicators$Residential_FinalEnergy_m2, GPPPolicies_indicators$Residential_FinalEnergy_m2)
Appliances_FinalEnergy_capita <- rbind(CurrentPolicies_indicators$Appliances_FinalEnergy_capita, GPPPolicies_indicators$Appliances_FinalEnergy_capita)
FuelUse_pkm_cars <- rbind(CurrentPolicies_indicators$FuelUse_pkm_cars, GPPPolicies_indicators$FuelUse_pkm_cars)
#ElectricCars_share <- rbind(CurrentPolicies_indicators$ElectricCars_share, GPPPolicies_indicators$ElectricCars_share)
CPS_ElectricCars_share <- CurrentPolicies_indicators$ElectricCars_share
CPS_ElectricCars_share$value <- CPS_ElectricCars_share$value*100
GPP_ElectricCars_share <- GPPPolicies_indicators$ElectricCars_share
GPP_ElectricCars_share$value <- GPP_ElectricCars_share$value*100
ElectricCars_share <- rbind(CPS_ElectricCars_share, GPP_ElectricCars_share)
#Deforestation_index <- rbind(CurrentPolicies_Deforestation_index, GPPPolicies_Deforestation_index)
CPS_Deforestation_index <- CurrentPolicies_Deforestation_index
CPS_Deforestation_index$value <- CPS_Deforestation_index$value*100
GPP_Deforestation_index <- GPPPolicies_Deforestation_index
GPP_Deforestation_index$value <- GPP_Deforestation_index$value*100
Deforestation_index <- rbind(CPS_Deforestation_index, GPP_Deforestation_index)

axis_font <- element_text(face="bold", size=20)
axis_title <- element_text(face="bold", size=19)
legend_font <- element_text(size=30)

# a) renewable elctricity
rm(a)
a <- ggplot() + 
  geom_line(data=filter(RenElecShare, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(a) Renewable\n electricity share (%)") +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# b) oil and gas production
rm(b)
b <- ggplot() + 
  geom_line(data=filter(OilGas_Intensity, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(b) GHG intensity oil/gas \nproduction (ktCO2eq/Mtoe)") +
  #labs(x = "year", y = expression(paste('(b) GHG intensity oil/gas \nproduction (ktCO2eq/Mtoe)'))) +
  #labs(x = "year", y = expression('(b) GHG intensity oil/gas production (ktCO'[2]*'eq/Mtoe)')) +
  #labs(x = "year", y = expression(atop('(b) GHG intensity oil/gas 
  #                                     production (ktCO'[2]*'eq/Mtoe)'))) +
  #labs(x = "year", y = expression(atop(
  #  Difference~'in'~relative~oxygen~consumption,
  #  "("*V0[`2peak`]*")"))) +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# c) energy efficiency industry
rm(c)
c <- ggplot() + 
  geom_line(data=filter(IndustryEfficiency, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(c) Final energy use \nper US$ (GJ/US$2005)") +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# d) f-gases
rm(d)
d <- ggplot() +
  geom_line(data=filter(FGas_Reduction_index, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(d) F-gas emissions\n relative to 2010") +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# e) energy efficiency building envelope
rm(e)
e <- ggplot() + 
  geom_line(data=filter(Residential_FinalEnergy_m2, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(e) Residential energy use\n per m2 (GJ/m2)") +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# f) appliances
rm(f)
f <- ggplot() + 
  geom_line(data=filter(Appliances_FinalEnergy_capita, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(f) Energy use appliances\n per capita (GJ/cap)") +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# g) fuel efficiency cars
rm(g)
g <- ggplot() + 
  geom_line(data=filter(FuelUse_pkm_cars, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(f) Average\n fuel efficiency cars (km/l)") +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# h) electric cars
rm(h)
h <- ggplot() + 
  geom_line(data=filter(ElectricCars_share, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(h) Share of \n electric cars (%)") +
  theme(legend.text=element_text(size=30)) +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# i) deforestation
rm(i)
i <- ggplot() + 
  geom_line(data=filter(Deforestation_index, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=Scenario, linetype=Scenario), size=2) +
  labs(x = "year", y = "(i) Reduction in deforestation \nrate relative to 2010 (%)") +
  theme(axis.title = axis_title) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

source('grid_arrange_shared_legend.R')
#pdf(file=paste("graphs/Fig2_v2.pdf"), width=24,height=14)
#fig2_grid <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
#dev.off()
#png(file=paste("graphs/Fig2_v2.png"), units="in", width=24,height=14, res=400)
jpeg(file=paste("graphs/Figure2.jpg"), units="in", width=24,height=14, res=400)
fig2_grid <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
dev.off()

tmp<-ggplot_gtable(ggplot_build(a))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
a=a+theme(legend.position = "none")
b=b+theme(legend.position = "none")
c=c+theme(legend.position = "none")
d=d+theme(legend.position = "none")
e=e+theme(legend.position = "none")
f=f+theme(legend.position = "none")
g=g+theme(legend.position = "none")
h=h+theme(legend.position = "none")
i=i+theme(legend.position = "none")
#lay<-rbind(c(1,2,3),c(4,5,6), c(7,8,9), c(10))
lay<-rbind(c(1,2,3),c(1,2,3),c(4,5,6),c(4,5,6), c(7,8,9), c(7,8,9), c(10))
fig2_v1=grid.arrange(a,b,c,d,e,f,g,h,i,legend,layout_matrix=lay)
#ggsave(file=paste("graphs/Figure2_v2.png",sep=""),fig2_v1,width=24,height=14,dpi=400)
ggsave(file=paste("graphs/Figure2_v2.jpg",sep=""),fig2_v1,width=24,height=14,dpi=400)

#fig2_grob <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
#arrangeGrob(fig2_grob)
#ggsave(file=paste("graphs/Fig2_v2.pdf"),fig2_grob,width=24,height=14,dpi=200)
