library(gridExtra)
library(grid)
library(rmarkdown)  # render pdf
library(stringr)
source('Settings.R')

setwd("~/R/TIMER_output")

source('Import_TIMER_output.R')
CurrentPolicies <- ImportTimerScenario('GPP_CurrentPolicies','SSP2')
GPPPolicies <- ImportTimerScenario('GPP_Total_fromCPS','SSP2')
GPPRENElectricity <- ImportTimerScenario('GPP_RENElectricity_fromCPS','SSP2')
GPPIndustry <- ImportTimerScenario('GPP_IndustryEfficiency_fromCPS','SSP2')
GPPBuildingCodes <- ImportTimerScenario('GPP_BuildingCodes_fromCPS','SSP2')
GPPAppliances <- ImportTimerScenario('GPP_AppliancesLight_fromCPS','SSP2')
GPPOilGas <- ImportTimerScenario('GPP_OilGas_fromCPS','SSP2')
GPPCAFEStandards <- ImportTimerScenario('GPP_CAFEStandards_fromCPS','SSP2')
GPPElectricCars_inclREN <- ImportTimerScenario('GPP_NewElectricCars_inclREN_fromCPS','SSP2')

source('Process_TIMER_output.R')
CurrentPolicies_indicators <- ProcessTimerScenario(CurrentPolicies)
GPPPolicies_indicators <- ProcessTimerScenario(GPPPolicies)
GPPRENElectricity_indicators <- ProcessTimerScenario(GPPRENElectricity)
GPPIndustry_indicators <- ProcessTimerScenario(GPPIndustry)
GPPBuildingCodes_indicators <- ProcessTimerScenario(GPPBuildingCodes)
GPPAppliances_indicators <- ProcessTimerScenario(GPPAppliances)
GPPOilGas_indicators <- ProcessTimerScenario(GPPOilGas)
GPPCAFEStandards_indicators <- ProcessTimerScenario(GPPCAFEStandards)
GPPElectricCars_inclREN_indicators <- ProcessTimerScenario(GPPElectricCars_inclREN)

# replace f-gas emissions for GPP policy scenario
GPPFGas_index <- read.table('data/FGases_index_GPP.csv', sep=";", header=TRUE, row.names=NULL)
tmp <- filter(CurrentPolicies_indicators$FGas_Reduction_index, region=="World", year>-1990, year<=2030)
tmp <- inner_join(tmp, GPPFGas_index, by=c('year', 'region'))
tmp <- mutate(tmp, value=pmin(value.x, value.y))
GPPFGas_index <- select(tmp, year, region, value)

# read in IIASA data for deforestation
GPP_Deforestation_data <- read.table('data/IIASA deforestation results.csv', sep=";", header=TRUE, row.names=NULL)
GPP_Deforestation_data$scenario <- str_trim(GPP_Deforestation_data$scenario)
GPP_Deforestation_data$indicator <- str_trim(GPP_Deforestation_data$indicator)
CurrentPolicies_Deforestation_index <- filter(GPP_Deforestation_data, scenario=="Current policies", indicator=="Reduction of deforestation rate relative to 2010")
CurrentPolicies_Deforestation_index <- select(CurrentPolicies_Deforestation_index, year, region, value)
GPPPolicies_Deforestation_index <- filter(GPP_Deforestation_data, scenario=="Good practice policies", indicator=="Reduction of deforestation rate relative to 2010")
GPPPolicies_Deforestation_index <- select(GPPPolicies_Deforestation_index, year, region, value)
#CurrentPolicies_Deforestation_index <- read.table('data/DeforestationReduction_index_CPS.csv', sep=";", header=TRUE, row.names=NULL)
#GPPPolicies_Deforestation_index <- read.table('data/DeforestationReduction_index_GPP.csv', sep=";", header=TRUE, row.names=NULL)

CPS_Total <-filter(CurrentPolicies_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_REN_Total <-filter(GPPRENElectricity_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_OilGas_Total <-filter(GPPOilGas_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_CAFEStandards_Total <-filter(GPPCAFEStandards_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_ElectricCars_inclREN_Total <-filter(GPPElectricCars_inclREN_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_BuildingCodes_Total <- filter(GPPBuildingCodes_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_Appliances_Total <- filter(GPPAppliances_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_Industry_Total <-filter(GPPIndustry_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_Total <-filter(GPPPolicies_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
CPS_Total <- mutate(CPS_Total, scenario="Current policies")
GPP_REN_Total <- mutate(GPP_REN_Total, scenario="Renewable electricity production")
GPP_OilGas_Total <- mutate(GPP_OilGas_Total, scenario="Oil and gas production")
GPP_CAFEStandards_Total <- mutate(GPP_CAFEStandards_Total, scenario="CAFE Standards")
GPP_ElectricCars_inclREN_Total <- mutate(GPP_ElectricCars_inclREN_Total, scenario="CAFE Standards")
GPP_BuildingCodes_Total <- mutate(GPP_BuildingCodes_Total, scenario="Building codes")
GPP_Appliances_Total <- mutate(GPP_Appliances_Total, scenario="Appliances and light")
GPP_Industry_Total <- mutate(GPP_Industry_Total, scenario="Good housekeeping in industry")
GPP_Total <- mutate(GPP_Total, scenario="Good practice policies")
GPP_Overview <- rbind(CPS_Total, GPP_REN_Total) %>% rbind(GPP_OilGas_Total) %>% rbind(GPP_CAFEStandards_Total) %>% rbind(GPP_ElectricCars_inclREN_Total) %>% rbind(GPP_BuildingCodes_Total) %>% rbind(GPP_Appliances_Total) %>% rbind(GPP_Industry_Total) %>% rbind(GPP_Total)
GPP_Overview <- select(GPP_Overview, year, value, scenario)

# FIGURE 1
# determine current and good practice policy emission levels
Scenarios <- c("Current policies", "Good practice policies")
#GPP <- filter(GPPPolicies_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
#GPP <- mutate(GPP, scenario="Good practice policies")
#GPP <- select(GPP, year, value, scenario)
#CPS <- filter(CurrentPolicies_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
#CPS <- mutate(CPS, scenario="Current policies")
#CPS <- select(CPS, year, value, scenario)
#data_fig1_1 <- rbind(CPS, GPP)
#data_fig1_1$scenario = factor(data_fig1_1$scenario, levels=Scenarios)

# determine reductions
CPS_Total <- select(CPS_Total, year, value)
GPP_Total <- select(GPP_Total, year, value)

#1. electricity reductions from demand sector
CPS_elec <- filter(CurrentPolicies$ENEMISCO2, year>=2010, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total")
Industry_elec <- filter(GPPIndustry$ENEMISCO2, year>=2010, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total")
BuildingCodes_elec <- filter(GPPBuildingCodes$ENEMISCO2, year>=2010, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total")
Appliances_elec <- filter(GPPAppliances$ENEMISCO2, year>=2010, year<=2030, region=="World", sector=="Power generation", energy_carrier=="Total")

red_Industry_elec <- inner_join(CPS_elec, Industry_elec, by=c('year', 'region', 'sector', 'energy_carrier'))
red_Industry_elec <- mutate(red_Industry_elec, value=value.x-value.y)
red_Industry_elec <- select(red_Industry_elec, year, value)
red_BuildingCodes_elec <- inner_join(CPS_elec, BuildingCodes_elec, by=c('year', 'region', 'sector', 'energy_carrier'))
red_BuildingCodes_elec <- mutate(red_BuildingCodes_elec, value=value.x-value.y)
red_BuildingCodes_elec <- select(red_BuildingCodes_elec, year, value)
red_Appliances_elec <- inner_join(CPS_elec, Appliances_elec, by=c('year', 'region', 'sector', 'energy_carrier'))
red_Appliances_elec <- mutate(red_Appliances_elec, value=value.x-value.y)
red_Appliances_elec <- select(red_Appliances_elec, year, value)

tmp1 <- inner_join(red_Industry_elec, red_BuildingCodes_elec, by=c('year'))
red_elec_demand <- inner_join(tmp1, red_Appliances_elec, by=c('year'))
red_elec_demand <- mutate(red_elec_demand, value=value.x+value.y+value)
red_elec_demand$value <- (44/12)*10^3*red_elec_demand$value
red_elec_demand <- select(red_elec_demand, year, value) %>% mutate(scenario="Reduction of electricity in industry and buildings sector")

# 2. reductions from oil, gas sector
GPP_OilGas_Total <- select(GPP_OilGas_Total, year, value)
red_OilGas_individual <- inner_join(CPS_Total, GPP_OilGas_Total, by=c("year"))
red_OilGas_individual <- mutate(red_OilGas_individual, value=value.x-value.y)
red_OilGas_individual <- select(red_OilGas_individual, year, value) %>% mutate(scenario="Reduction oil and gas production")
# ONLY DEMAND NON-CO2 reductions
#red_OilGas <-

# 3. reductions from energy efficiency in industry
GPP_Industry_Total <- select(GPP_Industry_Total, year, value)
red_Industry_individual <- inner_join(CPS_Total, GPP_Industry_Total, by=c("year"))
red_Industry_individual <- mutate(red_Industry_individual, value=value.x-value.y)
red_Industry_individual <- select(red_Industry_individual, year, value)
red_Industry_individual <- mutate(red_Industry_individual, scenario="Reduction industry efficiency")

#red_Industry <-

# 4. reduction of f-gases
#Good practice policies for f-gases were calculated outside the model and ar read in from csv file
CPS_FGases <- filter(CurrentPolicies_indicators$FGases, year>=2010, year <=2030, region=="World")
CPS_FGases <- mutate(CPS_FGases, scenario="Current policies")
GPP_FGases <- read.table('data/FGases_GPP.csv', sep=";", header=TRUE, row.names=NULL)
GPP_FGases <- filter(GPP_FGases, year>=2010, year <=2030)
GPP_FGases <- mutate(GPP_FGases, scenario="Good practice policies")
red_FGases_individual <- rbind(CPS_FGases_individual, GPP_FGases)
red_FGases_individual <- spread(red_FGases_individual, key=scenario, value=value)
red_FGases_individual <- mutate(red_FGases_individual, value=`Current policies`-`Good practice policies`)
red_FGases_individual <- select(red_FGases_individual, year, value) %>% mutate(scenario="Reduction f-gases")
red_FGases <- red_FGases_individual

# 5. reductions from building codes
GPP_BuildingCodes_Total <- select(GPP_BuildingCodes_Total, year, value)
red_BuildingCodes_individual <- inner_join(CPS_Total, GPP_BuildingCodes_Total, by=c("year"))
red_BuildingCodes_individual <- mutate(red_BuildingCodes_individual, value=value.x-value.y)
red_BuildingCodes_individual <- select(red_BuildingCodes_individual, year, value)
red_BuildingCodes_individual <- mutate(red_BuildingCodes_individual, scenario="Reduction building codes")
# ONLY DEMAND SECTOR CO2 emissions
#red_BuildingCodes <-

# 6. Appliances and light
GPP_Appliances_Total <- select(GPP_Appliances_Total, year, value)
red_Appliances_individual <- inner_join(CPS_Total, GPP_Appliances_Total, by=c("year"))
red_Appliances_individual <- mutate(red_Appliances_individual, value=value.x-value.y)
red_Appliances_individual <- select(red_Appliances_individual, year, value)
red_Appliances_individual <- mutate(red_Appliances_individual, scenario="Reduction building codes")
red_Appliances <- red_Appliances_individual
red_Appliances$value <- 0
  
# 7. reduction from CAFE standards
GPP_CAFEStandards_Total <- select(GPP_CAFEStandards_Total, year, value)
red_CAFEStandards_individual <- inner_join(CPS_Total, GPP_CAFEStandards_Total, by=c("year"))
red_CAFEStandards_individual <- mutate(red_CAFEStandards_individual, value=value.x-value.y)
red_CAFEStandards_individual <- select(red_CAFEStandards_individual, year, value)
red_CAFEStandards_individual <- mutate(red_CAFEStandards_individual, scenario="Reduction CAFE Standards")
red_CAFEStandards <- red_CAFEStandards_individual

# 8. reduction from electric cars (incl renewables)
# as we assume these cars are fueled with renewable electricity, we calculate the additional electricity relative to the REN elec scenario
RenElec_Total <-filter(GPPElectricCars_inclREN_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
RenElec_Total <- select(RenElec_Total, year, value)
GPP_ElectricCars_inclREN_Total <-filter(GPPElectricCars_inclREN_indicators$EMISCO2EQ, year>=2010, year<=2030, region=="World", main_sector=="Total", GHG_Category=="EMISCO2EQ")
GPP_ElectricCars_inclREN_Total <- select(GPP_ElectricCars_inclREN_Total, year, value)
GPP_REN_Total <- select(GPP_REN_Total, year, value)
red_ElectricCars_inclREN_individual <- inner_join(REN_Total, GPP_ElectricCars_inclREN_Total, by=c("year"))
red_ElectricCars_inclREN_individual <- mutate(red_ElectricCars_inclREN_individual, value=value.x-value.y)
red_ElectricCars_inclREN_individual <- select(red_ElectricCars_inclREN_individual, year, value)
red_ElectricCars_inclREN_individual <- mutate(red_ElectricCars_inclREN_individual, scenario="Reduction electric cars (incl REN)")
red_ElectricCars_inclREN <- red_ElectricCars_inclREN_individual

# 9. reductions from deforestation
red_Deforestation_individual <- filter(GPP_Deforestation_data, indicator=="CO2 emissions LULUCF")
red_Deforestation_individual <- select(red_Deforestation_individual, year, value, scenario)
red_Deforestation_individual <- spread(red_Deforestation_individual, scenario, value)
red_Deforestation_individual <- mutate(red_Deforestation_individual, value=`Current policies`-`Good practice policies`)
red_Deforestation_individual <- select(red_Deforestation_individual, year, value) %>% mutate(scenario="Reduction deforestation")
red_Deforestaion <- red_Deforestation_individual

# As Reductions for GPPREN_Electricity are total minus other reductions, we first need to determine CPS and GPP with adjustments for outside model calculations for f-gases and deforestation
CPS_LULUCF_CO2_PBL <- filter(CurrentPolicies$LUEMCO2, year>=2010, year <=2030, source=="Total", region=="World")
CPS_LULUCF_CO2_PBL <- select(CPS_LULUCF_CO2_PBL, year, value)
CPS_LULUCF_CO2_PBL <- mutate(CPS_LULUCF_CO2_PBL, GHG="LULUCF CO2 PBL")
CPS_LULUCF_CO2_PBL$value <- (44/12)*10^3*CPS_LULUCF_CO2_PBL$value
CPS_LULUCF_CO2_IIASA <- filter(GPP_Deforestation_data, scenario=="Current policies",  indicator=="CO2 emissions LULUCF")
CPS_LULUCF_CO2_IIASA <- select(CPS_LULUCF_CO2_IIASA, year, value)
CPS_LULUCF_CO2_IIASA <- mutate(CPS_LULUCF_CO2_IIASA, GHG="LULUCF CO2 IIASA")
CPS_Total_adj <- mutate(CPS_Total, GHG="Total")
CPS_Total_adj <- rbind(CPS_Total_adj, CPS_LULUCF_CO2_PBL)
CPS_Total_adj <- rbind(CPS_Total_adj, CPS_LULUCF_CO2_IIASA)
CPS_Total_adj <- spread(CPS_Total_adj, GHG, value)
CPS_Total_adj <- mutate(CPS_Total_adj, value=`Total`-`LULUCF CO2 PBL`+`LULUCF CO2 IIASA`)
CPS_Total_adj <- select(CPS_Total_adj, year, value)

GPP_LULUCF_CO2_PBL <- filter(GPPPolicies$LUEMCO2, year>=2010, year <=2030, source=="Total", region=="World")
GPP_LULUCF_CO2_PBL <- select(GPP_LULUCF_CO2_PBL, year, value)
GPP_LULUCF_CO2_PBL <- mutate(GPP_LULUCF_CO2_PBL, GHG="LULUCF CO2 PBL")
GPP_LULUCF_CO2_PBL$value <- (44/12)*10^3*GPP_LULUCF_CO2_PBL$value
GPP_LULUCF_CO2_IIASA <- filter(GPP_Deforestation_data, scenario=="Good practice policies",  indicator=="CO2 emissions LULUCF")
GPP_LULUCF_CO2_IIASA <- select(GPP_LULUCF_CO2_IIASA, year, value)
GPP_LULUCF_CO2_IIASA <- mutate(GPP_LULUCF_CO2_IIASA, GHG="LULUCF CO2 IIASA")
GPP_Total_adj <- mutate(GPP_Total, GHG="Total")
GPP_Total_adj <- rbind(GPP_Total_adj, GPP_LULUCF_CO2_PBL)
GPP_Total_adj <- rbind(GPP_Total_adj, GPP_LULUCF_CO2_IIASA)
red_FGases_adj <- select(red_FGases_adj, year, value) %>% mutate(GHG="Reductions f-gases")
GPP_Total_adj <- rbind(GPP_Total_adj, red_FGases_adj)
GPP_Total_adj <- spread(GPP_Total_adj, GHG, value)
GPP_Total_adj <- mutate(GPP_Total_adj, value=`Total`-`LULUCF CO2 PBL`+`LULUCF CO2 IIASA`-`Reductions f-gases`)
GPP_Total_adj <- select(GPP_Total_adj, year, value)

# 10. reductions from renewables
# red_RENElectricity = red_Total - red_elec_demand - red_OilGas - red_Industry - red_fgases - red_BuildingCodes -red_CAFEStandards
#                                - red_ElectricCars_inclREN - red_Deforestation
GPP_RENElectricity_Total <- select(GPP_RENElectricity_Total, year, value)
red_RENElectricity_individual <- inner_join(CPS_Total, GPP_RENElectricity_Total, by=c("year"))
red_RENElectricity_individual <- mutate(red_RENElectricity_individual, value=value.x-value.y)
red_RENElectricity_individual <- select(red_RENElectricity_individual, year, value)
red_RENElectricity_individual <- mutate(red_RENElectricity_individual, scenario="Reduction renewable electricity")


CPS_tmp <- mutate(CPS_Total_adj, scenario="Current policies")
GPP_tmp <- mutate(GPP_Total_adj, scenario="Good practice policies")
red_Total <- rbind(CPS_tmp, GPP_tmp)
red_Total <- spread(red_Total, scenario, value)
red_Total <- mutate(red_Total, value=`Current policies`-`Good practice policies`)
red_Total <- select(red_Total, year, value) %>% mutate(reductions="Total")

red_Other <- rbind(red_elec_demand, red_OilGas) %>% rbind(red_Industry) %>% rbind(red_FGases) %>% rbind(red_BuildingCodes) %>% 
             rbind(red_CAFEStandards) %>% rbind(red_ElectricCars_inclREN) %>% rbind(red_Deforestation) 
red_Other <- spread(red_Other, scenario, value)
red_Other <- mutate(red_Other, value=`Reduction oil and gas production`+`Reduction of electricity in industry and buildings sector` +
                                     `Reduction industry efficiency` + `Reduction electric cars (incl REN)` +
                                     `Reduction CAFE Standards` + `Reduction building codes` +
                                     `Reduction f-gases` + `Reduction deforestation`)
red_Other <- select(red_Other, year, value) %>% mutate(reductions="Other")

red_RENElectricity <- rbind(red_Total, red_Other)
red_RENElectricity <- spread(red_RENElectricity, reductions, value)
red_RENElectricity <- mutate(red_RENElectricity, value=`Total`-`Other`)
red_RENElectricity <- select(red_RENElectricity, year, value) %>% mutate(scenario="Reduction renewable electricity")
  
#data_fig1_2 <- rbind(CPS, GPP)
data_fig1_2 <- rbind(red_elec_demand, red_RENElectricity, red_OilGas, GPP)
#Reductions <- c("Total", " ")
Reductions <- c("Reduction of electricity in industry and buildings sector", "Renewable electricity", "Reduction oil and gas production",  " ")

data_fig1_2$scenario = factor(data_fig1_2$scenario, levels=Reductions)
fig1_left <- ggplot() +
        geom_area(data=data_fig1_2, aes(x=year, y=value, fill=scenario)) +
        geom_line(data=data_fig1_1, aes(x=year, y=value, colour=scenario, linetype=scenario), size=1.5) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_fill_manual("Reductions", values = c("orange","blue", "green", "white")) +
        scale_colour_manual("Scenario", values = c("black","black"))

# https://magesblog.com/post/2015-04-14-plotting-tables-alsongside-charts-in-r/
SummaryTable <- data.frame(
                   Policy_Action=c(1, 2, 3),
                   Reduction_1=c('1.5', '2.5', '4.0'),
                   Reduction_2=c('2.0', '3.0', '5.0')
                   )
names(SummaryTable) <- c("policy action", "Reduction1","Reduction2")
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
tbl <- tableGrob(SummaryTable, rows=NULL, theme=tt)

lay<-rbind(c(1,1,1,1,1,2), c(1,1,1,1,1,3))
fig1 <- grid.arrange(fig1_left, tbl, ncol=2, as.table=TRUE, layout_matrix=lay)


# FIGURE 2

# add scenario name as column
CurrentPolicies_indicators$RenElecShare <- mutate(CurrentPolicies_indicators$RenElecShare, scenario="Current policies")
CurrentPolicies_indicators$OilGas_Intensity <- mutate(CurrentPolicies_indicators$OilGas_Intensity, scenario="Current policies")
CurrentPolicies_indicators$IndustryEfficiency <- mutate(CurrentPolicies_indicators$IndustryEfficiency, scenario="Current policies")
CurrentPolicies_indicators$FGas_Reduction_index <- mutate(CurrentPolicies_indicators$FGas_Reduction_index, scenario="Current policies")
CurrentPolicies_indicators$Residential_FinalEnergy_m2 <- mutate(CurrentPolicies_indicators$Residential_FinalEnergy_m2, scenario="Current policies")
CurrentPolicies_indicators$Appliances_FinalEnergy_capita <- mutate(CurrentPolicies_indicators$Appliances_FinalEnergy_capita, scenario="Current policies")
CurrentPolicies_indicators$FuelUse_pkm_cars <- mutate(CurrentPolicies_indicators$FuelUse_pkm_cars, scenario="Current policies")
CurrentPolicies_indicators$ElectricCars_share <- mutate(CurrentPolicies_indicators$ElectricCars_share, scenario="Current policies")
CurrentPolicies_Deforestation_index <- mutate(CurrentPolicies_Deforestation_index, scenario="Current policies")

# add scenario name as column
GPPPolicies_indicators$RenElecShare <- mutate(GPPPolicies_indicators$RenElecShare, scenario="Good practice policies")
GPPPolicies_indicators$OilGas_Intensity <- mutate(GPPPolicies_indicators$OilGas_Intensity, scenario="Good practice policies")
GPPPolicies_indicators$IndustryEfficiency <- mutate(GPPPolicies_indicators$IndustryEfficiency, scenario="Good practice policies")
GPPFGas_index <- mutate(GPPFGas_index, scenario="Good practice policies")
GPPPolicies_indicators$Residential_FinalEnergy_m2 <- mutate(GPPPolicies_indicators$Residential_FinalEnergy_m2, scenario="Good practice policies")
GPPPolicies_indicators$Appliances_FinalEnergy_capita <- mutate(GPPPolicies_indicators$Appliances_FinalEnergy_capita, scenario="Good practice policies")
GPPPolicies_indicators$FuelUse_pkm_cars <- mutate(GPPPolicies_indicators$FuelUse_pkm_cars, scenario="Good practice policies")
GPPPolicies_indicators$ElectricCars_share <- mutate(GPPPolicies_indicators$ElectricCars_share, scenario="Good practice policies")
GPPPolicies_Deforestation_index <- mutate(GPPPolicies_Deforestation_index, scenario="Good practice policies")

# merge current and good practice policies
RenElecShare <- rbind(CurrentPolicies_indicators$RenElecShare, GPPPolicies_indicators$RenElecShare)
OilGas_Intensity <- rbind(CurrentPolicies_indicators$OilGas_Intensity, GPPPolicies_indicators$OilGas_Intensity)
IndustryEfficiency <- rbind(CurrentPolicies_indicators$IndustryEfficiency, GPPPolicies_indicators$IndustryEfficiency)
FGas_Reduction_index <- rbind(CurrentPolicies_indicators$FGas_Reduction_index, GPPFGas_index)
Residential_FinalEnergy_m2 <- rbind(CurrentPolicies_indicators$Residential_FinalEnergy_m2, GPPPolicies_indicators$Residential_FinalEnergy_m2)
Appliances_FinalEnergy_capita <- rbind(CurrentPolicies_indicators$Appliances_FinalEnergy_capita, GPPPolicies_indicators$Appliances_FinalEnergy_capita)
FuelUse_pkm_cars <- rbind(CurrentPolicies_indicators$FuelUse_pkm_cars, GPPPolicies_indicators$FuelUse_pkm_cars)
ElectricCars_share <- rbind(CurrentPolicies_indicators$ElectricCars_share, GPPPolicies_indicators$ElectricCars_share)
Deforestation_index <- rbind(CurrentPolicies_Deforestation_index, GPPPolicies_Deforestation_index)
                             
axis_font <- element_text(face="bold", size=22)
legend_font <- element_text(size=30)

# a) renewable elctricity
rm(a)
a <- ggplot() + 
  geom_line(data=filter(RenElecShare, year>=2015, year<=2030, region=="World"), 
                        aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(b) Renewable\n electricity share (%)") +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)
  
# b) oil and gas production
rm(b)
b <- ggplot() + 
  geom_line(data=filter(OilGas_Intensity, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = expression(paste('(b) GHG intensity oil/gas \nproduction (ktCO2eq/Mtoe)'))) +
  #labs(x = "year", y = expression('(b) GHG intensity oil/gas production (ktCO'[2]*'eq/Mtoe)')) +
  #labs(x = "year", y = expression(atop('(b) GHG intensity oil/gas 
  #                                     production (ktCO'[2]*'eq/Mtoe)'))) +
  #labs(x = "year", y = expression(atop(
  #  Difference~'in'~relative~oxygen~consumption,
  #  "("*V0[`2peak`]*")"))) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)
  
# c) energy efficiency industry
rm(c)
c <- ggplot() + 
  geom_line(data=filter(IndustryEfficiency, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(c) Final energy use per \nUS$(2005) (GJ/US$2005") +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# d) f-gases
rm(d)
d <- ggplot() +
  geom_line(data=filter(FGas_Reduction_index, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(d) F-gas emissions\n relative to 2010") +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# e) energy efficiency building envelope
rm(e)
e <- ggplot() + 
  geom_line(data=filter(Residential_FinalEnergy_m2, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(e) Residential energy use\n per m2 (GJ/m2)") +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# f) appliances
rm(f)
f <- ggplot() + 
  geom_line(data=filter(Appliances_FinalEnergy_capita, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(f) Energy use appliances\n per capita (GJ/cap)") +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# g) fuel efficiency cars
rm(g)
g <- ggplot() + 
  geom_line(data=filter(FuelUse_pkm_cars, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(f) Average\n fuel efficiency cars (km/l)") +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# h) electric cars
rm(h)
h <- ggplot() + 
  geom_line(data=filter(ElectricCars_share, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(h) Share of \n electric cars (%)") +
  theme(legend.text=element_text(size=30)) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

# i) deforestation
rm(i)
i <- ggplot() + 
  geom_line(data=filter(Deforestation_index, year>=2015, year<=2030, region=="World"), 
            aes(x = year, y = value, colour=scenario, linetype=scenario), size=2) +
  labs(x = "year", y = "(i) Reduction in deforestation \nrate relative to 2010 (%)") +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = axis_font) +
  theme(legend.text=legend_font) +
  theme(legend.title=legend_font) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  ylim(0, NA)

source('grid_arrange_shared_legend.R')
pdf(file=paste("graphs/Fig2_v2.pdf"), width=24,height=14)
fig2_grid <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
dev.off()
png(file=paste("graphs/Fig2_v2.png"), units="in", width=24,height=14, res=200)
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
lay<-rbind(c(1,2,3),c(4,5,6), c(7,8,9), c(10))
fig2_v1=grid.arrange(a,b,c,d,e,f,g,h,i,legend,layout_matrix=lay)
ggsave(file=paste("graphs/Fig2_v1.png",sep=""),fig2_v1,width=24,height=14,dpi=200)

#fig2_grob <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
#arrangeGrob(fig2_grob)
#ggsave(file=paste("graphs/Fig2_v2.pdf"),fig2_grob,width=24,height=14,dpi=200)
