#setwd("C:/Users/mrroelfs/Documents/R/TIMER_output")
library(data.table)
#library(plyr)
#library(dplyr)
#library(tidyr)
if("plyr" %in% (.packages())){
  detach(package:plyr) 
}
library(tidyverse)

# settings
#TIMERGeneration = 'TIMER_2015'
TIMER_folder = 'ClimatePolicies\\2_TIMER\\outputlib\\TIMER_2015\\ClimatePolicies'
IMAGE_folder = 'ClimatePolicies\\3_IMAGE\\Scenario_lib\\scen'
StartYear = 1990

NToN2O <- 44/28
CToCO2 <- 44/12
GWP_CH4 <- 25
GWP_N2O <- 298
GWP_HFC23 <- 14800
GWP_HFC32 <- 675
GWP_HFC4310 <- 1640
GWP_HFC125 <- 3500
GWP_HFC134a <- 1430
GWP_HFC143a <- 4470
GWP_HFC152 <- 124
GWP_HFC227 <- 3220
GWP_HFC236 <- 9810
GWP_HFC245 <- 693
GWP_CF4 <- 7390
GWP_C2F6 <- 12200
GWP_C6F14 <- 9300
GWP_SF6 <- 22800

MtoeToTJ=41868
GWhToTJ=3.6

MJ_l_gasoline = 34.8 #MJ/l
Load_car = 1.6 #persons

# labels for TIMER output tables
regions28 = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","World")
regions28_EU = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","EU","World")
regions27 = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","World")
sector = c('Industry', 'Transport', 'Residential', 'Service', 'Other', 'Total')
sector1 = c('Elec','Ind','Muni','Total non-agri')
sector2 = c('Industry','Transport','Residential','Services', 'Other1', 'Bunker oil', 'Non-energy', 'Hydrogen', 'Electricity', 'Other2')
sector3 = c('End-use industry', 'End-use transport', 'End-use residential', 'End-use services', 'End-use other', 'Energy transformation', 'Power generation', 'losses/leakages', 'Bunkers', 'Total')
main_sector = c('Energy supply', 'Transport', 'Industry', 'Buildings', 'AFOLU', 'Bunkers', 'Total')
energy_carrier_emis = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Total')
energy_carrier_energy = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Traditional biofuels', 'Nuclear', 'Solar/wind', 'Hydro-electricity')
energy_carrier_energy2 = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Traditional biofuels', 'Nuclear', 'Solar/wind', 'Hydro-electricity', 'Total')
energy_carrier_demand = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Traditional biofuels', 'Hydrogen', 'Secondary', 'Electricity', 'Total')
energy_carrier_ren = c('Modern biofuels', 'Solar/wind', 'Hydro-electricity')
energy_carrier_ren_excl_hydro = c('Modern biofuels', 'Solar/wind')
energy_carrier_nf = c('Modern biofuels', 'Traditional biofuels', 'Nuclear', 'Solar/wind', 'hydro-electricity')
industrial_process_CO2 = c('cement', 'feedstock', 'total')
industrial_process_CH4 = c('iron & steel', 'chemicals', 'bulk chemicals', 'total')
industrial_process_N2O = c('adipic acid', 'nitric acid', 'chemicals', 'total')
HFC = c('HFC23', 'HFC32',	'HFC4310', 'HFC125', 'HFC134a',	'HFC143a', 'HFC152', 'HFC227', 'HFC236', 'HFC245')
PFC = c('CF4','C2F6',	'SF6', 'C6F14')
land_use_source_CO2 = c('biomass burning', 'burning of traditional biomass', 'timber pool (short lifetime)', 'timber pool (long lifetime)', 'carbon release by regrowing vegetation', 'Total')
land_use_source_CH4 = c('biomass burning', 'fuelwood burning', 'agricultural waste burning', 'savanna burning', 'landfills', 'sewage', 'wetland rice', 'animals', 'animal waste', 'Total')
land_use_source_N2O = c('deforestation/biomass burning', 'traditional biomass/fuelwood burning', 'agricultural waste burning', 'savanna burning', 'land clearing', 'fertilizers', 'stables', 'grazing', 'manure application', 'fertilizers indirect', 'domestic sewage', 'from residues', 'biological N-fixation', 'Total')
population_groups = c('Total', 'Urban', 'Rural', 'U_1', 'U_2', 'U_3', 'U_4', 'U_5', 'R_1', 'R_2', 'R_3', 'R_4', 'R_5')
res_enduse_functions = c('HouseholdAppliances', 'Lighting', 'Cooking', 'WaterHeating', 'SpaceHeating', 'SpaceCooling')
travel_mode = c('Walking', 'Biking', 'Bus', 'Train', 'Car', 'High speed train', 'Air', 'Total')
car_type = c('ICE2000', 'x1', 'x2', 'ICE Adv H2', 'x3', 'ICE Diesel Oil', 'ICE Diesel Bio', 'ICE HEV Oil', 'ICE HEV Gas', 'x4', 'x5', 'ICE EFT50', 'x6', 'x7', 'FCV H2', 'Phev 10 oil', 'Phev 30 oil', 'x8', 'Phev 10 Bio', 'Phev 30 Bio', 'x9', 'BEV', 'x10', 'BEV 150', '-')