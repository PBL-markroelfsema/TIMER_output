#library(data.table)
#if("plyr" %in% (.packages())){
#  detach(package:plyr) 
#}
#library(tidyverse)

# settings
StartYear = 1990

# PBL colors
#source('../TIMER_output/functions/pbl_colors.R')
#rhg_cols <- c(hemelblauw3010, mosgroen3020, violet3030, donkergeel3040, paars3050, lichtblauw3060, roze3070, groen3080, rood3090, 
#              donkergroen3100, oranje3110, donkerbruin3120, robijnrood3130, bruin3140, mintgroen3150, geel3160)

Kilo = 10^3
Mega = 10^6
Giga = 10^9
Tera = 10^12
Peta = 10^15
Exa = 10^18

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

MJ_l_gasoline = 34.8           # MJ/l
Load_car = 1.6                 # persons
energy_intensity_fuel =	34.841 # MJ/l
co2_intensity_gasoline =	2.4  # gCO2/l
co2_intensity_diesel =	2.7    # gCO2/l
weight_gasoline	= 0.43
co2_intensity_fuel = 	co2_intensity_gasoline*weight_gasoline + co2_intensity_diesel*(1-weight_gasoline) # gCO2/l

# EU28
EU28 = c('AUT', 'BEL', 'DNK', 'FIN', 'FRA', 'DEU', 'GRC', 'IRL', 'ITA', 'LUX', 'MLT', 'NLD', 'PRT', 'ESP', 'SWE', 'GBR', 'BGR', 'HRV', 'CYP', 'CZE', 'EST', 'HUN', 'LVA', 'LTU', 'POL', 'ROU', 'SVK', 'SVN')
Europe = c(EU28, 'AND', 'FRO', 'GIB', 'VAT', 'ISL', 'LIE', 'MCO', 'NOR', 'SMR', 'CHE', 'ALB', 'BIH', 'MKD', 'SRB', 'MNE')

# labels for TIMER output tables
regions28 = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","World")
regions28_EU = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","EU","World")
regions27 = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","World")
regions26 = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF")
regions28_IMAGE = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","World")
sector = c('Industry', 'Transport', 'Residential', 'Service', 'Other', 'Total')
sector1 = c('Elec','Ind','Muni','Total non-agri')
sector2 = c('Industry','Transport','Residential','Services', 'Other1', 'Bunker oil', 'Non-energy', 'Hydrogen', 'Electricity', 'Other2')
sector3 = c('End-use industry', 'End-use transport', 'End-use residential', 'End-use services', 'End-use other', 'Energy transformation', 'Power generation', 'losses/leakages', 'Bunkers', 'Total')
sector_capture_2015 = c('electricity', 'other industry', 'cement', 'hydrogen', 'transformation', 'heat')
sector_capture_3_2 = c('electricity', 'other industry', 'cement', 'hydrogen', 'BLF-BECCS', 'transformation', 'heat', 'DAC')
sector_hydrogen = c('Industry', 'Transport', 'Residential', 'Service', 'Other', 'H2 in gas', 'Total')
sector_energy_supply = c('Electricity', 'Heat', 'Total')
main_sector = c('Energy supply', 'Transport', 'Industry', 'Buildings', 'LULUCF', 'Agriculture', 'Waste', 'Bunkers', 'Total')
energy_carrier_emis = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Total')
energy_carrier_energy = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Traditional biofuels', 'Nuclear', 'Solar/wind', 'Hydro-electricity')
energy_carrier_energy2 = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Traditional biofuels', 'Nuclear', 'Solar/wind', 'Hydro-electricity', 'Total')
energy_carrier_demand = c('Coal', 'Heavy oil', 'Light oil', 'Natural gas', 'Modern biofuels', 'Traditional biofuels', 'Hydrogen', 'Secondary', 'Electricity', 'Total')
energy_carrier_demand_ren = c('Modern biofuels','Hydrogen')
energy_carrier_residential = c('Coal', 'Oil', 'Natural gas', 'Modern biofuels', 'Traditional biofuels', 'Hydrogen', 'Secondary heat', 'Electricity')
energy_carrier_sec_fuel = c('Solid fuel', 'Liquid fuel', 'Gaseous fuel', 'Hydrogen', 'Modern biofuel', 'Secondary Heat', 'Tradtional biofuel', 'Electricity')
energy_carrier_sec_fuel2 = c('Solid fuel', 'Liquid fuel', 'Gaseous fuel', 'Hydrogen', 'Modern biofuel', 'Secondary Heat', 'Tradtional biofuel', 'Electricity', 'Total')
energy_carrier_trade = c('Coal', 'Oil', 'Natural gas', 'Modern biofuels', '-') # last unknown?
energy_carrier_heat = c('Coal', 'Oil', 'Natural gas', 'Modern biofuels')
energy_carrier_capture= c('Coal', 'Oil', 'Natural gas', 'Biomass', 'Other', 'Total') 
energy_carrier_ren = c('Modern biofuels', 'Solar/wind', 'Hydro-electricity')
energy_carrier_ren_excl_hydro = c('Modern biofuels', 'Solar/wind')
energy_carrier_nf = c('Modern biofuels', 'Nuclear', 'Solar/wind', 'Hydro-electricity')
energy_carrier_ren_28_2015        = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable',          '8','Waste','Biomass CC',                                          'Biomass + CS','CHP biomass'                   )
energy_carrier_ren_30_3_2        = c('PV large-scale','PV residential', 'CSP', 'Wind Onshore','Wind Offshore','Wave', 'Hydro','Other Renewable',          '8','Waste','Biomass CC',                                          'Biomass + CS','CHP biomass'                   )
energy_carrier_nf_28_2015         = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable','Nuclear','8','Waste','Biomass CC',                                          'Biomass + CS','CHP biomass'                   )
energy_carrier_nf_30_3_2         = c('PV large-scale','PV residential', 'CSP', 'Wind Onshore','Wind Offshore','Wave', 'Hydro','Other Renewable','Nuclear','8','Waste','Biomass CC',                                          'Biomass + CS','CHP biomass'                   )
energy_carrier_innovative_28_2015 = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable','Nuclear','8','Waste','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP biomass','CHP biomass + CS')
energy_carrier_innovative_30_3_2 = c('PV large-scale','PV residential', 'CSP', 'Wind Onshore','Wind Offshore','Wave', 'Hydro','Other Renewable','Nuclear','8','Waste','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP biomass','CHP biomass + CS')
energy_carrier_hydrogen = c('Coal', 'Oil', 'Natural Gas', 'Modern Biofuel', 'Electricity', 'Solar Thermal')
energy_carrier_hydrogen_ren = c('Solar Thermal', 'Modern Biofuel')
energy_carrier_hydrogen2 = c('Coal', 'Coal + CCS', 'Oil', 'Oil + CCS', 'Natural Gas', 'Natural Gas + CCS', 'Modern Biofuel', 'Modern Biofuel + CCS', 'Electrolysis', 'Solar Thermal', 'Small SMR', 'Imports')
energy_carrier_hydrogen2_ren = c('Modern Biofuel', 'Modern Biofuel + CCS','Solar Thermal')
energy_carrier_hydrogen2_innovative = c('Coal + CCS', 'Oil + CCS', 'Natural Gas + CCS', 'Modern Biofuel + CCS', 'Solar Thermal')
#energy_technology    = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable','Nuclear','8','Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP coal','CHP oil','CHP natural gas','CHP biomass','CHP coal + CS','CHP oil + CS','CHP natural gas + CS','CHP biomass + CS','Total')
energy_carrier_nf = c('Modern biofuels', 'Traditional biofuels', 'Nuclear', 'Solar/wind', 'hydro-electricity')
energy_technology_energy_supply = c('Coal w/o CCS','LLF w/o CCS', 'HLF w/o CCS', 'NG w/o CCS', 'Biomass w/o CCS', 'Coal w/ CCS','LLF w/ CCS', 'HLF w/ CCS', 'NG w/ CCS', 'Biomass w/ CCS', 'Total')
#energy_technology = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable','Nuclear','8','Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP coal','CHP oil','CHP natural gas','CHP biomass','CHP coal + CS','CHP oil + CS','CHP natural gas + CS','CHP biomass + CS','Total')
energy_technology_2015 = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable','Nuclear','8','Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP coal','CHP oil','CHP natural gas','CHP biomass','CHP coal + CS','CHP oil + CS','CHP natural gas + CS','CHP biomass + CS','Total')
energy_technology_3_2 = c('PV large-scale', 'PV residential', 'CSP', 'Wind Onshore','Wind Offshore', 'Wave', 'Hydro', 'Other Renewable', 'Nuclear', 'free', 'Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP coal','CHP oil','CHP natural gas','CHP biomass','CHP coal + CS','CHP oil + CS','CHP natural gas + CS','CHP biomass + CS', 'Total')
energy_technology_28_2015 = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable','Nuclear','8','Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP coal','CHP oil','CHP natural gas','CHP biomass','CHP coal + CS','CHP oil + CS','CHP natural gas + CS','CHP biomass + CS')
energy_technology_30_3_2 = c('PV large-scale', 'PV residential', 'CSP', 'Wind Onshore','Wind Offshore', 'Wave', 'Hydro', 'Other Renewable', 'Nuclear', 'free', 'Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS','CHP coal','CHP oil','CHP natural gas','CHP biomass','CHP coal + CS','CHP oil + CS','CHP natural gas + CS','CHP biomass + CS')
energy_technology_coal = c('Conv. Coal', 'IGCC', 'Coal + CS')
energy_technology_plant = c('Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS')
energy_technology_ren_2015 = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable', '8','Waste','Biomass CC','Biomass + CS','CHP biomass','CHP biomass + CS')
energy_technology_ren_3_2 = c('PV large-scale','PV residential', 'CSP', 'Wind Onshore','Wind Offshore','Wave', 'Hydro','Other Renewable', 'free','Waste','Biomass CC','Biomass + CS','CHP biomass','CHP biomass + CS')
energy_technology_wind = c('Wind Onshore','Wind Offshore')
energy_technology_solar_2015 = c('PV','CSP')
energy_technology_solar_3_2 = c('PV','PV residential', 'CSP')
energy_technology_20_2015 = c('PV','CSP', 'Wind Onshore','Wind Offshore','Hydro','Other Renewable','Nuclear','8','Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS')
energy_technology_22_3_2 = c('PV', 'PV residential', 'CSP', 'Wind Onshore','Wind Offshore', 'Wave', 'Hydro', 'Other Renewable', 'Nuclear', 'free', 'Conv. Coal','Conv. Oil','Conv. Natural gas','Waste','IGCC','OGCC','NG CC','Biomass CC','Coal + CS','Oil + CS','Natural gas + CS','Biomass + CS')
energy_technology_14 = c('Solar','Wind', 'Hydro','Other Renewable','Nuclear','Coals','Coal CCS','Oil','Oil CCS','NG','NG CCS','Biomass','Biomass CCS', 'Total')
heat_calibration = c('Model HP', 'IEA HP', 'Model CHP', 'IEA CHP')
industrial_process_CO2 = c('cement', 'feedstock', 'total')
industrial_process_CH4 = c('iron & steel', 'chemicals', 'bulk chemicals', 'total')
industrial_process_N2O = c('adipic acid', 'nitric acid', 'chemicals', 'total')
HFC = c('HFC23', 'HFC32',	'HFC4310', 'HFC125', 'HFC134a',	'HFC143a', 'HFC152', 'HFC227', 'HFC236', 'HFC245')
PFC = c('CF4','C2F6',	'SF6', 'C6F14')
land_use_source_CO2 = c('biomass burning', 'burning of traditional biomass', 'timber pool (short lifetime)', 'timber pool (long lifetime)', 'carbon release by regrowing vegetation', 'Total')
land_use_source_CO2_3_2 = c('biomass burning', 'burning of traditional biomass', 'crops  on peatland', 'timber pool (short lifetime)', 'timber pool (long lifetime)', 'carbon release by regrowing vegetation', 'Total')
land_use_source_CH4 = c('biomass burning', 'fuelwood burning', 'agricultural waste burning', 'savanna burning', 'landfills', 'sewage', 'wetland rice', 'animals', 'animal waste', 'Total')
land_use_source_CH4_3_2 = c('biomass burning', 'fuelwood burning', 'agricultural waste burning', 'savanna burning', 'landfills', 'sewage', 'wetland rice', 'animals', 'animal waste', 'degraded peatlands', 'Total')
land_use_source_N2O = c('deforestation/biomass burning', 'traditional biomass/fuelwood burning', 'agricultural waste burning', 'savanna burning', 'land clearing', 'fertilizers', 'stables', 'grazing', 'manure application', 'fertilizers indirect', 'domestic sewage', 'crop residues', 'biological N-fixation', 'Total')
land_use_source_N2O_3_2 = c('deforestation/biomass burning', 'traditional biomass/fuelwood burning', 'agricultural waste burning', 'savanna burning', 'land clearing', 'fertilizers', 'stables', 'grazing', 'manure application', 'fertilizers indirect', 'domestic sewage', 'crop residues', 'biological N-fixation', 'degraded peatlands', 'Total')
population_groups = c('Total', 'Urban', 'Rural', 'U_1', 'U_2', 'U_3', 'U_4', 'U_5', 'R_1', 'R_2', 'R_3', 'R_4', 'R_5')
population_groups3 = c('Total', 'Urban', 'Rural')
res_enduse_functions = c('HouseholdAppliances', 'Lighting', 'Cooking', 'WaterHeating', 'SpaceHeating', 'SpaceCooling')
res_appliances = c('Fan', 'Air cooler', 'Air conditioner', 'Refrigerator', 'Microwave', 'Washing machine', 'Clothes dryer', 'Dish washer', 'TV', 'VCR/DVD', 'PC/other', 'Other')
transport_modus = c('Passenger travel' , ' Freight' , 'Total')
travel_mode_travel = c('Walking', 'Biking', 'Bus', 'Train', 'Car', 'High speed train', 'Air', 'Total')
travel_mode_travel_excl_total = c('Walking', 'Biking', 'Bus', 'Train', 'Car', 'High speed train', 'Air')
travel_mode_freight = c('Inland shipping', 'Freigth train', 'Medium truck', 'Heavy truck', 'Air cargo', 'International shipping', '-', 'Total')
travel_mode_freight_excl_total = c('Inland shipping', 'Freigth train', 'Medium truck', 'Heavy truck', 'Air cargo', 'International shipping', '-')
#travel_mode = c('Walking', 'Biking', 'Bus', 'Train', 'Car', 'High speed train', 'Air', 'Inland shipping', 'Freigth train', 'Medium truck', 'Heavy truck', 'Air cargo', 'International shipping', '-', 'Total')
travel_mode = c(travel_mode_travel_excl_total, travel_mode_freight)
car_type = c('ICE2000', 'ICE2010', 'ICEAdvOil', 'ICE Adv H2', 'ICE Turbo Diesel', 'ICE Diesel Oil', 'ICE Diesel Bio', 'ICE HEV Oil', 'ICE HEV Gas', 'ICE HEV H2', 'ICE HEV Oil2', 'ICE EFT50', 'FCV Oil', 'FCV Bio', 'FCV H2', 'Phev 10 oil', 'Phev 30 oil', 'Phev 60 oil', 'Phev 10 Bio', 'Phev 30 Bio', 'Phev 60 Bio', 'BEV range extended', 'BEV', 'BEV 100km', '-')
car_type_included = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
#car_type = c('ICE2000', 'x1', 'x2', 'ICE Adv H2', 'x3', 'ICE Diesel Oil', 'ICE Diesel Bio', 'ICE HEV Oil', 'ICE HEV Gas', 'x4', 'x5', 'ICE EFT50', 'x6', 'x7', 'FCV H2', 'Phev 10 oil', 'Phev 30 oil', 'x8', 'Phev 10 Bio', 'Phev 30 Bio', 'x9', 'BEV', 'x10', 'BEV 150', '-')
bus_type =c('Oil', 'Bio', 'Gas', 'ElectricTrolley', 'HybridOil', 'HybridBio', 'ElectricBattery', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13', 'x14', 'x15', 'x16', 'x17', 'x18')
train_type =c('Oil', 'Bio', 'Electric','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25')
aircraft_type=c('Air conv', 'Air conv 1980', 'Air conv 2000', 'conv, bio', 'eff.oil', 'eff.bio', 'BWB oil', 'BWB bio', 'BWB eff. oil', 'BWB eff bio', 'Cryoplane','12','13','14','15','16','17','18','19','20','21','22','23','24','25')
forest_type=c('Mature forest', 'Regrowth (Abandond land)', 'Regrowth (Timber)', 'Carbon plantations', 'Total')
