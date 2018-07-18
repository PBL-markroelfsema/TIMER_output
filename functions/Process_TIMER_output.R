#pre: Make sure you are in the 1-click environment R-scripts directory
#     e.g. Y:\ontwapps\Timer\Users\Mark\CD_LINKSupdate\R-scripts/TIMER_output
#     Scenario is created with ImportTimerScenario and assigned to 'Scenario'
#post: this script processes TIMER output resulting in new output variables
# 3. Aggregates GHG emissions files to total emissions in CO2eq (unit)
#    All variables use 27 regions 
# 4. Creates ENEMISCO2EQ with totals per individual GHG and all GHG emisisons in CO2eq
# 5. Creates Renewable shares based on ElecProd

# These output variables are created
# EMISCO2EQ contains totals per individual GHG and total aggregated GHG (per region and year)
# ElecRenShare gives for each region and year the renewable share in electricity production

#TODO: 
# make EMISCO2EQ excl LULUCF
# make variable with individual and total GHG emissions per sector (energy supply, transport, industry, buildings, AFOLU, bunkers)
# CO2_intensity_GDP
# Energy_intensity_TPES_GDP

ProcessTimerScenario <- function(Scenario)
{ s <- deparse(substitute(Scenario)) # get object name as string
  if(!exists(s))
  { print(paste("Scenario ", s, " is not imported yet. First execute ImportTimerFile", sep=""))
    stop()
  }

source(paste('functions', 'Settings.R', sep='/'))
source(paste('functions', 'General Functions.R', sep='/'))

#3. 
# Aggregate emissions to CO2eq per TIMER file
ENEMISCO2_TOT = subset(Scenario$ENEMISCO2, energy_carrier == "Total" & year >= StartYear)
#ENEMISCO2_TOT$region = factor(ENEMISCO2_TOT$region,labels=regions28_EU)
ENEMISCO2_TOT$value = ENEMISCO2_TOT$value*10^3*CToCO2
ENEMISCO2_TOT = mutate(ENEMISCO2_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISCO2_TOT$main_sector = factor(ENEMISCO2_TOT$main_sector,levels=main_sector)
ENEMISCO2_TOT <- mutate(ENEMISCO2_TOT, GHG_Category="ENEMISCO2")
ENEMISCO2_TOT = select(ENEMISCO2_TOT, year, region, main_sector, GHG_Category, value)

ENEMISCH4_TOT = data.table(Scenario$ENEMISCH4)[energy_carrier == "Total" & year >= StartYear]
#ENEMISCH4_TOT$region = factor(ENEMISCH4_TOT$region,labels=regions28_EU)
ENEMISCH4_TOT$value = ENEMISCH4_TOT$value*GWP_CH4
ENEMISCH4_TOT = mutate(ENEMISCH4_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISCH4_TOT$main_sector = factor(ENEMISCH4_TOT$main_sector,levels=main_sector)
ENEMISCH4_TOT <- mutate(ENEMISCH4_TOT, GHG_Category="ENEMISCH4")
ENEMISCH4_TOT = select(ENEMISCH4_TOT, year, region, main_sector, GHG_Category, value)

ENEMISN2O_TOT = data.table(Scenario$ENEMISN2O)[energy_carrier == "Total" & year >= StartYear]
#ENEMISN2O_TOT$region = factor(ENEMISN2O_TOT$region,labels=regions28_EU)
ENEMISN2O_TOT$value = ENEMISN2O_TOT$value*NToN2O*GWP_N2O
ENEMISN2O_TOT = mutate(ENEMISN2O_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISN2O_TOT$main_sector = factor(ENEMISN2O_TOT$main_sector,levels=main_sector)
ENEMISN2O_TOT <- mutate(ENEMISN2O_TOT, GHG_Category="ENEMISN2O")
ENEMISN2O_TOT = select(ENEMISN2O_TOT, year, region, main_sector, GHG_Category, value)

INDEMISCO2_TOT = data.table(Scenario$INDEMISCO2)[industrial_process == "total" & year >= StartYear]
#INDEMISCO2_TOT$region = factor(INDEMISCO2_TOT$region,labels=regions28_EU)
INDEMISCO2_TOT$value = INDEMISCO2_TOT$value*10^3*CToCO2
#INDEMISCO2_TOT <- mutate(INDEMISCO2_TOT, main_sector="Industry")
INDEMISCO2_TOT = mutate(INDEMISCO2_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISCO2_TOT$main_sector = factor(INDEMISCO2_TOT$main_sector,levels=main_sector)
INDEMISCO2_TOT <- mutate(INDEMISCO2_TOT, GHG_Category="INDEMISCO2")
INDEMISCO2_TOT = select(INDEMISCO2_TOT, year, region, main_sector, GHG_Category, value)
tmp <- INDEMISCO2_TOT %>% mutate(main_sector="Industry")
INDEMISCO2_TOT <- rbind(INDEMISCO2_TOT, tmp)

INDEMISCH4_TOT = data.table(Scenario$INDEMISCH4)[industrial_process == "total" & year >= StartYear]
#INDEMISCH4_TOT$region = factor(INDEMISCH4_TOT$region,labels=regions28_EU)
INDEMISCH4_TOT$value = INDEMISCH4_TOT$value*GWP_CH4
#INDEMISCH4_TOT <- mutate(INDEMISCH4_TOT, main_sector="Industry")
INDEMISCH4_TOT = mutate(INDEMISCH4_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISCH4_TOT$main_sector = factor(INDEMISCH4_TOT$main_sector,levels=main_sector)
INDEMISCH4_TOT <- mutate(INDEMISCH4_TOT, GHG_Category="INDEMISCH4")
INDEMISCH4_TOT = select(INDEMISCH4_TOT, year, region, main_sector, GHG_Category, value)
tmp <- INDEMISCH4_TOT %>% mutate(main_sector="Industry")
INDEMISCH4_TOT <- rbind(INDEMISCH4_TOT, tmp)

INDEMISN2O_TOT = data.table(Scenario$INDEMISN2O)[industrial_process == "total" & year >= StartYear]
#INDEMISN2O_TOT$region = factor(INDEMISN2O_TOT$region,labels=regions28_EU)
INDEMISN2O_TOT$value = INDEMISN2O_TOT$value*NToN2O*GWP_N2O
#INDEMISN2O_TOT <- mutate(INDEMISN2O_TOT, main_sector="Industry")
INDEMISN2O_TOT = mutate(INDEMISN2O_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISN2O_TOT$main_sector = factor(INDEMISN2O_TOT$main_sector,levels=main_sector)
INDEMISN2O_TOT <- mutate(INDEMISN2O_TOT, GHG_Category="INDEMISN2O")
INDEMISN2O_TOT = select(INDEMISN2O_TOT, year, region, main_sector, GHG_Category, value)
tmp <- INDEMISN2O_TOT %>% mutate(main_sector="Industry")
INDEMISN2O_TOT <- rbind(INDEMISN2O_TOT, tmp)

HFC_TOT = data.table(Scenario$HFC_reg)[year >= StartYear]
HFC_TOT[HFC_gas=="HFC23"]$value =   10^-3*HFC_TOT[HFC_gas=="HFC23"]$value*GWP_HFC23 
HFC_TOT[HFC_gas=="HFC32"]$value =   10^-3*HFC_TOT[HFC_gas=="HFC32"]$value*GWP_HFC32 
HFC_TOT[HFC_gas=="HFC4310"]$value = 10^-3*HFC_TOT[HFC_gas=="HFC4310"]$value*GWP_HFC4310
HFC_TOT[HFC_gas=="HFC125"]$value =  10^-3*HFC_TOT[HFC_gas=="HFC125"]$value*GWP_HFC125
HFC_TOT[HFC_gas=="HFC134a"]$value = 10^-3*HFC_TOT[HFC_gas=="HFC134a"]$value*GWP_HFC134a
HFC_TOT[HFC_gas=="HFC143a"]$value = 10^-3*HFC_TOT[HFC_gas=="HFC143a"]$value*GWP_HFC143a
HFC_TOT[HFC_gas=="HFC152"]$value =  10^-3*HFC_TOT[HFC_gas=="HFC152"]$value*GWP_HFC152
HFC_TOT[HFC_gas=="HFC227"]$value =  10^-3*HFC_TOT[HFC_gas=="HFC227"]$value*GWP_HFC227
HFC_TOT[HFC_gas=="HFC236"]$value =  10^-3*HFC_TOT[HFC_gas=="HFC236"]$value*GWP_HFC236
HFC_TOT[HFC_gas=="HFC245"]$value =  10^-3*HFC_TOT[HFC_gas=="HFC245"]$value*GWP_HFC245
HFC_TOT <- HFC_TOT %>% group_by(year, region) %>% summarise(value=sum(value))
HFC_TOT <- ungroup(HFC_TOT)
HFC_TOT <- mutate(HFC_TOT, HFC_gas="Total")
#HFC_TOT <- bind_rows(HFC_TOT, HFC_tmp)
HFC_TOT = mutate(HFC_TOT, main_sector=mapply(function(x) MainSector(x), HFC_gas))
HFC_TOT$main_sector = factor(HFC_TOT$main_sector,levels=main_sector)
HFC_TOT <- mutate(HFC_TOT, GHG_Category="HFC")
HFC_TOT = select(HFC_TOT, year, region, main_sector, GHG_Category, value)

PFC_TOT = data.table(Scenario$PFC_reg)[year >= StartYear]
PFC_TOT[PFC_gas=="CF4"]$value =   10^-3*PFC_TOT[PFC_gas=="CF4"]$value*GWP_CF4
PFC_TOT[PFC_gas=="C2F6"]$value =  10^-3*PFC_TOT[PFC_gas=="C2F6"]$value*GWP_C2F6
PFC_TOT[PFC_gas=="C6F14"]$value = 10^-3*PFC_TOT[PFC_gas=="C6F14"]$value*GWP_C6F14 
PFC_TOT[PFC_gas=="SF6"]$value =   10^-3*PFC_TOT[PFC_gas=="SF6"]$value*GWP_SF6 
PFC_TOT <- PFC_TOT %>% group_by(year, region) %>% summarise(value=sum(value))
PFC_TOT <- ungroup(PFC_TOT)
PFC_TOT <- mutate(PFC_TOT, PFC_gas="Total")
#PFC_TOT <- bind_rows(PFC_TOT, PFC_tmp)
PFC_TOT = mutate(PFC_TOT, main_sector=mapply(function(x) MainSector(x), PFC_gas))
PFC_TOT$main_sector = factor(PFC_TOT$main_sector,levels=main_sector)
PFC_TOT <- mutate(PFC_TOT, GHG_Category="PFC")
PFC_TOT = select(PFC_TOT, year, region, main_sector, GHG_Category, value)

LUEMCO2_TOT = data.table(Scenario$LUEMCO2)[source == "Total" & year >= StartYear]
LUEMCO2_TOT$value = LUEMCO2_TOT$value*10^3*CToCO2
#LUEMCO2_TOT <- mutate(LUEMCO2_TOT, main_sector="AFOLU")
LUEMCO2_TOT = mutate(LUEMCO2_TOT, main_sector=mapply(function(x) MainSector(x), source))
LUEMCO2_TOT$main_sector = factor(LUEMCO2_TOT$main_sector,levels=main_sector)
LUEMCO2_TOT <- mutate(LUEMCO2_TOT, GHG_Category="LUEMCO2")
LUEMCO2_TOT = select(LUEMCO2_TOT, year, region, main_sector, GHG_Category, value)

LUEMCH4_TOT = data.table(Scenario$LUEMCH4)[source == "Total" & year >= StartYear]
LUEMCH4_TOT$value = LUEMCH4_TOT$value*GWP_CH4
#LUEMCH4_TOT <- mutate(LUEMCH4_TOT, main_sector="AFOLU")
LUEMCH4_TOT = mutate(LUEMCH4_TOT, main_sector=mapply(function(x) MainSector(x), source))
LUEMCH4_TOT$main_sector = factor(LUEMCH4_TOT$main_sector,levels=main_sector)
LUEMCH4_TOT <- mutate(LUEMCH4_TOT, GHG_Category="LUEMCH4")
LUEMCH4_TOT = select(LUEMCH4_TOT, year, region, main_sector, GHG_Category, value)

LUEMN2O_TOT = data.table(Scenario$LUEMN2O)[source == "Total" & year >= StartYear]
LUEMN2O_TOT$value = LUEMN2O_TOT$value*NToN2O*GWP_N2O
#LUEMN2O_TOT <- mutate(LUEMN2O_TOT, main_sector="AFOLU")
LUEMN2O_TOT = mutate(LUEMN2O_TOT, main_sector=mapply(function(x) MainSector(x), source))
LUEMN2O_TOT$main_sector = factor(LUEMN2O_TOT$main_sector,levels=main_sector)
LUEMN2O_TOT <- mutate(LUEMN2O_TOT, GHG_Category="LUEMN2O")
LUEMN2O_TOT = select(LUEMN2O_TOT, year, region, main_sector, GHG_Category, value)

# 4.
# Add all total aggregated emissions to on table EMISCO2EQ, and calculate total GHG emissions incl/excl LULUCF CO2
EMISCO2EQ <- ENEMISCO2_TOT
EMISCO2EQ <- bind_rows(EMISCO2EQ, ENEMISCH4_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, ENEMISN2O_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, INDEMISCO2_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, INDEMISCH4_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, INDEMISN2O_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, HFC_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, PFC_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, LUEMCO2_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, LUEMCH4_TOT)
EMISCO2EQ <- bind_rows(EMISCO2EQ, LUEMN2O_TOT)
EMISCO2EQ$main_sector <- factor(EMISCO2EQ$main_sector, levels=main_sector)
EMISCO2EQ <- select(EMISCO2EQ, year, region, main_sector, GHG_Category, value)

# for each year/region, main_sector sum all values to determine total GHG emissions, and add to table
EMISCO2EQ_tmp <- EMISCO2EQ %>% filter(main_sector=='Total') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_tmp <- ungroup(EMISCO2EQ_tmp)
EMISCO2EQ_tmp <- mutate(EMISCO2EQ_tmp, GHG_Category="EMISCO2EQ")
EMISCO2EQ_tmp <- mutate(EMISCO2EQ_tmp, main_sector='Total')
EMISCO2EQ_tmp$main_sector <- factor(EMISCO2EQ_tmp$main_sector, levels=main_sector)
EMISCO2EQ <- bind_rows(EMISCO2EQ, EMISCO2EQ_tmp)

# change GHG_Category and main_sector to factor
EMISCO2EQ$main_sector <- factor(EMISCO2EQ$main_sector, levels=main_sector)
EMISCO2EQ$GHG_Category <- factor(EMISCO2EQ$GHG_Category)

# calcualte total demand sector emissions (does not include f-gases)
EMIS <- bind_rows(ENEMISCO2_TOT, ENEMISCH4_TOT) %>% bind_rows(ENEMISN2O_TOT) %>% 
        bind_rows(INDEMISCO2_TOT) %>% bind_rows(INDEMISCH4_TOT) %>% bind_rows(INDEMISN2O_TOT) %>%
        bind_rows(HFC_TOT) %>% bind_rows(PFC_TOT)
EMIS_total <- subset(EMIS, main_sector=="Total")
EMIS_total <- EMIS_total %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_demand <- subset(EMIS, main_sector %in% c('Industry', 'Transport', 'Buildings'))
EMIS_demand <- EMIS_demand %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_supply <- subset(EMIS, main_sector=="Energy supply")
EMIS_supply <- EMIS_supply %>% group_by(year, region) %>% summarise(value=sum(value))

#calculate emissions per capita
Scenario$POP$value <- 10^6*Scenario$POP$value
EMISCO2EQpc <- left_join(EMISCO2EQ, Scenario$POP, by=c("region", "year"))
EMISCO2EQpc <- mutate(EMISCO2EQpc, value=value.x/value.y)
EMISCO2EQpc <- select(EMISCO2EQpc, year, region, main_sector, GHG_Category, value)
# next steps
# 1. Also make EMISCO2EQ_exclLULUCF
# 2. Can we do this with piping?
# temp2 <- EMISCO2EQ %>% group_by('year', 'region') %>% (function(x) sum(x$value))

#5.
# Calcualte share of renewable electricity production
Renewable <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_ren, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProd,Renewable)
tmp1 <- subset(tmp1, Renewable==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProd, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenElecShare <- tmp3 %>% group_by(year, region) %>% summarise(value=value.x/value.y)
RenElecShare <- data.frame(RenElecShare)

Renewable_excl_hydro <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_ren_excl_hydro, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProd,Renewable_excl_hydro)
tmp1 <- subset(tmp1, Renewable_excl_hydro==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProd, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenElecShare_excl_hydro <- tmp3 %>% group_by(year, region) %>% summarise(value=value.x/value.y)
RenElecShare_excl_hydro <- data.frame(RenElecShare_excl_hydro)

#6. Oil and gas intensity
# GHG intensity of oil and gas production (in ktCO2e/Mtoe)
CO2_oil <- filter(Scenario$ENEMISCO2, sector=="losses/leakages", energy_carrier=="Heavy oil")
CO2_gas <- filter(Scenario$ENEMISCO2, sector=="losses/leakages", energy_carrier=="Natural gas")
CH4_oil <- filter(Scenario$ENEMISCH4, sector=="losses/leakages", energy_carrier=="Heavy oil")
CH4_gas <- filter(Scenario$ENEMISCH4, sector=="losses/leakages", energy_carrier=="Natural gas")
CH4_oil$value <- CH4_oil$value*GWP_CH4
CH4_gas$value <- CH4_gas$value*GWP_CH4

OilGas_CO2 <- filter(Scenario$ENEMISCO2, sector=="losses/leakages", (energy_carrier=="Heavy oil" | energy_carrier=="Natural gas"))
OilGas_CO2$value <- OilGas_CO2$value*10^3*CToCO2
OilGas_CH4 <- filter(Scenario$ENEMISCH4, sector=="losses/leakages", (energy_carrier=="Heavy oil" | energy_carrier=="Natural gas"))
OilGas_CH4$value <- OilGas_CH4$value*GWP_CH4                     
OilGas_GHG <- bind_rows(OilGas_CO2, OilGas_CH4) 
OilGas_GHG_total <- OilGas_GHG %>% group_by(year, region) %>% summarise(value=sum(value))

OilGas_Prod <- filter(Scenario$EnergyProd, energy_carrier=="Heavy oil" | energy_carrier=="Light oil" | energy_carrier=="Natural gas")
OilGas_Prod$value <- OilGas_Prod$value*10^3*(1/MtoeToTJ) # from TJ to Mtoe
OilGas_Prod_total <- OilGas_Prod %>% group_by(year, region) %>% summarise(value=sum(value))
OilGas_Prod_total$value <- OilGas_Prod_total$value

OilGas_Intensity <- inner_join(OilGas_GHG_total, OilGas_Prod_total, by=c("year", "region"))
OilGas_Intensity <- mutate(OilGas_Intensity, value=value.x/value.y)
OilGas_Intensity <- select(OilGas_Intensity, year, region, value)
OilGas_Intensity$value <- OilGas_Intensity$value*1000
OilGas_Intensity <- data.frame(OilGas_Intensity)

# Energy intensity of industry sector (Kwh/US$2005)
Industry_FinalEnergy <- filter(Scenario$FinalEnergy, sector=="Industry", energy_carrier=="Total")
Industry_FinalEnergy <- select(Industry_FinalEnergy, year, region, value)
Industry_Efficiency <- inner_join(Industry_FinalEnergy, Scenario$IVA, by=c('year', 'region'))
Industry_Efficiency <- mutate(Industry_Efficiency, value=(10^3*(1/GWhToTJ)*10^6*value.x) / (10^6*value.y))
Industry_Efficiency <- select(Industry_Efficiency, year, region, value)
Industry_Efficiency <- data.frame(Industry_Efficiency)

#F-Gas index with reduction relative to 2010
HFC_TOT_tmp = select(HFC_TOT, year, region, value)
PFC_TOT_tmp = select(PFC_TOT, year, region, value)
FGases = inner_join(HFC_TOT_tmp, PFC_TOT_tmp, by = c('year', 'region'))
FGases = mutate(FGases, value=value.x+value.y)
FGases = select(FGases, year, region, value)
FGases_2010 = filter(FGases, year==2010)
FGases_2010 = select(FGases_2010, region, value)
FGas_Reduction_index = inner_join(FGases_2010, FGases, by=c('region'))
FGas_Reduction_index = mutate(FGas_Reduction_index, value=value.y/value.x)
FGas_Reduction_index = select(FGas_Reduction_index, year, region, value)

# Final Energy  per capita residential sector (GJ/capita)
Residential_FinalEnergy_capita <- filter(Scenario$FinalEnergy, sector=="Residential", energy_carrier=="Total")
Residential_FinalEnergy_capita <- select(Residential_FinalEnergy_capita, year, region, value)
Residential_Efficiency_capita <- inner_join(Residential_FinalEnergy_capita, Scenario$POP, by=c('year', 'region'))
Residential_Efficiency_capita <- mutate(Residential_Efficiency_capita, value=value.x / value.y)
Residential_Efficiency_capita <- select(Residential_Efficiency_capita, year, region, value)
Residential_Efficiency_capita$value <- Residential_Efficiency_capita$value*10^6
Residential_Efficiency_capita <- data.frame(Residential_Efficiency_capita)

# Final Energy  per m2 residential sector (GJ/m2)
Residential_FinalEnergy_m2 <- filter(Scenario$FinalEnergy, sector=="Residential", energy_carrier=="Total")
Residential_FinalEnergy_m2$value <- Residential_FinalEnergy_m2$value*10^6 # from PJ to GJ
Residential_FinalEnergy_m2 <- select(Residential_FinalEnergy_m2, year, region, value)
FloorSpace_total <- Scenario$FloorSpace
FloorSpace_total$value <- FloorSpace_total$value*10^6 # population is in millions
#FloorSpace_total <- filter(Scenario$Floorspace, population_group=="Total")
Residential_FinalEnergy_m2 <- inner_join(Residential_FinalEnergy_m2, FloorSpace_total, by=c('year', 'region'))
Residential_FinalEnergy_m2 <- mutate(Residential_FinalEnergy_m2, value=value.x / value.y)
Residential_FinalEnergy_m2 <- select(Residential_FinalEnergy_m2, year, region, value)
Residential_FinalEnergy_m2 <- data.frame(Residential_FinalEnergy_m2)

# Energy appliances per capita residential sector (GJ/capita)
Appliances_FinalEnergy_capita <- filter(Scenario$FinalEnergy_Residential, population_group=="Total", enduse_function=="HouseholdAppliances")
Appliances_FinalEnergy_capita <- select(Appliances_FinalEnergy_capita, year, region, value)
Appliances_FinalEnergy_capita <- inner_join(Appliances_FinalEnergy_capita, Scenario$POP, by=c('year', 'region'))
Appliances_FinalEnergy_capita <- mutate(Appliances_FinalEnergy_capita, value=value.x / value.y)
Appliances_FinalEnergy_capita <- select(Appliances_FinalEnergy_capita, year, region, value)
Appliances_FinalEnergy_capita <- data.frame(Appliances_FinalEnergy_capita)

# Car final energy use per kilometer
FuelUse_cars <- filter(Scenario$FinalEnergy_Transport, travel_mode=="Car")
FuelUse_cars <- select(FuelUse_cars, year, region, value)
PKm_cars <- filter(Scenario$PersonKilometers, travel_mode=="Car")
Pkm_cars <- select(PKm_cars, year, region, value)
FuelUse_pkm_cars <- inner_join(FuelUse_cars, Pkm_cars, by=c('year', 'region'))
FuelUse_pkm_cars <- mutate(FuelUse_pkm_cars, value=value.x / value.y)
FuelUse_pkm_cars <- select(FuelUse_pkm_cars, year, region, value)
# Convert to km/l
FuelUse_pkm_cars$value <- (MJ_l_gasoline/FuelUse_pkm_cars$value)/Load_car
FuelUse_pkm_cars <- data.frame(FuelUse_pkm_cars)

# Share of Electric cars
ElectricCars_share <- filter(Scenario$VehicleShare_cars, car_type=="BEV" | car_type=="BEV 150")
ElectricCars_share <- ElectricCars_share %>% group_by(year, region) %>% summarise(value=sum(value))
ElectricCars_share <- select(ElectricCars_share, year, region, value)
ElectricCars_share <- ungroup(ElectricCars_share)

l <- list(EMISCO2EQ=EMISCO2EQ,EMISCO2EQpc=EMISCO2EQpc, EMIS_demand=EMIS_demand, EMIS_supply=EMIS_supply, FGases=FGases,
          RenElecShare=RenElecShare, RenElecShare_excl_hydro=RenElecShare_excl_hydro, OilGas_Intensity = OilGas_Intensity, 
          IndustryEfficiency = Industry_Efficiency, FGas_Reduction_index = FGas_Reduction_index, 
          Residential_Efficiency_capita=Residential_Efficiency_capita, Residential_FinalEnergy_m2=Residential_FinalEnergy_m2, 
          Appliances_FinalEnergy_capita=Appliances_FinalEnergy_capita,
          FuelUse_pkm_cars=FuelUse_pkm_cars, ElectricCars_share=ElectricCars_share)
}