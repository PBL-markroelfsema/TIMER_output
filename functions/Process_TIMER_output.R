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
# make EMISCO2EQ excl LULUCF - DONE, ok?
# make variable with individual and total GHG emissions per sector (energy supply, transport, industry, buildings, AFOLU, bunkers) - in progress: bunkers to do, all sectors add individual GHGs
# CO2_intensity_GDP - DONE, ok?
# Energy_intensity_TPES_GDP - DONE, ok?

ProcessTimerScenario <- function(Scenario)
{ s <- deparse(substitute(Scenario)) # get object name as string
  if(!exists(s))
  { print(paste("Scenario ", s, " is not imported yet. First execute ImportTimerFile", sep=""))
    stop()
  }

source(paste('functions', 'Settings.R', sep='/'))
source(paste('functions', 'General Functions.R', sep='/'))


# Emissions ---------------------------------------------------------------


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

### 3b. Sector aggregates - before summing to total GHG  incl./excl. LULUCF ###
#3b1. Agriculture emissions: CH4 and N2O
EMISCH4_AGRI = data.table(Scenario$LUEMCH4)[source%in%c('wetland rice', 'animals', 'animal waste') & year >= StartYear]
EMISCH4_AGRI$value = EMISCH4_AGRI$value*GWP_CH4
EMISCH4_AGRI = mutate(EMISCH4_AGRI, main_sector=mapply(function(x) MainSector(x), source))
EMISCH4_AGRI <- mutate(EMISCH4_AGRI, GHG_Category="LUEMCH4")
EMISCH4_AGRI = select(EMISCH4_AGRI, year, region, main_sector, GHG_Category, value)

EMISN2O_AGRI = data.table(Scenario$LUEMN2O)[source%in%c('fertilizers', 'stables', 'grazing', 'manure application', 'fertilizers indirect') & year >= StartYear]
EMISN2O_AGRI$value = EMISN2O_AGRI$value*NToN2O*GWP_N2O
EMISN2O_AGRI = mutate(EMISN2O_AGRI, main_sector=mapply(function(x) MainSector(x), source))
EMISN2O_AGRI <- mutate(EMISN2O_AGRI, GHG_Category="LUEMN2O")
EMISN2O_AGRI = select(EMISN2O_AGRI, year, region, main_sector, GHG_Category, value)

#Sum CH4 and N2O for agriculture
EMISCO2EQ_AGRI <- bind_rows(EMISCH4_AGRI, EMISN2O_AGRI)
EMISCO2EQ_AGRI <- select(EMISCO2EQ_AGRI, year, region, main_sector, GHG_Category, value)
EMISCO2EQ_AGRI_tmp <- EMISCO2EQ_AGRI %>% filter(main_sector=='Agriculture') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_AGRI_tmp <- ungroup(EMISCO2EQ_AGRI_tmp)
EMISCO2EQ_AGRI_tmp <- mutate(EMISCO2EQ_AGRI_tmp, GHG_Category="AGREMISCO2EQ")
EMISCO2EQ_AGRI <- mutate(EMISCO2EQ_AGRI_tmp, main_sector='Total') #Agriculture?
EMISCO2EQ_AGRI$GHG_Category <- factor(EMISCO2EQ_AGRI$GHG_Category)
EMISCO2EQ_AGRI$main_sector <- factor(EMISCO2EQ_AGRI$main_sector)
                                 
#3b2. LULUCF emissions: CO2, CH4 and N2O
EMISCO2_LU = LUEMCO2_TOT %>% mutate(main_sector="Land-use")

EMISCH4_LU = data.table(Scenario$LUEMCH4)[source%in%c('biomass burning', 'fuelwood burning', 'agricultural waste burning', 'savanna burning') & year >= StartYear]
EMISCH4_LU$value = EMISCH4_LU$value*GWP_CH4
EMISCH4_LU = mutate(EMISCH4_LU, main_sector=mapply(function(x) MainSector(x), source))
EMISCH4_LU <- mutate(EMISCH4_LU, GHG_Category="LUEMCH4")
EMISCH4_LU = select(EMISCH4_LU, year, region, main_sector, GHG_Category, value)

EMISN2O_LU = data.table(Scenario$LUEMN2O)[source%in%c('deforestation/biomass burning', 'traditional biomass/fuelwood burning', 'agricultural waste burning', 'savanna burning', 'land clearing', 'from residues', 'biological N-fixation') & year >= StartYear]
EMISN2O_LU$value = EMISN2O_LU$value*NToN2O*GWP_N2O
EMISN2O_LU = mutate(EMISN2O_LU, main_sector=mapply(function(x) MainSector(x), source))
EMISN2O_LU <- mutate(EMISN2O_LU, GHG_Category="LUEMN2O")
EMISN2O_LU = select(EMISN2O_LU, year, region, main_sector, GHG_Category, value)

#Sum CO2, CH4 and N2O for land-use
EMISCO2EQ_LU <- bind_rows(EMISCH4_LU, EMISN2O_LU, EMISCO2_LU)
EMISCO2EQ_LU <- select(EMISCO2EQ_LU, year, region, main_sector, GHG_Category, value)
EMISCO2EQ_LU_tmp <- EMISCO2EQ_LU %>% filter(main_sector=='Land-use') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_LU_tmp <- ungroup(EMISCO2EQ_LU_tmp)
EMISCO2EQ_LU_tmp <- mutate(EMISCO2EQ_LU_tmp, GHG_Category="LUEMISCO2EQ")
EMISCO2EQ_LU <- mutate(EMISCO2EQ_LU_tmp, main_sector='Total') #Land-use?
EMISCO2EQ_LU$GHG_Category <- factor(EMISCO2EQ_LU$GHG_Category)
EMISCO2EQ_LU$main_sector <- factor(EMISCO2EQ_LU$main_sector)

#3b3. Waste emissions:CH4 and N2O
EMISCH4_WAS = data.table(Scenario$LUEMCH4)[source%in%c('landfills', 'sewage') & year >= StartYear]
EMISCH4_WAS$value = EMISCH4_WAS$value*GWP_CH4
EMISCH4_WAS = mutate(EMISCH4_WAS, main_sector=mapply(function(x) MainSector(x), source))
EMISCH4_WAS <- mutate(EMISCH4_WAS, GHG_Category="LUEMCH4")
EMISCH4_WAS = select(EMISCH4_WAS, year, region, main_sector, GHG_Category, value)

EMISN2O_WAS = data.table(Scenario$LUEMN2O)[source%in%c('domestic sewage') & year >= StartYear]
EMISN2O_WAS$value = EMISN2O_WAS$value*NToN2O*GWP_N2O
EMISN2O_WAS = mutate(EMISN2O_WAS, main_sector=mapply(function(x) MainSector(x), source))
EMISN2O_WAS <- mutate(EMISN2O_WAS, GHG_Category="LUEMN2O")
EMISN2O_WAS = select(EMISN2O_WAS, year, region, main_sector, GHG_Category, value)

#Sum CH4 and N2O for waste
EMISCO2EQ_WAS <- bind_rows(EMISCH4_WAS, EMISN2O_WAS)
EMISCO2EQ_WAS <- select(EMISCO2EQ_WAS, year, region, main_sector, GHG_Category, value)
EMISCO2EQ_WAS_tmp <- EMISCO2EQ_WAS %>% filter(main_sector=='Waste') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_WAS_tmp <- ungroup(EMISCO2EQ_WAS_tmp)
EMISCO2EQ_WAS_tmp <- mutate(EMISCO2EQ_WAS_tmp, GHG_Category="WASEMISCO2EQ")
EMISCO2EQ_WAS <- mutate(EMISCO2EQ_WAS_tmp, main_sector='Total') #Waste?
EMISCO2EQ_WAS$GHG_Category <- factor(EMISCO2EQ_WAS$GHG_Category)
EMISCO2EQ_WAS$main_sector <- factor(EMISCO2EQ_WAS$main_sector)

# 4.
# Add all total aggregated emissions to on table EMISCO2EQ, and calculate total GHG emissions incl/excl LULUCF CO2
EMISCO2EQ  <- bind_rows(ENEMISCO2_TOT,ENEMISCH4_TOT,ENEMISN2O_TOT,
                        INDEMISCH4_TOT,INDEMISCO2_TOT,INDEMISN2O_TOT,
                        HFC_TOT,PFC_TOT,
                        LUEMCO2_TOT,LUEMCH4_TOT,LUEMN2O_TOT)
EMISCO2EQexcl  <- bind_rows(ENEMISCO2_TOT,ENEMISCH4_TOT,ENEMISN2O_TOT,
                            INDEMISCH4_TOT,INDEMISCO2_TOT,INDEMISN2O_TOT,
                            HFC_TOT,PFC_TOT,
                            EMISCO2EQ_AGRI,EMISCO2EQ_WAS)
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

# Same for CO2eq excl.
EMISCO2EQexcl$main_sector <- factor(EMISCO2EQexcl$main_sector, levels=main_sector)
EMISCO2EQexcl <- select(EMISCO2EQexcl, year, region, main_sector, GHG_Category, value)
EMISCO2EQexcl_tmp <- EMISCO2EQexcl %>% filter(main_sector=='Total') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQexcl_tmp <- ungroup(EMISCO2EQexcl_tmp)
EMISCO2EQexcl_tmp <- mutate(EMISCO2EQexcl_tmp, GHG_Category="EMISCO2EQ")
EMISCO2EQexcl_tmp <- mutate(EMISCO2EQexcl_tmp, main_sector='Total')
EMISCO2EQexcl_tmp$main_sector <- factor(EMISCO2EQexcl_tmp$main_sector, levels=main_sector)
EMISCO2EQexcl <- bind_rows(EMISCO2EQexcl, EMISCO2EQexcl_tmp)
EMISCO2EQexcl$main_sector <- factor(EMISCO2EQexcl$main_sector, levels=main_sector)
EMISCO2EQexcl$GHG_Category <- factor(EMISCO2EQexcl$GHG_Category)

# calculate total demand sector emissions (does not include f-gases)
EMIS <- bind_rows(ENEMISCO2_TOT, ENEMISCH4_TOT) %>% bind_rows(ENEMISN2O_TOT) %>% 
        bind_rows(INDEMISCO2_TOT) %>% bind_rows(INDEMISCH4_TOT) %>% bind_rows(INDEMISN2O_TOT) %>%
        bind_rows(HFC_TOT) %>% bind_rows(PFC_TOT)
EMIS_total <- subset(EMIS, main_sector=="Total")
EMIS_total <- EMIS_total %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_demand <- subset(EMIS, main_sector %in% c('Industry', 'Transport', 'Buildings'))
EMIS_demand <- EMIS_demand %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_supply <- subset(EMIS, main_sector=="Energy supply")
EMIS_supply <- EMIS_supply %>% group_by(year, region) %>% summarise(value=sum(value))

# Calculate individual and total GHG sector emissions
EMIS_buildings <- subset(EMIS,main_sector%in%c("Buildings"))
EMIS_buildings <- EMIS_buildings %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_transport <- subset(EMIS,main_sector%in%c("Transport"))
EMIS_transport <- EMIS_transport %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_industry <- subset(EMIS,main_sector%in%c("Industry"))
EMIS_industry <- EMIS_industry %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_AFOLU <- bind_rows(EMISCO2EQ_AGRI, EMISCO2EQ_LU)
EMIS_AFOLU <- EMIS_AFOLU %>% group_by(year, region) %>% summarise(value=sum(value))

# Calculate total CO2 emissions
EMISCO2  <- bind_rows(ENEMISCO2_TOT,INDEMISCO2_TOT,LUEMCO2_TOT)
EMISCO2$main_sector <- factor(EMISCO2$main_sector, levels=main_sector)
EMISCO2 <- select(EMISCO2, year, region, main_sector, GHG_Category, value)
EMISCO2_tmp <- EMISCO2 %>% filter(main_sector=='Total') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2_tmp <- ungroup(EMISCO2_tmp)
EMISCO2_tmp <- mutate(EMISCO2_tmp, GHG_Category="EMISCO2")
EMISCO2_tmp <- mutate(EMISCO2_tmp, main_sector='Total')
EMISCO2_tmp$main_sector <- factor(EMISCO2_tmp$main_sector, levels=main_sector)
EMISCO2 <- bind_rows(EMISCO2, EMISCO2_tmp)
EMISCO2$main_sector <- factor(EMISCO2$main_sector, levels=main_sector)
EMISCO2$GHG_Category <- factor(EMISCO2$GHG_Category)

#calculate emissions per capita
Scenario$POP$value <- 10^6*Scenario$POP$value
EMISCO2EQpc <- left_join(EMISCO2EQ, Scenario$POP, by=c("region", "year"))
EMISCO2EQpc <- mutate(EMISCO2EQpc, value=value.x/value.y)
EMISCO2EQpc <- select(EMISCO2EQpc, year, region, main_sector, GHG_Category, value)
# next steps
# 1. Also make EMISCO2EQ_exclLULUCF - done, ok?
# 2. Can we do this with piping?
# temp2 <- EMISCO2EQ %>% group_by('year', 'region') %>% (function(x) sum(x$value))

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


# Electricity -------------------------------------------------------------

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
RenElecShare <- mutate(RenElecShare, unit="%")

Renewable_excl_hydro <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_ren_excl_hydro, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProd,Renewable_excl_hydro)
tmp1 <- subset(tmp1, Renewable_excl_hydro==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProd, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenElecShare_excl_hydro <- tmp3 %>% group_by(year, region) %>% summarise(value=value.x/value.y)
RenElecShare_excl_hydro <- data.frame(RenElecShare_excl_hydro)
RenElecShare_excl_hydro <- mutate(RenElecShare_excl_hydro, unit="%")

NonFossil <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_nf, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProd,NonFossil)
tmp1 <- subset(tmp1, NonFossil==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProd, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
NonFossilElecShare <- tmp3 %>% group_by(year, region) %>% summarise(value=value.x/value.y)
NonFossilElecShare <- data.frame(NonFossilElecShare)
NonFossilElecShare <- mutate(NonFossilElecShare, unit="%")

#7. Electricity capacity
# List electricity capacity per technology
ElecCap=data.table(Scenario$ElecCap)
ElecCapGeo <- ElecCap[energy_technology=="Other Renewable"]
ElecCapWindOff <- ElecCap[energy_technology=="Wind Offshore"]
ElecCapWindOn<- ElecCap[energy_technology=="Wind Onshore"]
ElecCapSolarPV<- ElecCap[energy_technology=="PV"]
ElecCapSolarCSP<- ElecCap[energy_technology=="CSP"]
ElecCapHydro<- ElecCap[energy_technology=="Hydro"]
ElecCapWaste<- ElecCap[energy_technology=="Waste"]
ElecCapNuclear<- ElecCap[energy_technology=="Nuclear"]
ElecCapCoalCCS<- ElecCap[energy_technology=="Coal + CS"]
ElecCapCoalTrad<- ElecCap[energy_technology=="Conv. Coal"]

#Calculate some technologies as sum of others
ElecCapTot<- spread(ElecCap,energy_technology,value) %>% mutate(Solar = PV + CSP, Wind = `Wind Offshore` + `Wind Onshore`, 
                                                                Coal = `Conv. Coal`+`Coal + CS`+`CHP coal`+`CHP coal + CS`,
                                                                Biomass = `Waste`+`Biomass CC`+`Biomass + CS`+`CHP biomass`+`CHP biomass + CS`,
                                                                WindSolar=Solar+Wind,
                                                                Renewable= Solar+Wind+Hydro+Biomass+`Other Renewable`+Nuclear) 
ElecCapTot <- data.table(gather(ElecCapTot,energy_technology,value,c(PV:Renewable)))
ElecCapCoalTot<- ElecCapTot[energy_technology=="Coal"] 
ElecCapBioTot <- ElecCapTot[energy_technology=="Biomass"]
ElecCapSolarTot <- ElecCapTot[energy_technology=="Solar"]
ElecCapWindTot <- ElecCapTot[energy_technology=="Wind"]
ElecCapRenTot <- ElecCapTot[energy_technology=="Renewable"]
ElecCapWSTot <- ElecCapTot[energy_technology=="WindSolar"]

# Electricity access
ElecAccTot=data.table(Scenario$ElecAcc)
ElecAccTot=ElecAccTot[population_group=="Total"]

# Intensity ---------------------------------------------------------------

# CO2 intensity of GDP
CO2_intensity <- merge(EMISCO2,Scenario$GDP_MER,by=c('year','region')) %>% mutate(value=value.x/value.y, unit.int=paste("MtCO2/",unit,sep=""))%>%filter(main_sector=="Total" & GHG_Category=="EMISCO2")%>%select(year,region,main_sector,GHG_Category,value,unit.int)
setnames(CO2_intensity,"unit.int","unit")
CO2_intensity_2015 = filter(CO2_intensity, year==2015)
CO2_intensity_2015 = select(CO2_intensity_2015, region, value)
CO2_intensity_index = inner_join(CO2_intensity_2015, CO2_intensity, by=c('region'))
CO2_intensity_index = mutate(CO2_intensity_index, value=value.y/value.x,unit="index 2015")
CO2_intensity_index = select(CO2_intensity_index, year, region, value,unit)

# Energy intensity of GDP
TPES = data.table(Scenario$TPES)[energy_carrier == "Total" & year >= StartYear]
TPES_intensity <- merge(TPES,Scenario$GDP_MER,by=c('year','region'))%>% mutate(value=1000*value.x/value.y, unit="MJ/US$(2005)")%>%select(year,region,energy_carrier,value,unit)
TPES_intensity_2015 = filter(TPES_intensity, year==2015)
TPES_intensity_2015 = select(TPES_intensity_2015, region, value)
TPES_intensity_index = inner_join(TPES_intensity_2015, TPES_intensity, by=c('region'))
TPES_intensity_index = mutate(TPES_intensity_index, value=value.y/value.x,unit="index 2015")
TPES_intensity_index = select(TPES_intensity_index, year, region, value,unit)

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

# Energy use --------------------------------------------------------------

#Share of renewables in TPES
Renewable <- ifelse(Scenario$TPES$energy_carrier %in% energy_carrier_ren, TRUE, FALSE)
tmp1 <- cbind(Scenario$TPES,Renewable)
tmp1 <- subset(tmp1, Renewable==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$TPES, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenTPESShare <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
RenTPESShare <- data.frame(RenTPESShare)
RenTPESShare <- mutate(RenTPESShare, unit="%")

#including nuclear
RenewableN <- ifelse(Scenario$TPES$energy_carrier %in% c('Modern biofuels', 'Solar/wind', 'Hydro-electricity','Nuclear'), TRUE, FALSE)
tmp1 <- cbind(Scenario$TPES,RenewableN)
tmp1 <- subset(tmp1, RenewableN==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$TPES, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenNucTPESShare <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
RenNucTPESShare <- data.frame(RenNucTPESShare)
RenNucTPESShare <- mutate(RenNucTPESShare, unit="%")

# Buildings ---------------------------------------------------------------


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
Residential_FinalEnergy_m2 <- mutate(Residential_FinalEnergy_m2, unit="GJ/m2")

# Energy appliances per capita residential sector (GJ/capita)
Appliances_FinalEnergy_capita <- filter(Scenario$FinalEnergy_Residential, population_group=="Total", enduse_function=="HouseholdAppliances")
Appliances_FinalEnergy_capita <- select(Appliances_FinalEnergy_capita, year, region, value)
Appliances_FinalEnergy_capita <- inner_join(Appliances_FinalEnergy_capita, Scenario$POP, by=c('year', 'region'))
Appliances_FinalEnergy_capita <- mutate(Appliances_FinalEnergy_capita, value=value.x / value.y)
Appliances_FinalEnergy_capita <- select(Appliances_FinalEnergy_capita, year, region, value)
Appliances_FinalEnergy_capita <- data.frame(Appliances_FinalEnergy_capita)


# Transport ---------------------------------------------------------------


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

# Car CO2 per km
CO2_km_cars <- filter(Scenario$TransportCO2Emissions, travel_mode=='Car') %>% select(year, region, value) %>% mutate(v="CO2")
Pkm_cars <- filter(Scenario$PersonKilometers, travel_mode=='Car') %>% select(year, region, value) %>% mutate(v='pkm')
CO2_km_cars <- rbind(CO2_km_cars, Pkm_cars)
CO2_km_cars <- spread(CO2_km_cars, key=v, value=value)
# convert to gCO2/km (from Mt/Tkm) and in vehicle kilomters, instead of pkm
CO2_km_cars <- mutate(CO2_km_cars, value=(Load_car/Tera)*10^12*CO2/pkm) %>% select(year, region, value)
CO2_km_cars <- mutate(CO2_km_cars, unit= "gCO2/km")

# Share of Electric cars
ElectricCars_share <- filter(Scenario$VehicleShare_cars, car_type=="BEV" | car_type=="BEV 150")
ElectricCars_share <- ElectricCars_share %>% group_by(year, region) %>% summarise(value=sum(value))
ElectricCars_share <- select(ElectricCars_share, year, region, value)
ElectricCars_share <- ungroup(ElectricCars_share)


# Industry ----------------------------------------------------------------

# Energy intensity of industry sector (Kwh/US$2005)
Industry_FinalEnergy <- filter(Scenario$FinalEnergy, sector=="Industry", energy_carrier=="Total")
Industry_FinalEnergy <- select(Industry_FinalEnergy, year, region, value)
Industry_Efficiency <- inner_join(Industry_FinalEnergy, Scenario$IVA, by=c('year', 'region'))
Industry_Efficiency <- mutate(Industry_Efficiency, value=(10^3*(1/GWhToTJ)*10^6*value.x) / (10^6*value.y))
Industry_Efficiency <- select(Industry_Efficiency, year, region, value)
Industry_Efficiency <- data.frame(Industry_Efficiency)

# INDUSTRY

# final energy per IVA (PJ/million US$(2005)
Industry_Energy <- filter(Scenario$FinalEnergy, sector=="Industry", energy_carrier=="Total") %>% select(year, region, value) %>% mutate(v="energy")
Industry_IVA <- select(Scenario$IVA, year, region, value) %>% mutate(v="IVA")
Industry_Energy_IVA <- rbind(Industry_Energy, Industry_IVA)
Industry_Energy_IVA <- spread(Industry_Energy_IVA, key=v, value=value)
Industry_Energy_IVA <- mutate(Industry_Energy_IVA, value=energy/IVA) %>% select(year, region, value)
Industry_Energy_IVA <- mutate(Industry_Energy_IVA, unit="PJ/million US$(2005)")


# Compile list ------------------------------------------------------------
abc <- 0

l <- list(EMISCO2EQ=EMISCO2EQ,EMISCO2EQexcl=EMISCO2EQexcl,EMISCO2EQpc=EMISCO2EQpc, EMISCO2=EMISCO2,
          EMIS_demand=EMIS_demand,EMIS_buildings=EMIS_buildings,EMIS_supply=EMIS_supply,EMIS_industry=EMIS_industry,EMIS_transport=EMIS_transport,
          EMISCO2EQ_AGRI=EMISCO2EQ_AGRI,EMISCO2EQ_LU=EMISCO2EQ_LU,EMISCO2EQ_WAS=EMISCO2EQ_WAS,LUEMCO2_TOT=LUEMCO2_TOT,EMIS_AFOLU=EMIS_AFOLU,
          FGases=FGases,HFC_TOT=HFC_TOT,
          RenElecShare=RenElecShare, RenElecShare_excl_hydro=RenElecShare_excl_hydro, NonFossilElecShare=NonFossilElecShare,RenTPESShare=RenTPESShare,RenNucTPESShare=RenNucTPESShare,
          ElecCapGeo=ElecCapGeo, ElecCapWindOff=ElecCapWindOff, ElecCapWindOn=ElecCapWindOn, ElecCapSolarPV=ElecCapSolarPV, ElecCapSolarCSP=ElecCapSolarCSP, ElecCapHydro=ElecCapHydro, 
          ElecCapWaste=ElecCapWaste, ElecCapNuclear=ElecCapNuclear, ElecCapCoalCCS=ElecCapCoalCCS, ElecCapCoalTrad=ElecCapCoalTrad,
          ElecCapCoalTot=ElecCapCoalTot, ElecCapBioTot=ElecCapBioTot, ElecCapSolarTot=ElecCapSolarTot, ElecCapWindTot=ElecCapWindTot, ElecCapRenTot=ElecCapRenTot, ElecCapWSTot=ElecCapWSTot, 
          OilGas_Intensity = OilGas_Intensity, CO2_intensity=CO2_intensity,CO2_intensity_index=CO2_intensity_index,TPES_intensity=TPES_intensity,TPES_intensity_index=TPES_intensity_index,
          IndustryEfficiency = Industry_Efficiency, FGas_Reduction_index = FGas_Reduction_index, 
          Residential_Efficiency_capita=Residential_Efficiency_capita, Residential_FinalEnergy_m2=Residential_FinalEnergy_m2, 
          Appliances_FinalEnergy_capita=Appliances_FinalEnergy_capita,
          CO2_km_cars=CO2_km_cars, FuelUse_pkm_cars=FuelUse_pkm_cars, ElectricCars_share=ElectricCars_share,
          Industry_Energy_IVA=Industry_Energy_IVA,
          ElecAccTot=ElecAccTot)
}
