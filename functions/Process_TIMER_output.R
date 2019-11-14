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

ProcessTimerScenario <- function(Scenario, Rundir, Project, Policy = FALSE)
{ source("../TIMER_output/functions/Settings.R")
  s <- deparse(substitute(Scenario)) # get object name as string
  if(!exists(s))
  { print(paste("Scenario ", s, " is not imported yet. First execute ImportTimerFile", sep=""))
    stop()
  }

source(paste(Rundir, Project, '6_R/TIMER_output/functions', 'Settings.R', sep='/'))
source(paste(Rundir, Project, '6_R/TIMER_output/functions', 'General Functions.R', sep='/'))


# Emissions ---------------------------------------------------------------
cat(sprintf("Emissions: \n"))

#3. 
# Aggregate emissions to CO2eq per TIMER file
ENEMISCO2_TOT = subset(Scenario$ENEMISCO2, energy_carrier == "Total" & year >= StartYear)
ENEMISCO2_TOT$value = ENEMISCO2_TOT$value*10^3*CToCO2
ENEMISCO2_TOT = mutate(ENEMISCO2_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISCO2_TOT$main_sector = factor(ENEMISCO2_TOT$main_sector,levels=main_sector)
ENEMISCO2_TOT = select(ENEMISCO2_TOT, year, region, main_sector, value) %>%
                group_by(year, region, main_sector) %>%
                summarise(value=sum(value, na.rm=TRUE))
ENEMISCO2_TOT <- mutate(ENEMISCO2_TOT, GHG_Category="ENEMISCO2")
ENEMISCO2_TOT <- mutate(ENEMISCO2_TOT, unit="MtCO2eq")

ENEMISCH4_TOT = data.table(Scenario$ENEMISCH4)[energy_carrier == "Total" & year >= StartYear]
ENEMISCH4_TOT$value = ENEMISCH4_TOT$value*GWP_CH4
ENEMISCH4_TOT = mutate(ENEMISCH4_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISCH4_TOT$main_sector = factor(ENEMISCH4_TOT$main_sector,levels=main_sector)
ENEMISCH4_TOT = select(ENEMISCH4_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
ENEMISCH4_TOT <- mutate(ENEMISCH4_TOT, GHG_Category="ENEMISCH4")
ENEMISCH4_TOT <- mutate(ENEMISCH4_TOT, unit="MtCO2eq")

ENEMISN2O_TOT = data.table(Scenario$ENEMISN2O)[energy_carrier == "Total" & year >= StartYear]
ENEMISN2O_TOT$value = ENEMISN2O_TOT$value*NToN2O*GWP_N2O
ENEMISN2O_TOT = mutate(ENEMISN2O_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISN2O_TOT$main_sector = factor(ENEMISN2O_TOT$main_sector,levels=main_sector)
ENEMISN2O_TOT = select(ENEMISN2O_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
ENEMISN2O_TOT <- mutate(ENEMISN2O_TOT, GHG_Category="ENEMISN2O")
ENEMISN2O_TOT <- mutate(ENEMISN2O_TOT, unit="MtCO2eq")

INDEMISCO2_TOT = data.table(Scenario$INDEMISCO2)[industrial_process == "total" & year >= StartYear]
INDEMISCO2_TOT$value = INDEMISCO2_TOT$value*10^3*CToCO2
INDEMISCO2_TOT = mutate(INDEMISCO2_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISCO2_TOT$main_sector = factor(INDEMISCO2_TOT$main_sector,levels=main_sector)
tmp <- INDEMISCO2_TOT %>% mutate(main_sector="Industry")
INDEMISCO2_TOT <- rbind(INDEMISCO2_TOT, tmp)
INDEMISCO2_TOT = select(INDEMISCO2_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
INDEMISCO2_TOT <- mutate(INDEMISCO2_TOT, GHG_Category="INDEMISCO2")
INDEMISCO2_TOT <- mutate(INDEMISCO2_TOT, unit="MtCO2eq")

INDEMISCH4_TOT = data.table(Scenario$INDEMISCH4)[industrial_process == "total" & year >= StartYear]
INDEMISCH4_TOT$value = INDEMISCH4_TOT$value*GWP_CH4
INDEMISCH4_TOT = mutate(INDEMISCH4_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISCH4_TOT$main_sector = factor(INDEMISCH4_TOT$main_sector,levels=main_sector)
tmp <- INDEMISCH4_TOT %>% mutate(main_sector="Industry")
INDEMISCH4_TOT <- rbind(INDEMISCH4_TOT, tmp)
INDEMISCH4_TOT = select(INDEMISCH4_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
INDEMISCH4_TOT <- mutate(INDEMISCH4_TOT, GHG_Category="INDEMISCH4")
INDEMISCH4_TOT <- mutate(INDEMISCH4_TOT, unit="MtCO2eq")

INDEMISN2O_TOT = data.table(Scenario$INDEMISN2O)[industrial_process == "total" & year >= StartYear]
INDEMISN2O_TOT$value = INDEMISN2O_TOT$value*NToN2O*GWP_N2O
INDEMISN2O_TOT = mutate(INDEMISN2O_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISN2O_TOT$main_sector = factor(INDEMISN2O_TOT$main_sector,levels=main_sector)
tmp <- INDEMISN2O_TOT %>% mutate(main_sector="Industry")
INDEMISN2O_TOT <- rbind(INDEMISN2O_TOT, tmp)
INDEMISN2O_TOT = select(INDEMISN2O_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
INDEMISN2O_TOT <- mutate(INDEMISN2O_TOT, GHG_Category="INDEMISN2O")
INDEMISN2O_TOT <- mutate(INDEMISN2O_TOT, unit="MtCO2eq")

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
HFC_TOT = mutate(HFC_TOT, main_sector=mapply(function(x) MainSector(x), HFC_gas))
HFC_TOT$main_sector = factor(HFC_TOT$main_sector,levels=main_sector)
HFC_TOT_indicator <- HFC_TOT
HFC_TOT_indicator <- mutate(HFC_TOT_indicator, unit="MtCO2eq")
tmp <- HFC_TOT %>% mutate(main_sector="Industry")
HFC_TOT <- rbind(HFC_TOT, tmp)
HFC_TOT = select(HFC_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
HFC_TOT <- mutate(HFC_TOT, GHG_Category="HFC")
HFC_TOT <- mutate(HFC_TOT, unit="MtCO2eq")

PFC_TOT = data.table(Scenario$PFC_reg)[year >= StartYear]
PFC_TOT[PFC_gas=="CF4"]$value =   10^-3*PFC_TOT[PFC_gas=="CF4"]$value*GWP_CF4
PFC_TOT[PFC_gas=="C2F6"]$value =  10^-3*PFC_TOT[PFC_gas=="C2F6"]$value*GWP_C2F6
PFC_TOT[PFC_gas=="C6F14"]$value = 10^-3*PFC_TOT[PFC_gas=="C6F14"]$value*GWP_C6F14 
PFC_TOT[PFC_gas=="SF6"]$value =   10^-3*PFC_TOT[PFC_gas=="SF6"]$value*GWP_SF6 
PFC_TOT <- PFC_TOT %>% group_by(year, region) %>% summarise(value=sum(value))
PFC_TOT <- ungroup(PFC_TOT)
PFC_TOT <- mutate(PFC_TOT, PFC_gas="Total")
PFC_TOT = mutate(PFC_TOT, main_sector=mapply(function(x) MainSector(x), PFC_gas))
PFC_TOT$main_sector = factor(PFC_TOT$main_sector,levels=main_sector)
tmp <- PFC_TOT %>% mutate(main_sector="Industry")
PFC_TOT <- rbind(PFC_TOT, tmp)
PFC_TOT = select(PFC_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
PFC_TOT <- mutate(PFC_TOT, GHG_Category="PFC")
PFC_TOT <- mutate(PFC_TOT, unit="MtCO2eq")

# First determine emissions per main_sector
# Then determine total emissions
LUEMCO2_TOT = data.table(Scenario$LUEMCO2)[year >= StartYear]
LUEMCO2_TOT <- filter(LUEMCO2_TOT, source != 'Total')
LUEMCO2_TOT$value = LUEMCO2_TOT$value*10^3*CToCO2
LUEMCO2_TOT = mutate(LUEMCO2_TOT, main_sector=mapply(function(x) MainSector(x), source))
LUEMCO2_TOT$main_sector = factor(LUEMCO2_TOT$main_sector,levels=main_sector)
LUEMCO2_TOT = select(LUEMCO2_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
LUEMCO2_TOT_indicator=LUEMCO2_TOT # TODO, gives errors?
LUEMCO2_TOT_indicator$unit<-"MtCO2eq"
LUEMCO2_TOT_indicator<-filter(LUEMCO2_TOT_indicator,main_sector=="LULUCF") %>% select(year,region,unit,value)
tmp <- select(LUEMCO2_TOT, year, region, main_sector, value) %>%
  group_by(year, region) %>%
  summarise(value=sum(value, na.rm=TRUE))
tmp <- mutate(tmp, main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
LUEMCO2_TOT <- rbind(LUEMCO2_TOT, tmp)
LUEMCO2_TOT = select(LUEMCO2_TOT, year, region, main_sector, value)
LUEMCO2_TOT <- mutate(LUEMCO2_TOT, GHG_Category="LUEMCO2")
LUEMCO2_TOT <- mutate(LUEMCO2_TOT, unit="MtCO2eq")

LUEMCH4_TOT = data.table(Scenario$LUEMCH4)[year >= StartYear]
LUEMCH4_TOT <- filter(LUEMCH4_TOT, source != 'Total')
LUEMCH4_TOT$value = LUEMCH4_TOT$value*GWP_CH4
LUEMCH4_TOT = mutate(LUEMCH4_TOT, main_sector=mapply(function(x) MainSector(x), source))
LUEMCH4_TOT$main_sector = factor(LUEMCH4_TOT$main_sector,levels=main_sector)
LUEMCH4_TOT = select(LUEMCH4_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
tmp <- select(LUEMCH4_TOT, year, region, main_sector, value) %>%
  group_by(year, region) %>%
  summarise(value=sum(value, na.rm=TRUE))
tmp <- mutate(tmp, main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
LUEMCH4_TOT <- rbind(LUEMCH4_TOT, tmp)
LUEMCH4_TOT = select(LUEMCH4_TOT, year, region, main_sector, value)
LUEMCH4_TOT <- mutate(LUEMCH4_TOT, GHG_Category="LUEMCH4")
LUEMCH4_TOT <- mutate(LUEMCH4_TOT, unit="MtCO2eq")

LUEMN2O_TOT = data.table(Scenario$LUEMN2O)[year >= StartYear]
LUEMN2O_TOT <- filter(LUEMN2O_TOT, source != 'Total')
LUEMN2O_TOT$value = LUEMN2O_TOT$value*NToN2O*GWP_N2O
LUEMN2O_TOT = mutate(LUEMN2O_TOT, main_sector=mapply(function(x) MainSector(x), source))
LUEMN2O_TOT$main_sector = factor(LUEMN2O_TOT$main_sector,levels=main_sector)
LUEMN2O_TOT = select(LUEMN2O_TOT, year, region, main_sector, value) %>%
  group_by(year, region, main_sector) %>%
  summarise(value=sum(value, na.rm=TRUE))
tmp <- select(LUEMN2O_TOT, year, region, main_sector, value) %>%
  group_by(year, region) %>%
  summarise(value=sum(value, na.rm=TRUE))
tmp <- mutate(tmp, main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
LUEMN2O_TOT <- rbind(LUEMN2O_TOT, tmp)
LUEMN2O_TOT = select(LUEMN2O_TOT, year, region, main_sector, value)
LUEMN2O_TOT <- mutate(LUEMN2O_TOT, GHG_Category="LUEMN2O")
LUEMN2O_TOT <- mutate(LUEMN2O_TOT, unit="MtCO2eq")

## 3b. Sector aggregates - before summing to total GHG  incl./excl. LULUCF ###
#3b1. Agriculture emissions: CH4 and N2O
EMISCH4_AGRI <- filter(LUEMCH4_TOT, main_sector=='Agriculture')
tmp <- EMISCH4_AGRI %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISCH4_AGRI <- rbind(EMISCH4_AGRI, tmp)
EMISCH4_AGRI <- mutate(EMISCH4_AGRI, GHG_Category="AGRLUEMCH4")

EMISN2O_AGRI <- filter(LUEMN2O_TOT, main_sector=='Agriculture')
tmp <- EMISN2O_AGRI %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISN2O_AGRI <- rbind(EMISN2O_AGRI, tmp)
EMISN2O_AGRI <- mutate(EMISN2O_AGRI, GHG_Category="AGRLUEMN2O")

#Sum CH4 and N2O for agriculture
EMISCO2EQ_AGRI <- bind_rows(EMISCH4_AGRI, EMISN2O_AGRI)
EMISCO2EQ_AGRI <- select(EMISCO2EQ_AGRI, year, region, main_sector, value)
EMISCO2EQ_AGRI <- EMISCO2EQ_AGRI %>% filter(main_sector=='Agriculture') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_AGRI <- ungroup(EMISCO2EQ_AGRI)
EMISCO2EQ_AGRI <- select(EMISCO2EQ_AGRI, year, region, main_sector, value)
EMISCO2EQ_AGRI_indicator<-EMISCO2EQ_AGRI
EMISCO2EQ_AGRI_indicator$unit<-"MtCO2eq"
EMISCO2EQ_AGRI_indicator <- select(EMISCO2EQ_AGRI_indicator, year, region, value,unit)
tmp <- EMISCO2EQ_AGRI %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISCO2EQ_AGRI <- rbind(EMISCO2EQ_AGRI, tmp)
EMISCO2EQ_AGRI <- mutate(EMISCO2EQ_AGRI, GHG_Category="AGREMISCO2EQ")

#3b2. LULUCF emissions: CO2, CH4 and N2O
EMISCO2_LU <- LUEMCO2_TOT # all AFOLU CO2 emissions are LULUCF
EMISCO2_LU <- mutate(EMISCO2_LU, GHG_Category="LULUCFLUEMCO2")

EMISCH4_LU <- filter(LUEMCH4_TOT, main_sector=='LULUCF')
tmp <- EMISCH4_LU %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISCH4_LU <- rbind(EMISCH4_LU, tmp)
EMISCH4_LU <- mutate(EMISCH4_LU, GHG_Category="LULUCFLUEMCH4")

EMISN2O_LU <- filter(LUEMN2O_TOT, main_sector=='LULUCF')
tmp <- EMISN2O_LU %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISN2O_LU <- rbind(EMISN2O_LU, tmp)
EMISN2O_LU <- mutate(EMISN2O_LU, GHG_Category="LULUCFLUEMN2O")

#Sum CH4 and N2O for LULUCF
EMISCO2EQ_LU <- bind_rows(EMISCO2_LU, EMISCH4_LU, EMISN2O_LU)
EMISCO2EQ_LU <- select(EMISCO2EQ_LU, year, region, main_sector, value)
EMISCO2EQ_LU <- EMISCO2EQ_LU %>% filter(main_sector=='LULUCF') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_LU <- ungroup(EMISCO2EQ_LU)
EMISCO2EQ_LU <- select(EMISCO2EQ_LU, year, region, main_sector, value)
EMISCO2EQ_LU_indicator <- EMISCO2EQ_LU
tmp <- EMISCO2EQ_LU %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISCO2EQ_LU <- rbind(EMISCO2EQ_LU, tmp) 
EMISCO2EQ_LU <- mutate(EMISCO2EQ_LU, GHG_Category="LULUCFEMISCO2EQ")
EMISCO2EQ_LU$unit <- "MtCO2eq"
EMISCO2EQ_LU_indicator$unit<-"MtCO2eq"
  
#3b3. Waste emissions:CH4 and N2O
EMISCH4_WAS <- filter(LUEMCH4_TOT, main_sector=='Waste')
tmp <- EMISCH4_WAS %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISCH4_WAS <- rbind(EMISCH4_WAS, tmp)
EMISCH4_WAS <- mutate(EMISCH4_WAS, GHG_Category="AGRLUEMCH4")

EMISN2O_WAS <- filter(LUEMN2O_TOT, main_sector=='Waste')
tmp <- EMISN2O_WAS %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISN2O_WAS <- rbind(EMISN2O_WAS, tmp)
EMISN2O_WAS <- mutate(EMISN2O_WAS, GHG_Category="AGRLUEMN2O")

#Sum CH4 and N2O for Waste
EMISCO2EQ_WAS <- bind_rows(EMISCH4_WAS, EMISN2O_WAS)
EMISCO2EQ_WAS <- select(EMISCO2EQ_WAS, year, region, main_sector, value)
EMISCO2EQ_WAS <- EMISCO2EQ_WAS %>% filter(main_sector=='Waste') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_WAS <- ungroup(EMISCO2EQ_WAS)
EMISCO2EQ_WAS <- select(EMISCO2EQ_WAS, year, region, main_sector, value)
tmp <- EMISCO2EQ_WAS %>% mutate(main_sector="Total")
tmp$main_sector = factor(tmp$main_sector,levels=main_sector)
EMISCO2EQ_WAS <- rbind(EMISCO2EQ_WAS, tmp)
EMISCO2EQ_WAS <- mutate(EMISCO2EQ_WAS, GHG_Category="WASTEAGREMISCO2EQ")

# 4.
# Add all total aggregated emissions to on table EMISCO2EQ, and calculate total GHG emissions incl/excl LULUCF CO2
EMISCO2EQ  <- bind_rows(ENEMISCO2_TOT,ENEMISCH4_TOT,ENEMISN2O_TOT,
                        INDEMISCO2_TOT, INDEMISCH4_TOT, INDEMISN2O_TOT,
                        HFC_TOT,PFC_TOT,
                        LUEMCO2_TOT,LUEMCH4_TOT,LUEMN2O_TOT)
EMISCO2EQ$main_sector <- factor(EMISCO2EQ$main_sector, levels=main_sector)

EMISCO2EQexcl  <- bind_rows(ENEMISCO2_TOT,ENEMISCH4_TOT,ENEMISN2O_TOT,
                            INDEMISCO2_TOT, INDEMISCH4_TOT, INDEMISN2O_TOT,
                            HFC_TOT,PFC_TOT,
                            EMISCO2EQ_AGRI,EMISCO2EQ_WAS)

# for each year/region, main_sector sum all values to determine total GHG emissions, and add to table
EMISCO2EQ_tmp <- EMISCO2EQ %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_tmp <- ungroup(EMISCO2EQ_tmp)
EMISCO2EQ_tmp <- mutate(EMISCO2EQ_tmp, GHG_Category="EMISCO2EQ")
EMISCO2EQ <- bind_rows(EMISCO2EQ, EMISCO2EQ_tmp)
EMISCO2EQ$GHG_Category <- factor(EMISCO2EQ$GHG_Category)
EMISCO2EQ$unit<-"MtCO2eq"
EMISCO2EQ_indicator<-filter(EMISCO2EQ,main_sector=="Total")
EMISCO2EQ_indicator<-filter(EMISCO2EQ_indicator,GHG_Category=="EMISCO2EQ")
EMISCO2EQ_indicator<-select(EMISCO2EQ_indicator, year, region, main_sector, value,unit) 

# change GHG_Category and main_sector to factor
#EMISCO2EQ$main_sector <- factor(EMISCO2EQ$main_sector, levels=main_sector)
#EMISCO2EQ$GHG_Category <- factor(EMISCO2EQ$GHG_Category)

# Same for CO2eq excl.
EMISCO2EQexcl$main_sector <- factor(EMISCO2EQexcl$main_sector, levels=main_sector)
EMISCO2EQexcl <- select(EMISCO2EQexcl, year, region, main_sector, value)
EMISCO2EQexcl_tmp <- EMISCO2EQexcl %>% filter(main_sector=='Total') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQexcl_tmp <- ungroup(EMISCO2EQexcl_tmp)
#EMISCO2EQexcl_tmp <- mutate(EMISCO2EQexcl_tmp, GHG_Category="EMISCO2EQ")
EMISCO2EQexcl_tmp <- mutate(EMISCO2EQexcl_tmp, main_sector='Total')
EMISCO2EQexcl_tmp$main_sector <- factor(EMISCO2EQexcl_tmp$main_sector, levels=main_sector)
EMISCO2EQexcl <- bind_rows(EMISCO2EQexcl, EMISCO2EQexcl_tmp)
EMISCO2EQexcl$main_sector <- factor(EMISCO2EQexcl$main_sector, levels=main_sector)
#EMISCO2EQexcl$GHG_Category <- factor(EMISCO2EQexcl$GHG_Category)
EMISCO2EQexcl <- mutate(EMISCO2EQexcl, unit="MtCO2eq")

# calculate total demand sector emissions (does not include f-gases)
EMIS <- bind_rows(ENEMISCO2_TOT, ENEMISCH4_TOT) %>% bind_rows(ENEMISN2O_TOT) %>% 
bind_rows(INDEMISCO2_TOT) %>% bind_rows(INDEMISCH4_TOT) %>% bind_rows(INDEMISN2O_TOT) %>%
      bind_rows(HFC_TOT) %>% bind_rows(PFC_TOT)
EMIS_total <- subset(EMIS, main_sector=="Total")
EMIS_total <- EMIS_total %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_demand <- subset(EMIS, main_sector %in% c('Industry', 'Transport', 'Buildings'))
EMIS_demand <- EMIS_demand %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_demand <- mutate(EMIS_demand, unit="MtCO2eq")
EMIS_supply <- subset(EMIS, main_sector=="Energy supply")
EMIS_supply <- EMIS_supply %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_supply <- mutate(EMIS_supply, unit="MtCO2eq")
# Calculate individual and total GHG sector emissions
EMIS_buildings <- subset(EMIS,main_sector%in%c("Buildings"))
EMIS_buildings <- EMIS_buildings %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_buildings <- mutate(EMIS_buildings, unit="MtCO2eq")
EMIS_transport <- subset(EMIS,main_sector%in%c("Transport"))
EMIS_transport <- EMIS_transport %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_transport <- mutate(EMIS_transport, unit="MtCO2eq")
EMIS_industry <- subset(EMIS,main_sector%in%c("Industry"))
EMIS_industry <- EMIS_industry %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_industry <- mutate(EMIS_industry, unit="MtCO2eq")

EMIS_AFOLU <- bind_rows(EMISCO2EQ_AGRI, EMISCO2EQ_LU)
EMIS_AFOLU <- EMIS_AFOLU %>% group_by(year, region) %>% summarise(value=sum(value))
EMIS_AFOLU <- mutate(EMIS_AFOLU, unit="MtCO2eq")

# Calculate total CO2 emissions
EMISCO2  <- bind_rows(ENEMISCO2_TOT,INDEMISCO2_TOT,LUEMCO2_TOT)
EMISCO2$main_sector <- factor(EMISCO2$main_sector, levels=main_sector)
EMISCO2 <- select(EMISCO2, year, region, main_sector, value)
EMISCO2 <- EMISCO2 %>% filter(main_sector=='Total') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
#EMISCO2_tmp <- mutate(EMISCO2_tmp, GHG_Category="EMISCO2")
#EMISCO2_tmp <- mutate(EMISCO2_tmp, main_sector='Total')
#EMISCO2_tmp$main_sector <- factor(EMISCO2_tmp$main_sector, levels=main_sector)
#EMISCO2 <- EMISCO2_tmp
#EMISCO2 <- bind_rows(EMISCO2, EMISCO2_tmp)
EMISCO2$main_sector <- factor(EMISCO2$main_sector, levels=main_sector)

EMISCO2_indicator <- select(EMISCO2, year, region, value)
EMISCO2_indicator <- mutate(EMISCO2_indicator, unit="MtCO2eq")

#calculate emissions per capita
Scenario$POP$value <- 10^6*Scenario$POP$value
EMISCO2EQpc <- left_join(EMISCO2EQ, Scenario$POP, by=c("region", "year"))
EMISCO2EQpc <- mutate(EMISCO2EQpc, value=value.x/value.y)
EMISCO2EQpc <- select(EMISCO2EQpc, year, region, main_sector, value)
# next steps
# 1. Also make EMISCO2EQ_exclLULUCF - done, ok?
# 2. Can we do this with piping?
# temp2 <- EMISCO2EQ %>% group_by('year', 'region') %>% (function(x) sum(x$value))

# Emissions ETS-sector (industry+electricity)
ENEMISCO2_power = subset(Scenario$ENEMISCO2, sector == "Power generation" & energy_carrier=="Total" & year >= StartYear)
ENEMISCO2_power$value = ENEMISCO2_power$value*10^3*CToCO2
ENEMISCO2_power <- mutate(ENEMISCO2_power, unit="MtCO2eq")
ENEMISCO2_power = select(ENEMISCO2_power, year, region, value, unit)

ENEMISCH4_power = subset(Scenario$ENEMISCH4, sector == "Power generation" & energy_carrier=="Total" & year >= StartYear)
ENEMISCH4_power$value = ENEMISCH4_power$value*GWP_CH4
ENEMISCH4_power <- mutate(ENEMISCH4_power, unit="MtCO2eq")
ENEMISCH4_power = select(ENEMISCH4_power, year, region, value, unit)

ENEMISN2O_power = subset(Scenario$ENEMISN2O, sector == "Power generation" & energy_carrier=="Total" & year >= StartYear)
ENEMISN2O_power$value = ENEMISN2O_power$value*NToN2O*GWP_N2O
ENEMISN2O_power <- mutate(ENEMISN2O_power, unit="MtCO2eq")
ENEMISN2O_power = select(ENEMISN2O_power, year, region, value, unit)

EMIS_power = rbind(ENEMISCO2_power,ENEMISCH4_power,ENEMISN2O_power) %>% group_by(year, region,unit) %>% summarise(value=sum(value))

EMIS_ETS <- bind_rows(EMIS_industry, EMIS_power)
EMIS_ETS <- EMIS_ETS %>% group_by(year, region,unit) %>% summarise(value=sum(value))

#F-Gas index with reduction relative to 2010
# TO DO
HFC_TOT_tmp <- HFC_TOT %>% filter(main_sector=="Total") %>% select(year, region, value)
PFC_TOT_tmp <- PFC_TOT %>% filter(main_sector=="Total") %>% select(year, region, value)
FGases = inner_join(HFC_TOT_tmp, PFC_TOT_tmp, by = c('year', 'region'))
FGases = mutate(FGases, value=value.x+value.y)
FGases = select(FGases, year, region, value)
FGases_indicator<-FGases
FGases_indicator$unit<-"MtCO2eq/yr"
FGases_2010 = filter(FGases, year==2010)
FGases_2010 = select(FGases_2010, year, region, value)
#FGas_Reduction_index <- NULL
FGas_Reduction_index = inner_join(FGases_2010, FGases, by=c('region'))
FGas_Reduction_index = mutate(FGas_Reduction_index, value=value.y/value.x)
setnames(FGas_Reduction_index,"year.y","year")
FGas_Reduction_index <- ungroup(FGas_Reduction_index)
FGas_Reduction_index = select(FGas_Reduction_index, year, region, value)


# Electricity -------------------------------------------------------------
cat(sprintf("Energy supply sector: \n"))

#5.
# Calcualte share of renewable electricity production
Renewable <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_ren, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProd,Renewable)
tmp1 <- subset(tmp1, Renewable==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProd, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenElecShare <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
RenElecShare <- data.frame(RenElecShare)
RenElecShare <- mutate(RenElecShare, unit="%")

# TODO: check, ren share is very different from above ren share
Renewable_28 <- ifelse(Scenario$ElecProdSpec$energy_carrier %in% energy_carrier_ren_28, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProdSpec,Renewable_28)
tmp1 <- subset(tmp1, Renewable_28==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProdSpec, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenElecShare_28 <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
RenElecShare_28 <- data.frame(RenElecShare_28)
RenElecShare_28 <- mutate(RenElecShare_28, unit="%")

Renewable_excl_hydro <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_ren_excl_hydro, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProd,Renewable_excl_hydro)
tmp1 <- subset(tmp1, Renewable_excl_hydro==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProd, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenElecShare_excl_hydro <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
RenElecShare_excl_hydro <- data.frame(RenElecShare_excl_hydro)
RenElecShare_excl_hydro <- mutate(RenElecShare_excl_hydro, unit="%")

NonFossil <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_nf, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProd,NonFossil)
tmp1 <- subset(tmp1, NonFossil==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProd, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
NonFossilElecShare <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
NonFossilElecShare <- data.frame(NonFossilElecShare)
NonFossilElecShare <- mutate(NonFossilElecShare, unit="%")

NonFossil_28 <- ifelse(Scenario$ElecProdSpec$energy_carrier %in% energy_carrier_nf_28, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProdSpec,NonFossil_28)
tmp1 <- subset(tmp1, NonFossil_28==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProdSpec, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
NonFossilElecShare_28 <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
NonFossilElecShare_28 <- data.frame(NonFossilElecShare_28)
NonFossilElecShare_28 <- mutate(NonFossilElecShare_28, unit="%")

Innovative_28 <- ifelse(Scenario$ElecProdSpec$energy_carrier %in% energy_carrier_innovative_28, TRUE, FALSE)
tmp1 <- cbind(Scenario$ElecProdSpec,Innovative_28)
tmp1 <- subset(tmp1, Innovative_28==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(Scenario$ElecProdSpec, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
InnovativeElecShare_28 <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
InnovativeElecShare_28 <- data.frame(InnovativeElecShare_28)
InnovativeElecShare_28 <- mutate(InnovativeElecShare_28, unit="%")

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

# CO2 standard new power plants
CO2EPGNew=data.table(Scenario$CO2EPG_new)
CO2EPGCoalNew=CO2EPGNew[energy_technology=="Conv. Coal"]%>%select(year, region,value,unit)
# Now only for coal, change to all FF power plants --> weighting with new production?
# TODO: weighting factor must be applied (based on what? no electricity production for new power plants exist, possibly
#       new_capacity * load_factor)
CO2EPGNew=CO2EPGNew%>% group_by(year, region,unit) %>% summarise(value=mean(value))

# Efficiency new power plants(%) aggretated for all fossil fueled plants
ElecEffNewPct=filter(Scenario$ElecEffPct_new, region %in% regions26, energy_technology %in% energy_technology_plant)
# efficiency for conventional coal fired power plants
ElecEffCoalNewPct=filter(ElecEffNewPct, energy_technology=="Conv. Coal")%>%select(year, region,value,unit)
# weighting with new capacity (TO DO: improve with secondary energy/TPES)
ElecCap_new_tmp <- filter(Scenario$ElecCap_new,  region %in% regions26, energy_technology %in% energy_technology_plant)
ElecCap_new_sum <- filter(Scenario$ElecCap_new,  region %in% regions26, energy_technology %in% energy_technology_plant) %>%
                   group_by(year, region) %>%
                   summarise(total_elec_cap_new = sum(value))
ElecEffNewPct_tmp <- inner_join(ElecCap_new_tmp, ElecEffNewPct, by=c('year', 'region', 'energy_technology'))
ElecEffNewPct_tmp <- inner_join(ElecEffNewPct_tmp, ElecCap_new_sum, by=c('year', 'region')) %>%
                     mutate(value_tmp=ifelse(total_elec_cap_new==0,0,(value.x*value.y)/total_elec_cap_new))
ElecEffNewPct <- group_by(ElecEffNewPct_tmp, year, region) %>%
                 summarise(value=sum(value_tmp)) %>%
                 mutate(unit="%")

# Efficiency power plants(%)
# TO DO: change to 'ElecProdSpec2/TotFuelUseSpec' in TUSS/epg (see Fuel efficiency power plants.xlsx in PBL CLIMA 2019 update)
ElecEffPct=filter(Scenario$ElecEffPct, region %in% regions26, energy_technology %in% energy_technology_plant)
# efficiency for conventional coal fired power plants
#ElecEffCoalPct=ElecEffPct[energy_technology=="Conv. Coal"]%>%select(year, region,value,unit)
ElecEffCoalPct=filter(ElecEffPct, energy_technology=="Conv. Coal")%>%select(year, region,value,unit)
# weighting factor is  applied (based on installed capacity, could be improved by calculating electricity production/consumption)
ElecCap_tmp <- filter(Scenario$ElecCap,  region %in% regions26, energy_technology %in% energy_technology_plant)
ElecCap_sum <- filter(Scenario$ElecCap,  region %in% regions26, energy_technology %in% energy_technology_plant) %>%
                   group_by(year, region) %>%
                   summarise(total_elec_cap = sum(value))
ElecEffPct_tmp <- inner_join(ElecCap_tmp, ElecEffPct, by=c('year', 'region', 'energy_technology'))
ElecEffPct_tmp <- inner_join(ElecEffPct_tmp, ElecCap_sum, by=c('year', 'region')) %>%
                  mutate(value_tmp=ifelse(total_elec_cap==0,0,(value.x*value.y)/total_elec_cap))
ElecEffPct <- group_by(ElecEffPct_tmp, year, region) %>%
                 summarise(value=sum(value_tmp)) %>%
                 mutate(unit="%")

# CO2 emissions per Kwh electricity generated
CO2_elec <- filter(Scenario$ENEMISCO2, sector=="Power generation", energy_carrier=="Total")
#CO2_elec$value <- (Giga/Kilo)*10^9*CToCO2*CO2_elec$value
# GtC to gCO2
CO2_elec$value <- 10^15*CToCO2*CO2_elec$value 
CO2_elec <- select(CO2_elec, year, region, value)
Elec_Kwh <- filter(Scenario$ElecProd, energy_carrier=="Total")
# PJ to KWh
#Elec_Kwh$value <- (Peta/Tera)*(1/GWhToTJ)*(Giga/Kilo)*Elec_Kwh$value
Elec_Kwh$value <- (2.777778*10^8)*Elec_Kwh$value
Elec_Kwh <- select(Elec_Kwh, year, region, value)
CO2_KWh <- inner_join(CO2_elec, Elec_Kwh, by=c("year", "region"))
CO2_KWh <- mutate(CO2_KWh, value=value.x/value.y)
CO2_KWh <- mutate(CO2_KWh, unit="gCO2/Kwh")

ElecProd_coal <- filter(Scenario$ElecProdSpec, energy_carrier=="Conv. Coal") %>% select(-energy_carrier)

# Hydrogen
RenHydrogenShare <- NULL
InnovativeHydrogenShare <- NULL
if (Policy==TRUE)  {
  try({
  HydrogenUse_directREN <- filter(Scenario$H2Prod, energy_carrier %in% energy_carrier_hydrogen2_ren) %>% 
                           group_by(year, region) %>%
                           summarise(value=sum(value)) %>%
                           select(year, region, value)
  HydrogenUse_elec <- filter(Scenario$H2Prod, energy_carrier=="Electrolysis") %>% 
                      select(year, region, value)
  HydrogenUse_total <- filter(Scenario$H2Prod, energy_carrier=="Total") %>% 
                       select(year, region, value)
  RenHydrogenShare <- left_join(HydrogenUse_directREN, HydrogenUse_elec, by=c('year', 'region')) %>%
                      left_join(RenElecShare_28, by=c('year', 'region')) %>%
                      left_join(HydrogenUse_total, by=c('year', 'region'))
  # value.x=HydrogenUse_directREN, value.y=HydrogenUse_elec, value.x.x=RenElecShare_28, value.y.y=HydrogenUse_total
  RenHydrogenShare <- mutate(RenHydrogenShare, value=ifelse(value.y.y==0, 0, (value.x+(value.x.x/100)*value.y)/value.y.y))
  RenHydrogenShare <- select(RenHydrogenShare, year, region, value) %>% mutate(unit="%")
  RenHydrogenShare$value <- 100*RenHydrogenShare$value


  HydrogenUse_directInnovative <- filter(Scenario$H2Prod, energy_carrier %in% energy_carrier_hydrogen2_innovative) %>% 
                                  group_by(year, region) %>%
                                  summarise(value=sum(value)) %>%
                                  select(year, region, value)
  HydrogenUse2_elec <- filter(Scenario$H2Prod, energy_carrier=="Electrolysis") %>% 
                       select(year, region, value)
  HydrogenUse2_total <- filter(Scenario$H2Prod, energy_carrier=="Total") %>% 
                        select(year, region, value)
  InnovativeHydrogenShare <- left_join(HydrogenUse_directInnovative, HydrogenUse_elec, by=c('year', 'region')) %>%
                             left_join(InnovativeElecShare_28, by=c('year', 'region')) %>%
                             left_join(HydrogenUse2_total, by=c('year', 'region'))
  # value.x=HydrogenUse_directREN, value.y=HydrogenUse_elec, value.x.x=RenElecShare_28, value.y.y=HydrogenUse_total
  InnovativeHydrogenShare <- mutate(InnovativeHydrogenShare, value=ifelse(value.y.y==0, 0, (value.x+(value.x.x/100)*value.y)/value.y.y))
  InnovativeHydrogenShare <- select(InnovativeHydrogenShare, year, region, value) %>% mutate(unit="%")
  InnovativeHydrogenShare$value <- 100*InnovativeHydrogenShare$value
  }) #try
}
else {RenHydrogenShare <- NULL
      InnovativeHydrogenShare <- NULL
}


# Intensity ---------------------------------------------------------------
cat(sprintf("Different intensities: \n"))

# CO2 intensity of GDP
CO2_intensity <- merge(filter(EMISCO2,main_sector=="Total"), Scenario$GDP_MER,by=c('year','region')) 
CO2_intensity <- mutate(CO2_intensity, value=value.x/value.y, unit.int=paste("MtCO2/",unit,sep=""))
#CO2_intesnity <- filter(CO2_intensity, main_sector=="Total")
CO2_intensity <- select(CO2_intensity, year,region,value,unit.int)
setnames(CO2_intensity,"unit.int","unit")
CO2_intensity_2015 = filter(CO2_intensity, year==2015)
CO2_intensity_2015 = select(CO2_intensity_2015, region, value)
CO2_intensity_index = inner_join(CO2_intensity_2015, CO2_intensity, by=c('region'))
CO2_intensity_index = mutate(CO2_intensity_index, value=value.y/value.x,unit="index 2015")
CO2_intensity_index = select(CO2_intensity_index, year, region, value,unit)

# Energy intensity of GDP
TPES_total = data.table(Scenario$TPES)[energy_carrier == "Total" & year >= StartYear]
TPES_intensity <- merge(TPES_total,Scenario$GDP_MER,by=c('year','region'))%>% mutate(value=1000*value.x/value.y, unit="MJ/US$(2005)")%>%select(year,region, value,unit)
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
OilGas_CH4_indicator<-OilGas_CH4 %>% group_by(year, region,unit) %>% summarise(value=sum(value))
OilGas_GHG_total_indicator<-OilGas_GHG_total
OilGas_GHG_total_indicator$unit<-"MtCO2eq"

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
cat(sprintf("Total energy use: \n"))

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

#including nuclear + Chinese accounting method
# 1. Chines accounting
TPES_CHN_accounting <- Scenario$TPES
TPES_CHN_accounting <- inner_join(TPES_CHN_accounting, ElecEffCoalPct, by=c("year", "region"))
TPES_CHN_accounting$value.y <- ifelse(!(TPES_CHN_accounting$energy_carrier %in% c('Solar/wind', 'Hydro-electricity','Nuclear')),1, TPES_CHN_accounting$value.y )
TPES_CHN_accounting <- mutate(TPES_CHN_accounting, value=value.x/value.y)  
TPES_CHN_accounting <- select(TPES_CHN_accounting, year, region, energy_carrier, value, unit.x)
TPES_CHN_accounting <- rename(TPES_CHN_accounting, unit=unit.x)
# 2. determine renewable TPES share
RenewableN <- ifelse(TPES_CHN_accounting$energy_carrier %in% c('Modern biofuels','Solar/wind', 'Hydro-electricity','Nuclear'), TRUE, FALSE)
tmp1 <- cbind(TPES_CHN_accounting,RenewableN)
tmp1 <- subset(tmp1, RenewableN==TRUE)
tmp1 <- tmp1 %>% group_by(year, region) %>% summarise(value=sum(value))
tmp2 <- subset(TPES_CHN_accounting, energy_carrier=="Total")
tmp3 <- inner_join(tmp1, tmp2, by=c("year", "region"))
RenNucTPESShare_CHN_acccounting <- tmp3 %>% group_by(year, region) %>% summarise(value=100*value.x/value.y)
RenNucTPESShare_CHN_acccounting <- data.frame(RenNucTPESShare_CHN_acccounting)
RenNucTPESShare_CHN_acccounting <- mutate(RenNucTPESShare_CHN_acccounting, unit="%")

# Gas share in TPES
NatGasTPES <- data.table(Scenario$TPES)[energy_carrier %in%c("Total","Natural gas") & year >= StartYear]
NatGasTPESshare <- spread(NatGasTPES,energy_carrier,value) %>% mutate(Gasshare=`Natural gas`/Total*100)
NatGasTPESshare <- data.table(gather(NatGasTPESshare,energy_carrier,value,c(`Natural gas`:Gasshare)))
NatGasTPESshare <- NatGasTPESshare[energy_carrier=="Gasshare"] %>% mutate(unit="%")

# REN share in final energy - check REN in transport, same method here? TODO
# Elecprod total#
RENelec <- ifelse(Scenario$ElecProd$energy_carrier %in% energy_carrier_ren, TRUE, FALSE)
RENelecprod <- cbind(Scenario$ElecProd,RENelec)
RENelecprod <- subset(RENelecprod, RENelec==TRUE)
RENelecprod <- RENelecprod %>% group_by(year, region) %>% summarise(value=sum(value))

# RSE total#
RENrse <- ifelse(Scenario$FinalEnergy$energy_carrier %in% energy_carrier_demand_ren, TRUE, FALSE)
RENrsetot <- cbind(Scenario$FinalEnergy,RENrse)
RENrsetot <- subset(RENrsetot, RENrse==TRUE)
RENrsetot <- RENrsetot %>% group_by(year, region) %>% summarise(value=sum(value))

# Total of elecprod & RSE - share#
RENfinalenergy <- inner_join(RENelecprod, RENrsetot, by=c("year", "region")) %>% group_by(year, region) %>% summarise(value=value.x+value.y)
RSEtot <- subset(Scenario$FinalEnergy, energy_carrier=="Total")
RSEtot <- subset(RSEtot, sector=="Total")
RENfinalenergyshare <- inner_join(RENfinalenergy, RSEtot, by=c("year", "region"))%>% group_by(year, region) %>% summarise(value=100*(value.x/value.y))
RENfinalenergyshare <- data.frame(RENfinalenergyshare)
RENfinalenergyshare <- mutate(RENfinalenergyshare, unit="%")

# coal consumption
Coal_consumption <- filter(Scenario$TPES, energy_carrier=="Coal") 
Coal_consumption <- data.frame(Coal_consumption)

# Energy consumption industry and power supply
FinalEnergy_industry <- filter(Scenario$FinalEnergy, sector=="Industry", energy_carrier=="Total")
FinalEnergy_industry <- select(FinalEnergy_industry, year, region, value, unit)
FinalEnergy_electricity <- filter(Scenario$FinalEnergy, energy_carrier=="Electricity", sector=="Total")
FinalEnergy_electricity <- select(FinalEnergy_electricity, year, region, value, unit)
PowerSupply_consumption <- filter(Scenario$ElecFuelUseTot, energy_technology=="Total")
PowerSupply_consumption$value <- (Giga/Peta)*PowerSupply_consumption$value
PowerSupply_consumption$unit <- "PJ"
PowerSupply_consumption <- select(PowerSupply_consumption, year, region, value, unit)
PowerSupply_consumption <- inner_join(PowerSupply_consumption, FinalEnergy_electricity, by=c('year', 'region'))
PowerSupply_consumption <- mutate(PowerSupply_consumption, value=value.x-value.y)
PowerSupply_consumption <- select(PowerSupply_consumption, year, region, value, unit.x)
PowerSupply_consumption <- rename(PowerSupply_consumption, unit=unit.x)
EnergyConsumption_industry_powersupply <- rbind(FinalEnergy_industry, PowerSupply_consumption)
EnergyConsumption_industry_powersupply <- EnergyConsumption_industry_powersupply %>% group_by(year, region,unit) %>% summarise(value=sum(value))

# Trade oil, gas
TradeOil <- filter(Scenario$NetTrade, energy_carrier=="Oil")
TradeOil <- select(TradeOil, year, region, value, unit)
TradeGas <- filter(Scenario$NetTrade, energy_carrier=="Natural gas")
TradeGas <- select(TradeGas, year, region, value, unit)

# Total final energy
FinalEnergy_total <- filter(Scenario$FinalEnergy, sector=="Total", energy_carrier=="Total")
FinalEnergy_total <- select(FinalEnergy_total, year, region, value, unit)

# gas flaring oil production
GasFlaringCO2 <- filter(Scenario$ENEMISCO2, sector=='losses/leakages', energy_carrier=="Heavy oil")
GasFlaringCO2 <- select(GasFlaringCO2, year, region, value, unit)
GasFlaringCO2$value <- 10^3*CToCO2*GasFlaringCO2$value 
GasFlaringCO2$unit <- "MtCO2eq"

# Buildings ---------------------------------------------------------------
cat(sprintf("Buildings sector: \n"))

# Final Energy  per capita residential sector (GJ/capita)
Residential_FinalEnergy_capita <- filter(Scenario$FinalEnergy, sector=="Residential", energy_carrier=="Total")
Residential_FinalEnergy_capita <- select(Residential_FinalEnergy_capita, year, region, value)
Residential_Efficiency_capita <- inner_join(Residential_FinalEnergy_capita, Scenario$POP, by=c('year', 'region'))
Residential_Efficiency_capita <- mutate(Residential_Efficiency_capita, value=value.x / value.y)
Residential_Efficiency_capita <- select(Residential_Efficiency_capita, year, region, value)
Residential_Efficiency_capita$value <- Residential_Efficiency_capita$value*10^6
Residential_Efficiency_capita <- data.frame(Residential_Efficiency_capita)
Residential_Efficiency_capita$unit<-"GJ/capita"

# Final Energy  per m2 residential sector (GJ/m2)
Residential_FinalEnergy_m2 <- filter(Scenario$FinalEnergy, sector=="Residential", energy_carrier=="Total")
Residential_FinalEnergy_m2$value <- Residential_FinalEnergy_m2$value*10^6 # from PJ to GJ
Residential_FinalEnergy_m2 <- select(Residential_FinalEnergy_m2, year, region, value)
FloorSpace_total <- Scenario$FloorSpace
FloorSpace_total$value <- FloorSpace_total$value*10^6 # population is in millions
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
Appliances_FinalEnergy_capita$unit<-"GJ/capita"

# total energy use in residential sector
FinalEnergy_Residential_total <- filter(Scenario$FinalEnergy_Residential, population_group=="Total", enduse_function=="HouseholdAppliances")
FinalEnergy_Residential_total <- select(FinalEnergy_Residential_total, year, region, value, unit)

# appliances energy use in residential sector
FinalEnergy_Residential_appliances <- filter(Scenario$FinalEnergy_Residential, population_group=="Total", enduse_function=="Total")
FinalEnergy_Residential_appliances <- select(FinalEnergy_Residential_total, year, region, value, unit)

# Total carbon capture (kgC)
CarbonCaptured_total <- filter(Scenario$CarbonCaptured, energy_carrier=='Total')
CarbonCaptured_total <- CarbonCaptured_total %>% group_by(year, region, unit) %>% summarise(value=sum(value))
CarbonCaptured_total <- select(CarbonCaptured_total, year, region, value, unit)

# renewable share in residential buildigns
elec_share_residential_buildings <- rbind(mutate(RenElecShare, population_group=population_groups[1])) %>% #total
    rbind(mutate(RenElecShare, population_group=population_groups[2])) %>% #urban
    rbind(mutate(RenElecShare, population_group=population_groups[3])) %>% #rural
    rbind(mutate(RenElecShare, population_group=population_groups[4])) %>% #U_1
    rbind(mutate(RenElecShare, population_group=population_groups[5])) %>% #U_2
    rbind(mutate(RenElecShare, population_group=population_groups[6])) %>% #U_3
    rbind(mutate(RenElecShare, population_group=population_groups[7])) %>% #U_4
    rbind(mutate(RenElecShare, population_group=population_groups[8])) %>% #U_5
    rbind(mutate(RenElecShare, population_group=population_groups[9])) %>% #R_1
    rbind(mutate(RenElecShare, population_group=population_groups[10])) %>% #R_2
    rbind(mutate(RenElecShare, population_group=population_groups[11])) %>% #R_3
    rbind(mutate(RenElecShare, population_group=population_groups[12])) %>% #R_4
    rbind(mutate(RenElecShare, population_group=population_groups[13])) #R_5
FuelUseResBuildings <- Scenario$FinalEnergy_Residential_energy_carrier
FuelUseResBuildings_bio <- filter(FuelUseResBuildings, energy_carrier == "Modern biofuels")
FuelUseResBuildings_bio <- select(FuelUseResBuildings_bio, year, region, population_group, value)
FuelUseResBuildings_elec <- filter(FuelUseResBuildings, energy_carrier == "Electricity")
FuelUseResBuildings_elec <- select(FuelUseResBuildings_elec, year, region,population_group, value)
FuelUseResBuildings_total <- filter(FuelUseResBuildings, energy_carrier == "Total")
FuelUseResBuildings_total <- select(FuelUseResBuildings_total, year, region, population_group, value)
RenResBuildingsShare <- inner_join(elec_share_residential_buildings, FuelUseResBuildings_elec, by=c('year', 'region', 'population_group')) %>%
    inner_join(FuelUseResBuildings_bio, by=c('year', 'region', 'population_group')) %>%
    inner_join(FuelUseResBuildings_total, by=c('year', 'region', 'population_group'))
# x=%-REN electricity, y=electricity fuel use, x.x = bio fuel use, y.y = total fuel use
RenResBuildingsShare <- RenResBuildingsShare %>% mutate(value=(0.01*value.x*value.y+value.x.x)/value.y.y) %>% select(year, region, value, population_group)
RenResBuildingsShare$value <- 100*RenResBuildingsShare$value
RenResBuildingsShare <- mutate(RenResBuildingsShare, unit= "%")

# non-fossil residential share
NonFossil_share_residential_buildings <- rbind(mutate(NonFossilElecShare, population_group=population_groups[1])) %>% #total
  rbind(mutate(NonFossilElecShare, population_group=population_groups[2])) %>% #urban
  rbind(mutate(NonFossilElecShare, population_group=population_groups[3])) %>% #rural
  rbind(mutate(NonFossilElecShare, population_group=population_groups[4])) %>% #U_1
  rbind(mutate(NonFossilElecShare, population_group=population_groups[5])) %>% #U_2
  rbind(mutate(NonFossilElecShare, population_group=population_groups[6])) %>% #U_3
  rbind(mutate(NonFossilElecShare, population_group=population_groups[7])) %>% #U_4
  rbind(mutate(NonFossilElecShare, population_group=population_groups[8])) %>% #U_5
  rbind(mutate(NonFossilElecShare, population_group=population_groups[9])) %>% #R_1
  rbind(mutate(NonFossilElecShare, population_group=population_groups[10])) %>% #R_2
  rbind(mutate(NonFossilElecShare, population_group=population_groups[11])) %>% #R_3
  rbind(mutate(NonFossilElecShare, population_group=population_groups[12])) %>% #R_4
  rbind(mutate(NonFossilElecShare, population_group=population_groups[13])) #R_5
NonFossilResBuildingsShare <- inner_join(NonFossil_share_residential_buildings, FuelUseResBuildings_elec, by=c('year', 'region', 'population_group')) %>%
  inner_join(FuelUseResBuildings_bio, by=c('year', 'region', 'population_group')) %>%
  inner_join(FuelUseResBuildings_total, by=c('year', 'region', 'population_group'))
# x=%-REN electricity, y=electricity fuel use, x.x = bio fuel use, y.y = total fuel use
NonFossilResBuildingsShare <- NonFossilResBuildingsShare %>% mutate(value=(0.01*value.x*value.y+value.x.x)/value.y.y) %>% select(year, region, value, population_group)
NonFossilResBuildingsShare$value <- 100*NonFossilResBuildingsShare$value
NonFossilResBuildingsShare <- mutate(NonFossilResBuildingsShare, unit= "%")

# Transport ---------------------------------------------------------------
cat(sprintf("Transport sector: \n"))

# Car final energy use per kilometer
FuelUse_cars <- filter(Scenario$FinalEnergy_trvl_Transport, travel_mode=="Car")
FuelUse_cars <- select(FuelUse_cars, year, region, value)
PKm_cars <- filter(Scenario$PersonKilometers, travel_mode=="Car")
Pkm_cars <- select(PKm_cars, year, region, value)
FuelUse_pkm_cars <- inner_join(FuelUse_cars, Pkm_cars, by=c('year', 'region'))
FuelUse_pkm_cars <- mutate(FuelUse_pkm_cars, value=value.x / value.y)
FuelUse_pkm_cars <- select(FuelUse_pkm_cars, year, region, value)
# Convert to km/l
FuelUse_pkm_cars$value <- (MJ_l_gasoline/FuelUse_pkm_cars$value)/Load_car
FuelUse_pkm_cars <- data.frame(FuelUse_pkm_cars)
FuelUse_pkm_cars$unit<-"km/L"

# Car CO2 per km
CO2_cars <- filter(Scenario$TransportCO2Emissions, travel_mode=='Car') %>% select(year, region, value) %>% mutate(v="CO2")
CO2_km_cars <- filter(Scenario$TransportTravelCO2Emissions, travel_mode=='Car') %>% select(year, region, value) %>% mutate(v="CO2")
Pkm_cars <- filter(Scenario$PersonKilometers, travel_mode=='Car') %>% select(year, region, value) %>% mutate(v='pkm')
CO2_km_cars <- rbind(CO2_cars, Pkm_cars)
CO2_km_cars <- spread(CO2_km_cars, key=v, value=value)
CO2_cars <- select(CO2_cars,-v) %>% mutate(unit="Mt CO2")
# convert to gCO2/km (from Mt/Tkm) and in vehicle kilomters, instead of pkm
CO2_km_cars <- mutate(CO2_km_cars, value=(Load_car/Tera)*10^12*CO2/pkm) %>% select(year, region, value)
CO2_km_cars <- mutate(CO2_km_cars, unit= "gCO2/km")

# CO2 emissions medium trucks
CO2_MedT <- filter(Scenario$TransportCO2Emissions, travel_mode=='Medium truck') %>% select(year, region, value) %>% mutate(unit="Mt CO2")

# CO2 emissions heavy trucks
CO2_HvyT <- filter(Scenario$TransportCO2Emissions, travel_mode=='Heavy truck') %>% select(year, region, value) %>% mutate(unit="Mt CO2")


# Share of Electric cars
ElectricCars_share <- filter(Scenario$VehicleShare_cars, car_type=="BEV" | car_type=="BEV 100km")
ElectricCars_share <- ElectricCars_share %>% group_by(year, region) %>% summarise(value=sum(value))
ElectricCars_share <- select(ElectricCars_share, year, region, value)
ElectricCars_share <- ungroup(ElectricCars_share)
ElectricCars_share <- mutate(ElectricCars_share, unit= "%")

# renewable share in transport
RenTransportShare = data.frame(matrix(ncol=0,nrow=0))
RenTransportShare_trvl = data.frame(matrix(ncol=0,nrow=0))
RenTransportShare_frgt = data.frame(matrix(ncol=0,nrow=0))
RenTransportShare_cars = data.frame(matrix(ncol=0,nrow=0))
NonFossilTransportShare = data.frame(matrix(ncol=0,nrow=0))
if (Policy==TRUE) {
try({
elec_share_transport_travel <- rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[1])) %>% #walking
                          rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[2])) %>% #biking
                          rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[3])) %>% #Bus
                          rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[4])) %>% #Train
                          rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[5])) %>% #Car
                          rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[6])) %>% #High speed train
                          rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[7])) %>% #Air
                          rbind(mutate(RenElecShare, travel_mode=travel_mode_travel[8]))     #Total travel
elec_share_transport_travel <- mutate(elec_share_transport_travel, type="Travel")
elec_share_transport_freight <- rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[1])) %>% #inland shipping
                           rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[2])) %>% #freight train
                           rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[3])) %>% #medium truck
                           rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[4])) %>% #heavy truck
                           rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[5])) %>% #air cargo
                           rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[6])) %>% #international shipping
                           rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[7])) %>% #-
                           rbind(mutate(RenElecShare, travel_mode=travel_mode_freight[8])) #Total freight
elec_share_transport_freight <- mutate(elec_share_transport_freight, type="Freight")
elec_share_transport_total <- rbind(mutate(RenElecShare, travel_mode="Total"))
elec_share_transport_total <- mutate(elec_share_transport_total, type="Total")
elec_share_transport <- rbind(elec_share_transport_travel, elec_share_transport_freight) %>% rbind(elec_share_transport_total)
elec_share_transport <- select(elec_share_transport, year, region, travel_mode, type, value)
elec_share_transport$travel_mode = factor(elec_share_transport$travel_mode, levels=travel_mode)
FuelUse_transport_trvl_tmp <- mutate(Scenario$FuelUseFleet_trvl, type="Travel")
FuelUse_transport_frgt_tmp <- mutate(Scenario$FuelUseFleet_frgt, type= "Freight")
FuelUse_transport <- rbind(FuelUse_transport_trvl_tmp, FuelUse_transport_frgt_tmp)
FuelUse_transport <- as.data.frame(FuelUse_transport)
FuelUse_transport_total <- filter(FuelUse_transport, travel_mode=="Total") %>% group_by(year, region, travel_mode, energy_carrier, unit) %>% summarize(value=sum(value))
FuelUse_transport_total <- mutate(FuelUse_transport_total, type="Total") %>% select(year, region, travel_mode, energy_carrier, value, unit, type)
FuelUse_transport_total <- as.data.frame(FuelUse_transport_total)
FuelUse_transport <- rbind(FuelUse_transport, FuelUse_transport_total)
fuel_transport_bio <- filter(FuelUse_transport, energy_carrier == "Modern biofuel")
fuel_transport_bio <- select(fuel_transport_bio, year, region, travel_mode, type, value)
fuel_transport_elec <- filter(FuelUse_transport, energy_carrier == "Electricity")
fuel_transport_elec <- select(fuel_transport_elec, year, region, travel_mode, type, value)
fuel_transport_total <- filter(FuelUse_transport, energy_carrier == "Total")
fuel_transport_total <- select(fuel_transport_total, year, region, travel_mode, type, value)
fuel_transport_elec$travel_mode <- factor(fuel_transport_elec$travel_mode, levels=travel_mode)
fuel_transport_bio$travel_mode <- factor(fuel_transport_bio$travel_mode, levels=travel_mode)
fuel_transport_total$travel_mode <- factor(fuel_transport_total$travel_mode, levels=travel_mode)
RenTransportShare <- inner_join(elec_share_transport, fuel_transport_elec, by=c('year', 'region', 'travel_mode', 'type')) %>%
                       inner_join(fuel_transport_bio, by=c('year', 'region', 'travel_mode','type')) %>%
                       inner_join(fuel_transport_total, by=c('year', 'region', 'travel_mode', 'type'))
# x=%-REN electricity, y=electricity fuel use, x.x = bio fuel use, y.y = total fuel use
RenTransportShare <- RenTransportShare %>% mutate(value=(0.01*value.x*value.y+value.x.x)/value.y.y) %>% select(year, region, value, travel_mode, type)
RenTransportShare$value <- 100*RenTransportShare$value
RenTransportShare <- mutate(RenTransportShare, unit= "%")
RenTransportShare_trvl <- filter(RenTransportShare, type=="Travel") %>% select(-type)
RenTransportShare_frgt <- filter(RenTransportShare, type=="Freight") %>% select(-type)
RenTransportShare_cars <- filter(RenTransportShare, travel_mode=="Car") %>% select(year, region, value, unit)

# Non-fossil share
NonFossil_share_transport_travel <- rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[1])) %>% #walking
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[2])) %>% #biking
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[3])) %>% #Bus
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[4])) %>% #Train
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[5])) %>% #Car
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[6])) %>% #High speed train
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[7])) %>% #Air
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_travel[8]))     #Total travel
NonFossil_share_transport_travel <- mutate(NonFossil_share_transport_travel, type="Travel")
NonFossil_share_transport_freight <- rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[1])) %>% #inland shipping
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[2])) %>% #freight train
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[3])) %>% #medium truck
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[4])) %>% #heavy truck
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[5])) %>% #air cargo
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[6])) %>% #international shipping
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[7])) %>% #-
  rbind(mutate(NonFossilElecShare, travel_mode=travel_mode_freight[8])) #Total freight
NonFossil_share_transport_travel$travel_mode <- factor(NonFossil_share_transport_travel$travel_mode, levels=travel_mode)
NonFossil_share_transport_freight$travel_mode <- factor(NonFossil_share_transport_freight$travel_mode, levels=travel_mode)
NonFossil_share_transport_freight <- mutate(NonFossil_share_transport_freight, type="Freight")
NonFossil_share_transport_total <- rbind(mutate(NonFossilElecShare, travel_mode="Total"))
NonFossil_share_transport_total$travel_mode <- factor(NonFossil_share_transport_total$travel_mode, levels=travel_mode)
NonFossil_share_transport_total <- mutate(NonFossil_share_transport_total, type="Total")
NonFossil_share_transport <- rbind(NonFossil_share_transport_travel, NonFossil_share_transport_freight) %>% rbind(NonFossil_share_transport_total)
NonFossil_share_transport <- select(NonFossil_share_transport, year, region, travel_mode, type, value)
fuel_transport_elec$travel_mode <- factor(fuel_transport_elec$travel_mode, levels=travel_mode)
fuel_transport_bio$travel_mode <- factor(fuel_transport_bio$travel_mode, levels=travel_mode)
fuel_transport_total$travel_mode <- factor(fuel_transport_total$travel_mode, levels=travel_mode)
NonFossilTransportShare <- inner_join(NonFossil_share_transport, fuel_transport_elec, by=c('year', 'region', 'travel_mode', 'type')) %>%
  inner_join(fuel_transport_bio, by=c('year', 'region', 'travel_mode','type')) %>%
  inner_join(fuel_transport_total, by=c('year', 'region', 'travel_mode', 'type'))
# x=%-NonFossil electricity, y=electricity fuel use, x.x = bio fuel use, y.y = total fuel use
NonFossilTransportShare <- NonFossilTransportShare %>% mutate(value=(0.01*value.x*value.y+value.x.x)/value.y.y) %>% select(year, region, value, travel_mode, type)
NonFossilTransportShare$value <- 100*NonFossilTransportShare$value
NonFossilTransportShare <- mutate(NonFossilTransportShare, unit= "%")
}) # try
} # if
else {RenTransportShare = data.frame(matrix(ncol=0,nrow=0))
      RenTransportShare_trvl = data.frame(matrix(ncol=0,nrow=0))
      RenTransportShare_frgt = data.frame(matrix(ncol=0,nrow=0))
      RenTransportShare_cars = data.frame(matrix(ncol=0,nrow=0))
      NonFossilTransportShare = data.frame(matrix(ncol=0,nrow=0))
}

# Blending share for cars
BlendingShareBio_cars_energy = data.frame(matrix(ncol=0,nrow=0))
if (Policy==TRUE) {
try({
  BlendingShareBio_cars_energy <- filter(Scenario$BlendingShareBio_energy, travel_mode=="Car") %>% select(year, region, value, unit)
}) # try
} # if
else {
  BlendingShareBio_cars_energy = data.frame(matrix(ncol=0,nrow=0))
  }
  
# Reduction cars relative to 2021 (EU transport target)
# 1. relative to 2025
CO2_cars_2021_2025 = filter(CO2_cars, year==2021)
CO2_cars_2021_2025 = select(CO2_cars_2021_2025, year, region, value)
CO2_cars_2021_2025_Reduction_index = inner_join(CO2_cars_2021_2025, CO2_cars, by=c('region'))
CO2_cars_2021_2025_Reduction_index = mutate(CO2_cars_2021_2025_Reduction_index, value=value.y/value.x)
setnames(CO2_cars_2021_2025_Reduction_index,"year.y","year")
CO2_cars_2021_2025_Reduction_index <- ungroup(CO2_cars_2021_2025_Reduction_index)
CO2_cars_2021_2025_Reduction_index = select(CO2_cars_2021_2025_Reduction_index, year, region, value)
CO2_cars_2021_2025_Reduction_index <- mutate(CO2_cars_2021_2025_Reduction_index, unit="%")

# 2. relative to 2030
CO2_cars_2021_2030 = filter(CO2_cars, year==2021)
CO2_cars_2021_2030 = select(CO2_cars_2021_2030, year, region, value)
CO2_cars_2021_2030_Reduction_index = inner_join(CO2_cars_2021_2030, CO2_cars, by=c('region'))
CO2_cars_2021_2030_Reduction_index = mutate(CO2_cars_2021_2030_Reduction_index, value=value.y/value.x)
setnames(CO2_cars_2021_2030_Reduction_index,"year.y","year")
CO2_cars_2021_2030_Reduction_index <- ungroup(CO2_cars_2021_2030_Reduction_index)
CO2_cars_2021_2030_Reduction_index = select(CO2_cars_2021_2030_Reduction_index, year, region, value)
CO2_cars_2021_2030_Reduction_index <- mutate(CO2_cars_2021_2030_Reduction_index, unit="%")
=======

# total travel transport co2 emissions including indirect electricity
CO2_i_tmp <- filter(Scenario$CO2EPG, energy_technology=="Total") %>%
  select(-unit)
Elec_trvl_transport_tmp <- filter(Scenario$FinalEnergy_carrier_trvl_Transport, energy_carrier=="Electricity") %>%
  select(-unit)
TransportTravelCO2Emissions_Elec = inner_join(CO2_i_tmp, Elec_trvl_transport_tmp, by=c('year', 'region')) %>%
                             mutate(CO2_elec=(1/10^-6*GWhToTJ)*value.x*value.y/10^12, unit="MtCO2") %>%
                             select(-value.y, -value.y, -energy_carrier) %>%
                             rename(value=CO2_elec)
TransportTravelCO2Emissions_inclElec = inner_join(TransportTravelCO2Emissions, TransportTravelCO2Emissions_inclElec, by=c('year', 'region', 'travel_mode'))
>>>>>>> aad6cebe637e2bf7847791d9f93b23ab90daa8a1

# Industry ----------------------------------------------------------------
cat(sprintf("Industry sector: \n"))

# Energy intensity of industry sector (Kwh/US$2005)
Industry_FinalEnergy <- filter(Scenario$FinalEnergy, sector=="Industry", energy_carrier=="Total")
Industry_FinalEnergy <- select(Industry_FinalEnergy, year, region, value)
Industry_Efficiency <- inner_join(Industry_FinalEnergy, Scenario$IVA, by=c('year', 'region'))
Industry_Efficiency <- mutate(Industry_Efficiency, value=(10^3*(1/GWhToTJ)*10^6*value.x) / (10^6*value.y))
Industry_Efficiency <- select(Industry_Efficiency, year, region, value)
Industry_Efficiency <- data.frame(Industry_Efficiency)
Industry_Efficiency$unit<-"Kwh/US$2005"

# INDUSTRY

# final energy per IVA (PJ/million US$(2005)
Industry_Energy <- filter(Scenario$FinalEnergy, sector=="Industry", energy_carrier=="Total") %>% select(year, region, value) %>% mutate(v="energy")
Industry_IVA <- select(Scenario$IVA, year, region, value) %>% mutate(v="IVA")
Industry_Energy_IVA <- rbind(Industry_Energy, Industry_IVA)
Industry_Energy_IVA <- spread(Industry_Energy_IVA, key=v, value=value)
Industry_Energy_IVA <- mutate(Industry_Energy_IVA, value=energy/IVA) %>% select(year, region, value)
Industry_Energy_IVA <- mutate(Industry_Energy_IVA, unit="PJ/million US$(2005)")

# CO2 emissions per IVA (MtCO2/million US$(2005)
Industry_Emis <- filter(INDEMISCO2_TOT, main_sector=="Total") %>% select(year, region, value) %>% mutate(v="emis")
Industry_Emis_IVA <- rbind(data.table(Industry_Emis), data.table(Industry_IVA))
Industry_Emis_IVA <- spread(Industry_Emis_IVA, key=v, value=value)
Industry_Emis_IVA <- mutate(Industry_Emis_IVA, value=emis/IVA) %>% select(year, region, value)
Industry_Emis_IVA <- mutate(Industry_Emis_IVA, unit="MtCO2/million US$(2005)")

# IMAGE
cat(sprintf("AFOLU sector: \n"))

# Reforestation (in 100 km2)
Forest_area_total <- filter(Scenario$ForestArea, forest_type=="Total")

# Compile list ------------------------------------------------------------
cat(sprintf("Process list: \n"))
l <- list(EMISCO2EQexcl=EMISCO2EQexcl,EMISCO2EQpc=EMISCO2EQpc, EMISCO2=EMISCO2, EMISCO2EQ=EMISCO2EQ,EMIS_ETS=EMIS_ETS,
          EMIS_demand=EMIS_demand,EMIS_buildings=EMIS_buildings,EMIS_supply=EMIS_supply,EMIS_industry=EMIS_industry,EMIS_transport=EMIS_transport,EMIS_power=EMIS_power,
          EMISCO2EQ_LU=EMISCO2EQ_LU,EMISCO2EQ_WAS=EMISCO2EQ_WAS,LUEMCO2_TOT=LUEMCO2_TOT,EMIS_AFOLU=EMIS_AFOLU,LUEMCH4_TOT=LUEMCH4_TOT, LUEMN2O_TOT=LUEMN2O_TOT,
          EMISCO2EQ_LU_indicator=EMISCO2EQ_LU_indicator,LUEMCO2_TOT_indicator=LUEMCO2_TOT_indicator,EMISCO2EQ_indicator=EMISCO2EQ_indicator,
          EMISCO2EQ_AGRI_indicator=EMISCO2EQ_AGRI_indicator,HFC_TOT_indicator=HFC_TOT_indicator,FGases_indicator=FGases_indicator,EMISCO2_indicator=EMISCO2_indicator,
          # energy supply
          TPES_total=TPES_total, TPES_CHN_accounting=TPES_CHN_accounting,
          RenElecShare=RenElecShare, RenElecShare_excl_hydro=RenElecShare_excl_hydro, NonFossilElecShare=NonFossilElecShare,
          RenElecShare_28=RenElecShare_28, NonFossilElecShare_28=NonFossilElecShare_28, InnovativeElecShare_28=InnovativeElecShare_28,
          RenHydrogenShare=RenHydrogenShare, InnovativeHydrogenShare=InnovativeHydrogenShare,
          RenTPESShare=RenTPESShare,RenNucTPESShare_CHN_acccounting=RenNucTPESShare_CHN_acccounting,RenNucTPESShare=RenNucTPESShare,
          RENfinalenergyshare=RENfinalenergyshare,
          NatGasTPESshare=NatGasTPESshare,Coal_consumption=Coal_consumption, EnergyConsumption_industry_powersupply=EnergyConsumption_industry_powersupply,
          ElecCapGeo=ElecCapGeo, ElecCapWindOff=ElecCapWindOff, ElecCapWindOn=ElecCapWindOn, ElecCapSolarPV=ElecCapSolarPV, ElecCapSolarCSP=ElecCapSolarCSP, ElecCapHydro=ElecCapHydro, 
          ElecCapWaste=ElecCapWaste, ElecCapNuclear=ElecCapNuclear, ElecCapCoalCCS=ElecCapCoalCCS, ElecCapCoalTrad=ElecCapCoalTrad,
          ElecCapCoalTot=ElecCapCoalTot, ElecCapBioTot=ElecCapBioTot, ElecCapSolarTot=ElecCapSolarTot, ElecCapWindTot=ElecCapWindTot, ElecCapRenTot=ElecCapRenTot, ElecCapWSTot=ElecCapWSTot, 
          CO2EPGNew=CO2EPGNew,CO2EPGCoalNew=CO2EPGCoalNew,ElecEffNewPct=ElecEffNewPct,ElecEffCoalNewPct=ElecEffCoalNewPct,ElecEffPct=ElecEffPct,ElecEffCoalPct=ElecEffCoalPct,
          CO2_KWh=CO2_KWh,
          ElecProd_coal=ElecProd_coal, 
          OilGas_Intensity = OilGas_Intensity,OilGas_CH4_indicator=OilGas_CH4_indicator, OilGas_GHG_total_indicator=OilGas_GHG_total_indicator,
          CO2_intensity=CO2_intensity,CO2_intensity_index=CO2_intensity_index,TPES_intensity=TPES_intensity,TPES_intensity_index=TPES_intensity_index,
          TradeOil=TradeOil, TradeGas=TradeGas, FinalEnergy_total=FinalEnergy_total,GasFlaringCO2=GasFlaringCO2,
          CarbonCaptured_total=CarbonCaptured_total,
          # industry
          Industry_Efficiency = Industry_Efficiency, FGas_Reduction_index = FGas_Reduction_index, Industry_Energy_IVA=Industry_Energy_IVA,Industry_Emis_IVA=Industry_Emis_IVA,
          # buildings
          Residential_Efficiency_capita=Residential_Efficiency_capita, Residential_FinalEnergy_m2=Residential_FinalEnergy_m2,Appliances_FinalEnergy_capita=Appliances_FinalEnergy_capita,
          FinalEnergy_Residential_total=FinalEnergy_Residential_total, FinalEnergy_Residential_appliances=FinalEnergy_Residential_appliances,
          RenResBuildingsShare=RenResBuildingsShare, NonFossilResBuildingsShare=NonFossilResBuildingsShare,
          # transport
          CO2_km_cars=CO2_km_cars, CO2_cars=CO2_cars, CO2_MedT=CO2_MedT, CO2_HvyT=CO2_HvyT,
          FuelUse_pkm_cars=FuelUse_pkm_cars, ElectricCars_share=ElectricCars_share, 
          RenTransportShare_trvl=RenTransportShare_trvl, RenTransportShare_frgt=RenTransportShare_frgt, RenTransportShare=RenTransportShare, RenTransportShare_cars=RenTransportShare_cars,
          NonFossilTransportShare=NonFossilTransportShare,
          BlendingShareBio_cars_energy=BlendingShareBio_cars_energy,
<<<<<<< HEAD
          CO2_cars_2021_2030_Reduction_index=CO2_cars_2021_2030_Reduction_index, CO2_cars_2021_2025_Reduction_index=CO2_cars_2021_2025_Reduction_index,
=======
          TransportCO2Emissions_Elec=TransportCO2Emissions_Elec,
>>>>>>> aad6cebe637e2bf7847791d9f93b23ab90daa8a1
          # SDG
          ElecAccTot=ElecAccTot,
          # AFOLU
          Forest_area_total=Forest_area_total)
}
