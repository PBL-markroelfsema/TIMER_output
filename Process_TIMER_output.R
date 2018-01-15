#pre: Scenario is created with ImportTimerScenario and assigned to 'Scenario'
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
 
ProcessTimerScenario <- function(Scenario)
{ s <- deparse(substitute(Scenario)) # get object name as string
  if(!exists(s))
  { print(paste("Scenario ", s, " is not imported yet. First execute ImportTimerFile", sep=""))
    stop()
  }

source('Settings.R')
source('General Functions.R')

#3. 
# Aggregate emissions to CO2eq per TIMER file
ENEMISCO2_TOT = subset(Scenario$ENEMISCO2, energy_carrier == "Total" & year >= StartYear)
ENEMISCO2_TOT$region = factor(ENEMISCO2_TOT$region,labels=regions28_EU)
ENEMISCO2_TOT$value = ENEMISCO2_TOT$value*10^3*CToCO2
ENEMISCO2_TOT = mutate(ENEMISCO2_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISCO2_TOT$main_sector = factor(ENEMISCO2_TOT$main_sector,levels=main_sector)
ENEMISCO2_TOT <- mutate(ENEMISCO2_TOT, GHG_Category="ENEMISCO2")
#ENEMISCO2_TOT = select(ENEMISCO2_TOT, year, region, main_sector, GHG_Category, value)
ENEMISCO2_TOT = select(ENEMISCO2_TOT, year, region, main_sector, GHG_Category, value)

ENEMISCH4_TOT = data.table(Scenario$ENEMISCH4)[energy_carrier == "Total" & year >= StartYear]
ENEMISCH4_TOT$region = factor(ENEMISCH4_TOT$region,labels=regions28_EU)
ENEMISCH4_TOT$value = ENEMISCH4_TOT$value*GWP_CH4
ENEMISCH4_TOT = mutate(ENEMISCH4_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISCH4_TOT$main_sector = factor(ENEMISCH4_TOT$main_sector,levels=main_sector)
ENEMISCH4_TOT <- mutate(ENEMISCH4_TOT, GHG_Category="ENEMISCH4")
#ENEMISCH4_TOT = select(ENEMISCH4_TOT, year, region, main_sector, GHG_Category, value)
ENEMISCH4_TOT = select(ENEMISCH4_TOT, year, region, main_sector, GHG_Category, value)

ENEMISN2O_TOT = data.table(Scenario$ENEMISN2O)[energy_carrier == "Total" & year >= StartYear]
ENEMISN2O_TOT$region = factor(ENEMISN2O_TOT$region,labels=regions28_EU)
ENEMISN2O_TOT$value = ENEMISN2O_TOT$value*NToN2O*GWP_N2O
ENEMISN2O_TOT = mutate(ENEMISN2O_TOT, main_sector=mapply(function(x) MainSector(x), sector))
ENEMISN2O_TOT$main_sector = factor(ENEMISN2O_TOT$main_sector,levels=main_sector)
ENEMISN2O_TOT <- mutate(ENEMISN2O_TOT, GHG_Category="ENEMISN2O")
ENEMISN2O_TOT = select(ENEMISN2O_TOT, year, region, main_sector, GHG_Category, value)

INDEMISCO2_TOT = data.table(Scenario$INDEMISCO2)[industrial_process == "total" & year >= StartYear]
INDEMISCO2_TOT$region = factor(INDEMISCO2_TOT$region,labels=regions28_EU)
INDEMISCO2_TOT$value = INDEMISCO2_TOT$value*10^3*CToCO2
#INDEMISCO2_TOT <- mutate(INDEMISCO2_TOT, main_sector="Industry")
INDEMISCO2_TOT = mutate(INDEMISCO2_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISCO2_TOT$main_sector = factor(INDEMISCO2_TOT$main_sector,levels=main_sector)
INDEMISCO2_TOT <- mutate(INDEMISCO2_TOT, GHG_Category="INDEMISCO2")
INDEMISCO2_TOT = select(INDEMISCO2_TOT, year, region, main_sector, GHG_Category, value)

INDEMISCH4_TOT = data.table(Scenario$INDEMISCH4)[industrial_process == "total" & year >= StartYear]
INDEMISCH4_TOT$region = factor(INDEMISCH4_TOT$region,labels=regions28_EU)
INDEMISCH4_TOT$value = INDEMISCH4_TOT$value*GWP_CH4
#INDEMISCH4_TOT <- mutate(INDEMISCH4_TOT, main_sector="Industry")
INDEMISCH4_TOT = mutate(INDEMISCH4_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISCH4_TOT$main_sector = factor(INDEMISCH4_TOT$main_sector,levels=main_sector)
INDEMISCH4_TOT <- mutate(INDEMISCH4_TOT, GHG_Category="INDEMISCH4")
INDEMISCH4_TOT = select(INDEMISCH4_TOT, year, region, main_sector, GHG_Category, value)

INDEMISN2O_TOT = data.table(Scenario$INDEMISN2O)[industrial_process == "total" & year >= StartYear]
INDEMISN2O_TOT$region = factor(INDEMISN2O_TOT$region,labels=regions28_EU)
INDEMISN2O_TOT$value = INDEMISN2O_TOT$value*NToN2O*GWP_N2O
#INDEMISN2O_TOT <- mutate(INDEMISN2O_TOT, main_sector="Industry")
INDEMISN2O_TOT = mutate(INDEMISN2O_TOT, main_sector=mapply(function(x) MainSector(x), industrial_process))
INDEMISN2O_TOT$main_sector = factor(INDEMISN2O_TOT$main_sector,levels=main_sector)
INDEMISN2O_TOT <- mutate(INDEMISN2O_TOT, GHG_Category="INDEMISN2O")
INDEMISN2O_TOT = select(INDEMISN2O_TOT, year, region, main_sector, GHG_Category, value)

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
HFC_tmp <- HFC_TOT %>% group_by(year, region) %>% summarise(value=sum(value))
HFC_tmp <- ungroup(HFC_tmp)
HFC_tmp <- mutate(HFC_tmp, HFC_gas="Total")
HFC_TOT <- bind_rows(HFC_TOT, HFC_tmp)
HFC_TOT = mutate(HFC_TOT, main_sector=mapply(function(x) MainSector(x), HFC_gas))
HFC_TOT$main_sector = factor(HFC_TOT$main_sector,levels=main_sector)
HFC_TOT <- mutate(HFC_TOT, GHG_Category="HFC")
HFC_TOT = select(HFC_TOT, year, region, main_sector, GHG_Category, value)

PFC_TOT = data.table(Scenario$PFC_reg)[year >= StartYear]
PFC_TOT[PFC_gas=="CF4"]$value =   10^-3*PFC_TOT[PFC_gas=="CF4"]$value*GWP_CF4
PFC_TOT[PFC_gas=="C2F6"]$value =  10^-3*PFC_TOT[PFC_gas=="C2F6"]$value*GWP_C2F6
PFC_TOT[PFC_gas=="C6F14"]$value = 10^-3*PFC_TOT[PFC_gas=="C6F14"]$value*GWP_C6F14 
PFC_TOT[PFC_gas=="SF6"]$value =   10^-3*PFC_TOT[PFC_gas=="SF6"]$value*GWP_SF6 
PFC_tmp <- PFC_TOT %>% group_by(year, region) %>% summarise(value=sum(value))
PFC_tmp <- ungroup(PFC_tmp)
PFC_tmp <- mutate(PFC_tmp, PFC_gas="Total")
PFC_TOT <- bind_rows(PFC_TOT, PFC_tmp)
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
#EMISCO2EQ_tmp <- EMISCO2EQ %>% group_by(year, region) %>% summarise(value=sum(value))
#temp1 <- ddply(EMISCO2EQ, c('year', 'region'), function(x) sum(x$value))
#colnames(temp1)[3]<-"value"
#temp1$GHG_Category <- "EMISCO2EQ"
EMISCO2EQ_tmp <- EMISCO2EQ %>% filter(main_sector=='Total') %>% group_by(year, region, main_sector) %>% summarise(value=sum(value))
EMISCO2EQ_tmp <- ungroup(EMISCO2EQ_tmp)
EMISCO2EQ_tmp <- mutate(EMISCO2EQ_tmp, GHG_Category="EMISCO2EQ")
EMISCO2EQ_tmp <- mutate(EMISCO2EQ_tmp, main_sector='Total')
EMISCO2EQ_tmp$main_sector <- factor(EMISCO2EQ_tmp$main_sector, levels=main_sector)
EMISCO2EQ <- bind_rows(EMISCO2EQ, EMISCO2EQ_tmp)

# change GHG_Category and main_sector to factor
EMISCO2EQ$main_sector <- factor(EMISCO2EQ$main_sector, levels=main_sector)
EMISCO2EQ$GHG_Category <- factor(EMISCO2EQ$GHG_Category)

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

l <- list(EMISCO2EQ=EMISCO2EQ,RenElecShare=RenElecShare)
}