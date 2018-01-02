# This script
# 1. Creates lables for TIMER output files
# 2. Imports files from TIMER_folder
# 3. Aggregates GHG emissions files to total emissions in CO2eq (unit)
# 4. Creates ENEMISCO2EQ with totals per individual GHG and all GHG emisisons in CO2eq

setwd("C:/Users/mrroelfs/Documents/R/TIMER_output")

# settings
TIMER_folder = 'TIMER_2015'
IMAGE_folder = 'IMAGE'
TIMER_project = 'GPP_CurrentPolicies_update'
IMAGE_project = 'NPi'
StartYear = 1990

#TIMER_2015/<scenario>/tuss
# CO2Spec

#TIMER_2015/<scenario>/indicatoren
# ENEMISCO2
# ENEMISCH4
# ENEMISN2O
# INDEMISCO2
# INDEMISCH4
# INDEMISN2O

# IMAGE/<scenario>/output
# LUEMCO2
# LUEMCH4
# LUEMN2O

library(data.table)
library(tidyverse)
#library(tidyr)
#library(dplyr)
library(plyr)
source('mym2r.R')

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

# prepare correct labels for mym file dimensions
CO2Spec = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project, "/tuss", sep=""), TIMER_project, filename='CO2Spec.out', novarname = T)

ENEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""), filename='ENEMISCO2.out', novarname = T)
ENEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""), filename='ENEMISCH4.out', novarname = T)
ENEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""),filename='ENEMISN2O.out', novarname = T)

INDEMISCO2 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""), filename='INDEMISCO2.out', novarname = T)
INDEMISCH4 = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""), filename='INDEMISCH4.out', novarname = T)
INDEMISN2O = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""), filename='INDEMISN2O.out', novarname = T)

HFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""), filename='EMISHFC_reg.out', novarname = T)
PFC_reg = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_project,"/indicatoren", sep=""), filename='EMISPFC_reg.out', novarname = T)

LUEMCO2 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_project,"/output", sep=""), filename='LUEMCO2.out', novarname = T)
LUEMCH4 = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_project,"/output", sep=""), filename='LUEMCH4.out', novarname = T)
LUEMN2O = read.mym2r.nice(mym.folder=IMAGE_folder, scen.econ=paste(IMAGE_project,"/output", sep=""), filename='LUEMN2O.out', novarname = T)

#Aggregate emissions to CO2eq per TIMER file
ENEMISCO2_TOT = data.table(ENEMISCO2)[sector == "Total" & energy_carrier == "Total" & year >= StartYear]
ENEMISCO2_TOT$value = ENEMISCO2_TOT$value*10^3*CToCO2
ENEMISCO2_TOT[,sector:=NULL]
ENEMISCO2_TOT[,energy_carrier:=NULL]
ENEMISCO2_TOT <- mutate(ENEMISCO2_TOT, GHG_Category="ENEMISCO2")

ENEMISCH4_TOT = data.table(ENEMISCH4)[sector == "Total" & energy_carrier == "Total" & year >= StartYear]
ENEMISCH4_TOT$value = ENEMISCH4_TOT$value*GWP_CH4
ENEMISCH4_TOT[,sector:=NULL]
ENEMISCH4_TOT[,energy_carrier:=NULL]
ENEMISCH4_TOT <- mutate(ENEMISCH4_TOT, GHG_Category="ENEMISCH4")

ENEMISN2O_TOT = data.table(ENEMISN2O)[sector == "Total" & energy_carrier == "Total" & year >= StartYear]
ENEMISN2O_TOT$value = ENEMISN2O_TOT$value*NToN2O*GWP_N2O
ENEMISN2O_TOT[,sector:=NULL]
ENEMISN2O_TOT[,energy_carrier:=NULL]
ENEMISN2O_TOT <- mutate(ENEMISN2O_TOT, GHG_Category="ENEMISN2O")

INDEMISCO2_TOT = data.table(INDEMISCO2)[industrial_process == "total" & year >= StartYear]
INDEMISCO2_TOT$value = INDEMISCO2_TOT$value*10^3*CToCO2
INDEMISCO2_TOT[,industrial_process:=NULL]
INDEMISCO2_TOT <- mutate(INDEMISCO2_TOT, GHG_Category="INDEMISCO2")

INDEMISCH4_TOT = data.table(INDEMISCH4)[industrial_process == "total" & year >= StartYear]
INDEMISCH4_TOT$value = INDEMISCH4_TOT$value*GWP_CH4
INDEMISCH4_TOT[,industrial_process:=NULL]
INDEMISCH4_TOT <- mutate(INDEMISCH4_TOT, GHG_Category="INDEMISCH4")

INDEMISN2O_TOT = data.table(INDEMISN2O)[industrial_process == "total" & year >= StartYear]
INDEMISN2O_TOT$value = INDEMISN2O_TOT$value*NToN2O*GWP_N2O
INDEMISN2O_TOT[,industrial_process:=NULL]
INDEMISN2O_TOT <- mutate(INDEMISN2O_TOT, GHG_Category="INDEMISN2O")

HFC_temp = data.table(HFC_reg)[year >= StartYear]
HFC_temp[HFC_gas=="HFC23"]$value =   10^-3*HFC_temp[HFC_gas=="HFC23"]$value*GWP_HFC23 
HFC_temp[HFC_gas=="HFC32"]$value =   10^-3*HFC_temp[HFC_gas=="HFC32"]$value*GWP_HFC32 
HFC_temp[HFC_gas=="HFC4310"]$value = 10^-3*HFC_temp[HFC_gas=="HFC4310"]$value*GWP_HFC4310
HFC_temp[HFC_gas=="HFC125"]$value =  10^-3*HFC_temp[HFC_gas=="HFC125"]$value*GWP_HFC125
HFC_temp[HFC_gas=="HFC134a"]$value = 10^-3*HFC_temp[HFC_gas=="HFC134a"]$value*GWP_HFC134a
HFC_temp[HFC_gas=="HFC143a"]$value = 10^-3*HFC_temp[HFC_gas=="HFC143a"]$value*GWP_HFC143a
HFC_temp[HFC_gas=="HFC152"]$value =  10^-3*HFC_temp[HFC_gas=="HFC152"]$value*GWP_HFC152
HFC_temp[HFC_gas=="HFC227"]$value =  10^-3*HFC_temp[HFC_gas=="HFC227"]$value*GWP_HFC227
HFC_temp[HFC_gas=="HFC236"]$value =  10^-3*HFC_temp[HFC_gas=="HFC236"]$value*GWP_HFC236
HFC_temp[HFC_gas=="HFC245"]$value =  10^-3*HFC_temp[HFC_gas=="HFC245"]$value*GWP_HFC245
HFC_TOT <- ddply(HFC_temp, c('year', 'region'), function(x) sum(x$value))
colnames(HFC_TOT)[3]<-"value"
HFC_TOT <- mutate(HFC_TOT, GHG_Category="HFC")

PFC_temp = data.table(PFC_reg)[year >= StartYear]
PFC_temp[PFC_gas=="CF4"]$value =   10^-3*PFC_temp[PFC_gas=="CF4"]$value*GWP_CF4
PFC_temp[PFC_gas=="C2F6"]$value =  10^-3*PFC_temp[PFC_gas=="C2F6"]$value*GWP_C2F6
PFC_temp[PFC_gas=="C6F14"]$value = 10^-3*PFC_temp[PFC_gas=="C6F14"]$value*GWP_C6F14 
PFC_temp[PFC_gas=="SF6"]$value =   10^-3*PFC_temp[PFC_gas=="SF6"]$value*GWP_SF6 
PFC_TOT <- ddply(PFC_temp, c('year', 'region'), function(x) sum(x$value))
colnames(PFC_TOT)[3]<-"value"
PFC_TOT <- mutate(PFC_TOT, GHG_Category="PFC")

LUEMCO2_TOT = data.table(LUEMCO2)[source == "Total" & year >= StartYear]
LUEMCO2_TOT$value = LUEMCO2_TOT$value*10^3*CToCO2
LUEMCO2_TOT[,source:=NULL]
LUEMCO2_TOT <- mutate(LUEMCO2_TOT, GHG_Category="LUEMCO2")

LUEMCH4_TOT = data.table(LUEMCH4)[source == "Total" & year >= StartYear]
LUEMCH4_TOT$value = LUEMCH4_TOT$value*GWP_CH4
LUEMCH4_TOT[,source:=NULL]
LUEMCH4_TOT <- mutate(LUEMCH4_TOT, GHG_Category="LUEMCH4")

LUEMN2O_TOT = data.table(LUEMN2O)[source == "Total" & year >= StartYear]
LUEMN2O_TOT$value = LUEMN2O_TOT$value*NToN2O*GWP_N2O
LUEMN2O_TOT[,source:=NULL]
LUEMN2O_TOT <- mutate(LUEMN2O_TOT, GHG_Category="LUEMN2O")

# Add all total aggregated emissions to on table EMISCO2EQ, and calculate total GHG emissions incl/excl LULUCF CO2
rm(EMISCO2EQ)
EMISCO2EQ <- rbind(ENEMISCO2_TOT, ENEMISCH4_TOT )
EMISCO2EQ <- rbind(EMISCO2EQ, ENEMISN2O_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, INDEMISCO2_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, INDEMISCH4_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, INDEMISN2O_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, LUEMCO2_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, LUEMCH4_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, LUEMN2O_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, HFC_TOT)
EMISCO2EQ <- rbind(EMISCO2EQ, PFC_TOT)
# for each year/region, sum all values to determine total GHG emissions, and add to table
temp1 <- ddply(EMISCO2EQ, c('year', 'region'), function(x) sum(x$value))
colnames(temp1)[3]<-"value"
temp1$GHG_Category <- "EMISCO2EQ"
EMISCO2EQ <- rbind(EMISCO2EQ, temp1)

# next steps
# 1. Also make EMISCO2EQ_exclLULUCF
# 2. Can we do this with piping?
# temp2 <- EMISCO2EQ %>% group_by('year', 'region') %>% (function(x) sum(x$value))


rm(ENEMISCO2_TOT)
rm(ENEMISCH4_TOT)
rm(ENEMISN2O_TOT)
rm(INDEMISCO2_TOT)
rm(INDEMISCH4_TOT)
rm(INDEMISN2O_TOT)
rm(LUEMCO2_TOT)
rm(LUEMCH4_TOT)
rm(LUEMN2O_TOT)
rm(HFC_temp)
rm(HFC_TOT)
rm(PFC_temp)
rm(PFC_TOT)