library(tidyverse)
NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy')
NoPolicyi <- ProcessTimerScenario(NoPolicy)

CAFETargets <- ImportTimerScenario('CAFE_Targets','NoPolicy')
CAFETargetsi <- ProcessTimerScenario(CAFETargets)

BiofuelTargets <- ImportTimerScenario('Biofuel_targets','NoPolicy')
BiofuelTargetsi <- ProcessTimerScenario(BiofuelTargets)

BuildingTargets <- ImportTimerScenario('Building_standards_targets','NoPolicy')
BuildingTargetsi <- ProcessTimerScenario(BuildingTargets)

CarbonTaxes <- ImportTimerScenario('Carbon_taxes','NoPolicy')
CarbonTaxesi <- ProcessTimerScenario(CarbonTaxes)

ElectricVehiclesTargets <- ImportTimerScenario('Electric_vehicle_targets','NoPolicy')
ElectricVehiclesTargetsi <- ProcessTimerScenario(ElectricVehiclesTargets)

FGasTargets <- ImportTimerScenario('F-gas_targets','NoPolicy')
FGasTargetsi <- ProcessTimerScenario(FGasTargets)

OilImportTargets <- ImportTimerScenario('Oil_import','NoPolicy')
OilImportTargetsi <- ProcessTimerScenario(OilImportTargets)

OilMethaneTargets <- ImportTimerScenario('Oil_methane','NoPolicy')
OilMethaneTargetsi <- ProcessTimerScenario(OilMethaneTargets)

PowerPlantStandards <- ImportTimerScenario('Power_plant_standards_targets','NoPolicy')
PowerPlantStandardsi <- ProcessTimerScenario(PowerPlantStandards)

RenTargets <- ImportTimerScenario('Ren_targets','NoPolicy')
RenTargetsi <- ProcessTimerScenario(RenTargets)

NPi <- ImportTimerScenario('NPi_update','NPi')
NPii <- ProcessTimerScenario(NPi)

INDCi <- ImportTimerScenario('INDCi','NPi')
INDCii <- ProcessTimerScenario(INDCi)

select_regions = c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World")

CalcReductions <- function(select_year, Scenario_i_1, Scenario_i_2, select_regions, select_main_sector, policy)
{ source('Settings.R')
  tmp1 <- Scenario_i_1$EMISCO2EQ %>% filter(year==select_year & region %in% select_regions, main_sector==select_main_sector) %>% select(region, main_sector, GHG_Category, value)
  tmp2 <- Scenario_i_2$EMISCO2EQ %>% filter(year==select_year & region %in% select_regions, main_sector==select_main_sector) %>% select(region, main_sector, GHG_Category, value)
  ReductionFromPolicies <- inner_join(tmp1, tmp2, by=c('region', 'main_sector', 'GHG_Category'))
  #ReductionFromPoliciesEx <- arrange(ReductionFromPolicies, region, main_sector, GHG_Category)
  ReductionFromPolicies <- mutate(ReductionFromPolicies, value = value.x-value.y)
  ReductionFromPolicies <- mutate(ReductionFromPolicies, policy=policy)
  ReductionFromPolicies <- select(ReductionFromPolicies, policy, region, main_sector, GHG_Category, value)
  ReductionFromPolicies <- arrange(ReductionFromPolicies, policy, region, main_sector, GHG_Category)
}

ReductionFromPolicies <- CalcReductions(2030, NoPolicyi, CAFETargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Cafe target")
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, BiofuelTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Biofuel target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, BuildingTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Building target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, CarbonTaxesi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Carbon Tax"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, ElectricVehiclesTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Electric vehicle share"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, FGasTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "F-gas target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, OilImportTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Oil import target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, OilMethaneTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Oil Methane target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, PowerPlantStandardsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Power plant standard"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, RenTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Renewable target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, NPii, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Current policies"))
