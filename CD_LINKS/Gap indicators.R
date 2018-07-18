library(gridExtra)
source('functions/Settings.R')
source('functions/General Functions.R')
source('functions/Import_TIMER_output.R')
source('functions/Process_TIMER_output.R')

#ProjectDir = "~/disks/y/ontwapps/Timer/Users/Mark"
#Project = 'CD_LINKSupdate'

ProjectDir = "~/disks/y/ontwapps/Timer/Users/Mathijs/Projects/CD-LINKS"
Project = "CD-LINKS"
R_dir = paste(ProjectDir, Project, "R-scripts/TIMER_output", sep="/")
setwd(R_dir)
getwd()

NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy')
NPi <- ImportTimerScenario('NPi','NPi')
INDCi <- ImportTimerScenario('INDCi','INDCi')
NPi2020_1000 <- ImportTimerScenario('NPi2020_1000','NPi2020_1000')
NPi2020_400<- ImportTimerScenario('NPi2020_400','NPi2020_400')

NoPolicy_ind <- ProcessTimerScenario(NoPolicy)
NPi_ind <- ProcessTimerScenario(NPi)
INDCi_ind <- ProcessTimerScenario(INDCi)
NPi2020_1000_ind <- ProcessTimerScenario(NPi2020_1000)
NPi2020_400_ind <- ProcessTimerScenario(NPi2020_400)

REN_electricity <- mutate(NoPolicy_ind$RenElecShare, scenario="No policy")
REN_electricity <- mutate(NPi_ind$RenElecShare, scenario="National policies") %>% rbind(REN_electricity)
REN_electricity <- mutate(INDCi_ind$RenElecShare, scenario="NDC") %>% rbind(REN_electricity)
REN_electricity <- mutate(NPi2020_1000_ind$RenElecShare, scenario="2C") %>% rbind(REN_electricity)
REN_electricity <- mutate(NPi2020_400_ind$RenElecShare, scenario="1.5C") %>% rbind(REN_electricity)

figure_REN_electricity <- ggplot(data=filter(REN_electricity, region=="World", year>=2010, year<=2050)) +
                          geom_line(aes(x=year, y=value, color=scenario)) +
                          labs(title="Electricity sector", subtitle="share of renewable electricity") +
                          labs(x = "year", y = "%") +
                          theme(panel.background = element_rect(fill = 'white', colour = 'black'))

CO2_intensity_cars <- mutate(NoPolicy_ind$CO2_km_cars, scenario="No policy")
CO2_intensity_cars <- mutate(NPi_ind$CO2_km_cars, scenario="National policies") %>% rbind(CO2_intensity_cars)
CO2_intensity_cars <- mutate(INDCi_ind$CO2_km_cars, scenario="NDC") %>% rbind(CO2_intensity_cars)
CO2_intensity_cars <- mutate(NPi2020_1000_ind$CO2_km_cars, scenario="2C") %>% rbind(CO2_intensity_cars)
CO2_intensity_cars <- mutate(NPi2020_400_ind$CO2_km_cars, scenario="1.5C") %>% rbind(CO2_intensity_cars)

figure_CO2_intensity_cars <- ggplot(data=filter(CO2_intensity_cars, region=="World", year>=2010, year<=2050)) +
  geom_line(aes(x=year, y=value, color=scenario)) +
  labs(title="Transport sector", subtitle="CO2-intensity cars") +
  labs(x = "year", y = "gCO2/km") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

Energy_intensity_residential_buildings <- mutate(NoPolicy_ind$Residential_FinalEnergy_m2, scenario="No policy")
Energy_intensity_residential_buildings <- mutate(NPi_ind$Residential_FinalEnergy_m2, scenario="National policies") %>% rbind(Energy_intensity_residential_buildings)
Energy_intensity_residential_buildings <- mutate(INDCi_ind$Residential_FinalEnergy_m2, scenario="NDC") %>% rbind(Energy_intensity_residential_buildings)
Energy_intensity_residential_buildings <- mutate(NPi2020_1000_ind$Residential_FinalEnergy_m2, scenario="2C") %>% rbind(Energy_intensity_residential_buildings)
Energy_intensity_residential_buildings <- mutate(NPi2020_400_ind$Residential_FinalEnergy_m2, scenario="1.5C") %>% rbind(Energy_intensity_residential_buildings)

figure_Energy_intensity_residential_buildings <- ggplot(data=filter(Energy_intensity_residential_buildings, region=="World", year>=2010, year<=2050)) +
  geom_line(aes(x=year, y=value, color=scenario)) +
  labs(title="Buildings sector", subtitle="residential energy use per m2") +
  labs(x = "year", y = "GJ/m2") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

Energy_intensity_industry <- mutate(NoPolicy_ind$Industry_Energy_IVA, scenario="No policy")
Energy_intensity_industry <- mutate(NPi_ind$Industry_Energy_IVA, scenario="National policies") %>% rbind(Energy_intensity_industry)
Energy_intensity_industry <- mutate(INDCi_ind$Industry_Energy_IVA, scenario="NDC") %>% rbind(Energy_intensity_industry)
Energy_intensity_industry <- mutate(NPi2020_1000_ind$Industry_Energy_IVA, scenario="2C") %>% rbind(Energy_intensity_industry)
Energy_intensity_industry <- mutate(NPi2020_400_ind$Industry_Energy_IVA, scenario="1.5C") %>% rbind(Energy_intensity_industry)

figure_Energy_intensity_industry <- ggplot(data=filter(Energy_intensity_industry, region=="World", year>=2010, year<=2050)) +
  geom_line(aes(x=year, y=value, color=scenario)) +
  labs(title="Industry sector", subtitle="energy use per industry value added") +
  labs(x = "year", y = "PJ/million US$(2005)") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

g1 <- grid.arrange(figure_REN_electricity, figure_CO2_intensity_cars, figure_Energy_intensity_residential_buildings, figure_Energy_intensity_industry, ncol=2, nrow=2)
g2 <- grid_arrange_shared_legend(figure_REN_electricity, figure_CO2_intensity_cars, figure_Energy_intensity_residential_buildings, figure_Energy_intensity_industry)
