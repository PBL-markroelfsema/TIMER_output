library(ggplot2)
library(gridExtra)
library(grid)

source('Import_TIMER_output.R')
source('Settings.R')
CurrentPolicies <- ImportTimerScenario('GPP_CurrentPolicies','SSP2')
GPPPolicies <- ImportTimerScenario('GPP_Total_fromCPS','SSP2')

source('Process_TIMER_output.R')
source('Settings.R')
CurrentPolicies_indicators <- ProcessTimerScenario(CurrentPolicies)
GPPPolicies_indicators <- ProcessTimerScenario(GPPPolicies)

# replace f-gas emissions for GPP policy scenario
GPPFGas_index <- read.table('data/FGases_index_GPP.csv', sep=";", header=TRUE, row.names=NULL)
tmp <- filter(CurrentPolicies_indicators$FGas_Reduction_index, region=="World", year>-1990, year<=2030)
tmp <- inner_join(tmp, GPPFGas_index, by=c('year', 'region'))
tmp <- mutate(tmp, value=pmin(value.x, value.y))
GPPFGas_index <- select(tmp, year, region, value)

# read in IIASA data for deforestation
CurrentPolicies_Deforestation_index <- read.table('data/DeforestationReduction_index_CPS.csv', sep=";", header=TRUE, row.names=NULL)
GPPPolicies_Deforestation_index <- read.table('data/DeforestationReduction_index_GPP.csv', sep=";", header=TRUE, row.names=NULL)

# a) renewable elctricity
a <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$RenElecShare, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$RenElecShare, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2) +
  labs(x = "year", y = "(a) Renewable\n electricity share") +
  scale_colour_discrete(name="Scenario",
                    #breaks=c("Current policies", "Good practice policies"),
                    labels=c("Current policies", "Good practice policies")) +
                    theme(axis.title = element_text(face="bold", size=22)) +
                    theme(legend.text=element_text(size=30)) +
                    theme(legend.title=element_text(size=30)) +
                    theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# b) oil and gas production
b <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(b) GHG intensity\n of oil and gas production") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(legend.text=element_text(size=30)) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# c) energy efficiency industry
c <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$IndustryEfficiency, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$IndustryEfficiency, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(c) Final energy use\n per US$(2005)") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(legend.text=element_text(size=30)) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# d) f-gases
d <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$FGas_Reduction_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPFGas_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2) +
  labs(x = "year", y = "(d) F-gas emissions\n relative to 2010") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(legend.text=element_text(size=30)) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# e) energy efficiency building envelope
e <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$Residential_Efficiency_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$Residential_Efficiency_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2) +
  labs(x = "year", y = "(e) Residential energy use\n per capita") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(legend.text=element_text(size=30)) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# f) appliances
f <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$Appliances_FinalEnergy_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$Appliances_FinalEnergy_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(f) Energy use appliances\n per capita") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(legend.text=element_text(size=30)) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# g) fuel efficiency cars
g <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$FuelUse_pkm_cars, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$FuelUse_pkm_cars, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(g) Average\n fuel efficiency cars") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(legend.text=element_text(size=30)) +
                      scale_x_discrete(labels = abbreviate) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# h) electric cars
h <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$ElectricCars_share, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$ElectricCars_share, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(h) Share of electric cars") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(legend.text=element_text(size=30)) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# i) deforestation
i <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_Deforestation_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_Deforestation_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(i) Deforestation emissions\n relative to 2010") +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
                      theme(axis.title = element_text(face="bold", size=22)) +
                      theme(legend.text=element_text(size=30)) +
                      theme(panel.background = element_rect(fill = 'white', colour = 'black'))



fig <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
ggsave(file=paste("graphs/Fig2.png",sep=""),fig,width=24,height=14,dpi=200)
