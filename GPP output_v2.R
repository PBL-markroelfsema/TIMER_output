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

source('grid_arrange_shared_legend.R')
rm(a)
y_axix_a = "(a) Renewable\n electricity share"
data_a1=subset(CurrentPolicies_indicators$RenElecShare, year>=2015 & year<=2030 & region=="World")
data_a2=subset(GPPPolicies_indicators$RenElecShare, year>=2015 & year<=2030 & region=="World")
a <- gpp_graph(data_a1, data_a2, y_axix_a)

rm(b)
y_axix_b = "(b) GHG intensity\n of oil and gas production"
data_b1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_b2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
b <- gpp_graph(data_b1, data_b2, y_axix_b)

rm(c)
y_axix_c = "(c) Final energy use\n per US$(2005)"
data_c1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_c2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
c <- gpp_graph(data_c1, data_c2, y_axix_c)

rm(d)
y_axix_d = "(d) F-gas emissions\n relative to 2010"
data_d1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_d2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
d <- gpp_graph(data_d1, data_d2, y_axix_d)

rm(e)
y_axix_e = "(e) Residential energy use\n per capita"
data_e1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_e2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
e <- gpp_graph(data_e1, data_e2, y_axix_e)

rm(f)
y_axix_f = "(f) Energy use appliances\n per capita"
data_f1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_f2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
f <- gpp_graph(data_f1, data_f2, y_axix_f)

rm(g)
y_axix_g = "(g) Average\n fuel efficiency cars"
data_g1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_g2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
g <- gpp_graph(data_g1, data_g2, y_axix_g)

rm(h)
y_axix_h = "(h) Share of electric cars"
data_h1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_h2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
h <- gpp_graph(data_h1, data_h2, y_axix_h)

rm(i)
y_axix_i = "(i) Deforestation emissions\n relative to 2010"
data_i1=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
data_i2=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World")
i <- gpp_graph(data_i1, data_i2, y_axix_i)

#fig_tst <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
fig_tst <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
fig_tst <- grid_arrange_shared_legend(a,b, ncol = 3, nrow = 3)
ggsave(file=paste("graphs/Fig_tst.png",sep=""),fig_tst,width=24,height=14,dpi=200)



# a) renewable elctricity
a <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$RenElecShare, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$RenElecShare, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(a) Renewable\n electricity share") +
  #scale_colour_manual(name="Scenario", values=c("Current policies" = "red", "Good practice policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      #breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text=element_text(size=22)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
# b) oil and gas production
b <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$OilGas_Intensity, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(b) GHG intensity\n of oil and gas production") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text=element_text(size=30)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  

# c) energy efficiency industry
c <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$IndustryEfficiency, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$IndustryEfficiency, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(c) Final energy use\n per US$(2005)") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text=element_text(size=30)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  

# d) f-gases
d <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$FGas_Reduction_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPFGas_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2) +
  labs(x = "year", y = "(d) F-gas emissions\n relative to 2010") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text=element_text(size=30)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  

# e) energy efficiency building envelope
e <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$Residential_Efficiency_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$Residential_Efficiency_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2) +
  labs(x = "year", y = "(e) Residential energy use\n per capita") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text=element_text(size=30)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  

# f) appliances
f <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$Appliances_FinalEnergy_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$Appliances_FinalEnergy_capita, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(f) Energy use appliances\n per capita") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text=element_text(size=30)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  
# g) fuel efficiency cars
g <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$FuelUse_pkm_cars, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$FuelUse_pkm_cars, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(g) Average\n fuel efficiency cars") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text=element_text(size=30)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  
# h) electric cars
h <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_indicators$ElectricCars_share, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_indicators$ElectricCars_share, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(h) Share of electric cars") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(legend.text=element_text(size=30)) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  

# i) deforestation
i <- ggplot() + 
  geom_line(data=subset(CurrentPolicies_Deforestation_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=subset(GPPPolicies_Deforestation_index, year>=2015 & year<=2030 & region=="World"), aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = "(i) Deforestation emissions\n relative to 2010") +
  #scale_colour_manual(name="Scenario", values=c("Current Policies" = "red", "Good Practice Policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=22)) +
  theme(axis.text = element_text(face="bold", size=22)) +
  theme(legend.text = element_text(size=30)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))


fig <- grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i, ncol = 3, nrow = 3)
ggsave(file=paste("graphs/Fig2.png",sep=""),fig,width=24,height=14,dpi=200)

library(ggpubr)
fig2 <- ggarrange(a,b,c,d,e,f,g,h,i, ncol=3, nrow=3, common.legend = TRUE, legend="bottom")
ggsave(file=paste("graphs/Fig2_v2.png",sep=""),fig2,width=24,height=14,dpi=150)
