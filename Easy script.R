CurrentPolicies <- ImportTimerScenario('NPi_update','NPi')
CurrentPolicies_indicators <- ProcessTimerScenario(CurrentPolicies)

rm(CO2Spec)
rm(ElecProd)
rm(EMISCO2EQ)
rm(ENEMISCH4)
rm(ENEMISN2O)
rm(ENEMISCO2)
rm(HFC_reg)
rm(PFC_reg)
rm(INDEMISCH4)
rm(INDEMISCO2)
rm(INDEMISN2O)
rm(LUEMCO2)
rm(LUEMCH4)
rm(LUEMN2O)

rm(EMISCO2EQ_tmp)
rm(HFC_tmp)
rm(PFC_tmp)
rm(tmp1)
rm(tmp2)
rm(tmp3)

rm(ENEMISCH4_TOT)
rm(ENEMISN2O_TOT)
rm(ENEMISCO2_TOT)
rm(HFC_TOT)
rm(PFC_TOT)
rm(INDEMISCH4_TOT)
rm(INDEMISCO2_TOT)
rm(INDEMISN2O_TOT)
rm(LUEMCO2_TOT)
rm(LUEMCH4_TOT)
rm(LUEMN2O_TOT)


CPS <- CurrentPolicies
x<-subset(CurrentPolicies$ENEMISCO2, region=="USA" & sector=="Total" & energy_carrier=="Total")$year
y<-subset(CurrentPolicies$ENEMISCO2, region=="USA" & sector=="Total" & energy_carrier=="Total")$value
plot(x,y)

CPS <- CurrentPolicies
x<-subset(CPS$ENEMISCO2, region!="World" & sector=="Total" & energy_carrier=="Total")$region
y<-subset(CPS$ENEMISCO2, region!="World" & sector=="Total" & energy_carrier=="Total")$value
plot(x,y)

CPS <- CurrentPolicies
plot(CPS$ENEMISCO2)
plot(CPS$ENEMISCO2$year + CPS$ENEMISCO2$value)
plot(CPS$ENEMISCO2$value~CPS$ENEMISCO2$region+CPS$ENEMISCO2$sector)

CP2s<-subset(CPS$ENEMISCO2, region!="World")
coplot(CPS$ENEMISCO2$value~CPS$ENEMISCO2$year|CPS$ENEMISCO2$region)
#coplot(CPS2$ENEMISCO2$value~CPS2$ENEMISCO2$year|CPS2$ENEMISCO2$region)

CPSi <- CurrentPolicies_indicators
x<-subset(CPSi$RenElecShare, region=="CHN")$year
y<-subset(CPSi$RenElecShare, region=="CHN")$value
plot(x,y)

Rtmp1 <- subset(CPSi$RenElecShare, region=="CHN")
ggplot(data=Rtmp1)+geom_point(mapping=aes(x=year, y=value))

Rtmp2 <- subset(CPSi$RenElecShare, region=="CHN" | region=="USA")
ggplot(data=Rtmp2)+geom_point(mapping=aes(x=year, y=value, colour=region))
ggplot(data=Rtmp2)+geom_point(mapping=aes(x=year, y=value, size=region))
ggplot(data=Rtmp2)+geom_point(mapping=aes(x=year, y=value, alpha=region))

Etmp1 <- subset(CPS$ENEMISCO2, region=="World" & energy_carrier=="Total")
ggplot(data=Etmp1)+geom_point(mapping=aes(x=year, y=value, colour=sector))
ggplot(data=Etmp1)+geom_point(mapping=aes(x=year, y=value))+facet_wrap(~sector)

Etmp2 <- subset(CPS$ENEMISCO2, region=="World")
ggplot(data=Etmp2)+geom_point(mapping=aes(x=year, y=value))+facet_grid(energy_carrier~sector)

Rtmp1 <- subset(CPSi$RenElecShare, region=="CHN")
ggplot(data=Rtmp1)+geom_smooth(mapping=aes(x=year, y=value))

Rtmp3 <- subset(CPSi$RenElecShare, region %in% c("USA", "CHN", "INDIA"))
ggplot(data=Rtmp3)+geom_smooth(mapping=aes(x=year, y=value, linetype=region))
ggplot(data=Rtmp3)+geom_smooth(mapping=aes(x=year, y=value, linetype=region, colour=region))

ggplot(data=Rtmp1)+geom_point(mapping=aes(x=year, y=value)) + geom_smooth(mapping=aes(x=year, y=value))

Etmp3 <- subset(CPS$ENEMISCO2, region=="World" & energy_carrier=="Total")
ggplot(data=Etmp3, mapping=aes(x=year, y=value))+geom_point(mapping=aes(colour=sector))+geom_smooth(mapping=aes(colour=sector))

Etmp4 <- subset(CPS$ENEMISCO2, year == 2015 & region=="World" & energy_carrier=="Total" & sector != "Total")
ggplot(data=Etmp4)+geom_bar(mapping = aes(x=sector, y=value), stat="identity")

Etmp5 <- subset(CPS$ENEMISCO2, region=="World" & energy_carrier=="Total" & sector != "Total")
ggplot(data=Etmp5)+stat_summary(mapping=aes(x=sector, y=value), fun.ymin=min, fun.ymax=max, fun.y=median)

ggplot(data=Etmp4)+geom_bar(mapping = aes(x=sector, y=value, colour=sector), stat="identity")
ggplot(data=Etmp5)+geom_bar(mapping = aes(x=sector, y=value, fill=year), stat="identity")
