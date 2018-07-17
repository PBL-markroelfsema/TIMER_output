source('Settings.R')
source('process_data.R')
iea_data <- read.table("Data/iea-tj-1990-2015-v2.csv", header=TRUE, sep=",", row.names=NULL)


edgar_co2 <- invisible(fread(paste0("data/EDGAR CO2.csv"),header=TRUE))
save("edgar_co2",file = paste0("data/EDGAR CO2.csv",".Rdata"))
edgar_co2 <- gather(edgar_co2, '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', 
                               '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', 
                               '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', 
                               '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008',
                                        key='year', value='value')
edgar_co2_oil_gas <- subset(edgar_co2, IPCC=="1B2")

edgar_ch4 <- invisible(fread(paste0("data/EDGAR CH4.csv"),header=TRUE))
save("edgar_co2",file = paste0("data/EDGAR CH4.csv",".Rdata"))
edgar_ch4 <- gather(edgar_ch4, '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', 
                               '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', 
                               '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', 
                               '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008',
                               key='year', value='value')
edgar_ch4_oil_gas <- subset(edgar_ch4, IPCC=="1B2")

#iea_data <- read.csv2("Data/iea-tj-1990-2015-v2.csv")
iea_oil_gas <- subset(iea_data, FLOW %in% c("IMPORTS", "EXPORTS", "AVBUNK", "MARBUNK", "STOCKCHA", "TPES")) 
iea_oil_gas <- subset(iea_oil_gas, TIMER_CARRIER %in% c("Gas", "OilHeavy", "OilLight"))
iea_oil_gas_aggr <- iea_oil_gas %>% group_by(COUNTRY, FLOW, TIMER_CARRIER, Year) %>% summarise(value=sum(VALUE))
#iea_oil_gas_intensity <- spread(iea_oil_gas_aggr, key=c("COUNTRY", "FLOW", "TIMER_CARRIER", "YEAR"), value = VALUE)
iea_oil_gas_intensity <- spread(iea_oil_gas_aggr, key=c("FLOW"), value = value)
iea_oil_gas_intensity[is.na(iea_oil_gas_intensity)] <- 0
iea_oil_gas_intensity <- mutate(iea_oil_gas_intensity, production=TPES-IMPORTS-EXPORTS-AVBUNK-MARBUNK-STOCKCHA)
