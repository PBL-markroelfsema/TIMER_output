# example how to use the r2mym functions in mym2r.R

# NOTE: examples below may help, but only reading the comments inside the mym2r functions prepare.r2mym() and write.r2mym() may get you there faster!

# make sure this script, mym2r.R, AllWCS.2.out, and the folder example.r2mym are in your R working directory
# e.g.:
# setwd('D:/code per project/mym2r')
setwd('~/disks/y/ontwapps/Timer/Users/BijlD/timer_tools/R-scripts/mym2r')

# load mym2r.R functions so we can call them. NOTE: mym2r.R uses libraries plyr, stringr, and reshape2, so you may need to install those

source('mym2r.R')   # include path to timer_tools/R-scripts if necessary

# country mapping
cmap = read.csv('example.r2mym/mapping.csv')

# eerst helder krijgen hoeveel (en welke) waarden er zijn per M-dimensie
# normally I would use text labels for these, but this works as well. see also example 3 below
all.year	<- c(1990,1995,2000,2005,2010,2013)
all.c_224	= 1:224
all.age		= 4:21
all.sex		= 1:2

##### oud voorbeeld van Paul Lucas: #####

# stub1 = expand.grid(year = all.year, VSF_nr = all.c_224, age = all.age, sex = all.sex)
# inputfile1 = 'IHME-Data-Childhood underweight-Deaths.csv'
# outputfile1= 'childunderweight_deaths.dat'
# header1 = paste0('REAL childunderweight_deaths[',length(all.c_224),',',length(all.age),',',length(all.sex),'](t) = [')

# Mdata1 <- prepare.r2mym(inputfile1, stub1)
# dummy <- write.r2mym(Mdata1, outputfile1, header1)

##### second example by David Bijl: #####

# gedoe met ?..location opgelost door .csv opnieuw te saven, grote getallen worden daarbij wel automatisch afgerond.

inputfile2  = 'example.r2mym/IHME-Data-Childhood underweight-Deaths-2.csv'
outputfile2 = 'example.r2mym/childunderweight_deaths-2.dat'

stub2 = expand.grid(year = all.year, VSF_nr = all.c_224, age = all.age, sex = all.sex) # == stub1

data2 <- read.csv(inputfile2)

# Map country-data to GISMO order
data2 = merge(data2, cmap, by.x='location', by.y='location')

Mdata2 <- prepare.r2mym(data = data2, stub = stub2, value.var = 'nm_mean')

write.r2mym(data = Mdata2, outputfile = outputfile2, value.var = 'nm_mean', MyM.vartype = 'REAL', MyM.varname = 'childunderweight_deaths', time.dependent = TRUE)
Mdata2.reread = read.mym2r.nice(mym.folder = '', scen.econ = '', filename = outputfile2, collist = list(all.c_224,all.age,all.sex), namecols = c('VSF_nr','age','sex'), 
                                yearheader = 'year', yearsrun = all.year)
head(Mdata2)
head(Mdata2.reread)
summary(Mdata2)
summary(Mdata2.reread)  # note the dimension labels are now factors with numeric labels e.g. c('1','2'). This can be very confusing! Better to supply text labels e.g. c('Male','Female')

##### third example, including string factors: #####

all.sex3 = c('Male','Female')
stub3    = expand.grid(year = all.year, VSF_nr = all.c_224, age = all.age, sex = all.sex3)

data3    = read.csv(inputfile2)
data3$sex = data3$sex_name             # use the text labels!

outputfile3 = 'example.r2mym/childunderweight_deaths-3.dat'

data3 = merge(data3, cmap, by.x='location', by.y='location')

Mdata3 <- prepare.r2mym(data = data3, stub = stub3, value.var = 'nm_mean')

write.r2mym(data = Mdata3, outputfile = outputfile3, value.var = 'nm_mean', MyM.vartype = 'REAL', MyM.varname = 'childunderweight_deaths', time.dependent = TRUE)

Mdata3.reread = read.mym2r.nice(mym.folder = '', scen.econ = '', filename = outputfile3, collist = list(all.c_224,all.age,all.sex3), namecols = c('VSF_nr','age','sex'), 
                                yearheader = 'year', yearsrun = all.year)
head(Mdata3)
head(Mdata3.reread)
summary(Mdata3)
summary(Mdata3.reread) # note the dimension labels are now factors with numeric labels e.g. c('1','2'). This can be very confusing! Better to supply text labels.

##### fourth example, no time dependence: #####

stub4 = expand.grid(VSF_nr = all.c_224, age = all.age, sex = all.sex)

data4 = read.csv(inputfile2)
data4 = subset(data4, year==2000)

outputfile4 = 'example.r2mym/childunderweight_deaths-4.dat'

data4 = merge(data4, cmap, by.x='location', by.y='location')

Mdata4 <- prepare.r2mym(data = data4, stub = stub4, value.var = 'nm_mean')

write.r2mym(data = Mdata4, outputfile = outputfile4, value.var = 'nm_mean', MyM.vartype = 'REAL', MyM.varname = 'childunderweight_deaths', time.dependent = FALSE)

Mdata4.reread = read.mym2r.nice(mym.folder = '', scen.econ = '', filename = outputfile4, collist = list(all.c_224,all.age,all.sex), namecols = c('VSF_nr','age','sex'), 
                                yearheader = 'year', yearsrun = all.year) # these last 2 arguments have no effect this time, because read.mym2r auto-detects time dependence
head(Mdata4)
head(Mdata4.reread)
summary(Mdata4)
summary(Mdata4.reread) # note the dimension labels are now factors with numeric labels e.g. c('1','2'). This can be very confusing! Better to supply text labels.
