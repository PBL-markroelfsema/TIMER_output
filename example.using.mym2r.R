# example how to use mym2r.R

# make sure this script, mym2r.R, AllWCS.2.out, and the folder structure example.mym.output are in your R working directory
# e.g.:
# setwd('D:/code per project/mym2r')
#setwd('~/disks/y/ontwapps/Timer/Users/BijlD/timer_tools/R-scripts/mym2r')

# load mym2r.R functions so we can call them. NOTE: mym2r.R uses libraries plyr, stringr, and reshape2, so you may need to install those

source('mym2r.R')

# prepare correct labels for mym file dimensions

regions28 = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","TOTAL")
sectors   = c('Elec','Ind','Muni','Total non-agri')

# call mym2r

# (slashes are added automatically)
allwcs = read.mym2r.nice(mym.folder='example.mym.output', scen.econ='example.scenario', filename='AllWCS', collist=list(regions28,sectors), namecols=c('region','sector'))

# in case you don't have scenario folders etc:
allwcs2 = read.mym2r.nice(mym.folder='', scen.econ='', filename='AllWCS.2', collist=list(regions28,sectors), namecols=c('region','sector'))

# specify novarname=TRUE if you don't want the varname column

head(allwcs)

# demonstrate auto-dimensions-lookup functionality:

allwcs = read.mym2r.nice(mym.folder='example.mym.output', scen.econ='example.scenario', filename='AllWCS', novarname = T)
head(allwcs)
