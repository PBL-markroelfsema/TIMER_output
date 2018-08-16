library(gridExtra)
source('functions/Settings.R')
source('functions/General Functions.R')
source('functions/Import_TIMER_output.R')
source('functions/Process_TIMER_output.R')
source('functions/pbl_colors.R')
source('CD_LINKS/Settings_indicators.R')

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
INDCi2030_1000<- ImportTimerScenario('INDC2030i_1000','INDC2030i_1000')

NoPolicy_ind <- ProcessTimerScenario(NoPolicy)
NPi_ind <- ProcessTimerScenario(NPi)
INDCi_ind <- ProcessTimerScenario(INDCi)
NPi2020_1000_ind <- ProcessTimerScenario(NPi2020_1000)
NPi2020_400_ind <- ProcessTimerScenario(NPi2020_400)
INDCi2030_1000_ind <- ProcessTimerScenario(INDCi2030_1000)

