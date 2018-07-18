source('Settings.R')
source('Import_TIMER_output.R')
source('Process_TIMER_output.R')

#ProjectDir = "~/disks/y/ontwapps/Timer/Users/Mark"
#Project = 'CD_LINKSupdate'

ProjectDir = "~/disks/y/ontwapps/Timer/Users/Mathijs/Projects/CD-LINKS"
Project = "CD_LINKS"
R_dir = paste(ProjectDir, Project, "R-scripts/TIMER_output", sep="/")
setwd(R_dir)
getwd()

NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy')
NoPolicy_ind <- ProcessTimerScenario(NoPolicy)
NPi <- ImportTimerScenario('NPi','NPi')
NPi_ind <- ProcessTimerScenario(NPi)
INDCi <- ImportTimerScenario('INDCi','INDCi')
INDCi_ind <- ProcessTimerScenario(INDCi)
NPi2020_1000 <- ImportTimerScenario('NPi2020_1000','NPi2020_1000')
NPi2020_1000_ind <- ProcessTimerScenario(NPi2020_1000)
NPi2020_400<- ImportTimerScenario('NPi2020_400','NPi2020_400')
NPi2020_400_ind <- ProcessTimerScenario(NPi2020_400)