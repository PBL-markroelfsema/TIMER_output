source('functions/Settings.R')
#scenario = 'NoPolicy'
#TUSSdir = 'indicatoren'
#filedir = paste(TIMER_folder, scenario, TUSSdir, sep="\\")
#filename = 'ENEMISCO2.out'
#filepath = paste(filedir, "\\", filename, sep="")
#print(filepath)

filedir = "ClimatePolicies\\2_TIMER\\TUSS_2015\\views"
filename = "emission.vdf"
filepath = paste(filedir, "\\", filename, sep="")

ReadTUSSFile = function(filepath) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

x<-ReadTUSSFile(filepath)
