MainSector <- function(sector)
{  switch(as.character(sector),
          'Total'='Total',
          'total'='Total',
          #ENEMIS
          'End-use industry'='Industry', 
          'End-use transport'='Transport', 
          'End-use residential'='Buildings', 
          'End-use services'='Buildings', 
          'End-use other'='Industry', 
          'Energy transformation'='Energy supply', 
          'Power generation'='Energy supply', 
          'losses/leakages'='Energy supply', 
          'Bunkers'='Bunkers',
         #INDEMISCO2
          'cement'='Industry',
          'feedstock'='Industry',
         #INDEMISCH4
          'iron & steel'='Industry',
          'chemicals'='Industry',
          'bulk chemicals'='Industry',
         #INDEMISN2O
          'adipic acid'='Industry',
          'nitric acid'='Industry',
          #'chemicals'
         #LUEMCO2
          'biomass burning'='LULUCF',
          'burning of traditional biomass'='LULUCF',
          'crops  on peatland'='LULUCF', # Is this correct?
          'timber pool (short lifetime)'='LULUCF',
          'timber pool (long lifetime)'='LULUCF',
          'carbon release by regrowing vegetation'='LULUCF',
         #LUEMCH4
          'biomass burning'='LULUCF',
          'fuelwood burning'='LULUCF',
          'agricultural waste burning'='LULUCF',
          'savanna burning'='LULUCF',
          'landfills'='Waste',
          'sewage'='Waste',
          'wetland rice'='Agriculture',
          'animals'='Agriculture',
          'animal waste'='Agriculture',
          'degraded peatlands'='LULUCF', # is this correct?
        #LUEMN2O
         'deforestation/biomass burning'='LULUCF',
         'traditional biomass/fuelwood burning'='LULUCF',
         'agricultural waste burning'='LULUCF',
         'savanna burning'='LULUCF',
         'land clearing'='LULUCF',
         'fertilizers'='Agriculture',
         'stables'='Agriculture',
         'grazing'='Agriculture',
         'manure application'='Agriculture',
         'fertilizers indirect'='Agriculture',
         'domestic sewage'='Waste',
         'from residues'='LULUCF',
          #'crop residues'='LULUCF',
          #'biological N-fixation'='LULUCF',
          'crop residues'='Agriculture',
          'biological N-fixation'='Agriculture',
          'degraded peatlands'='LULUCF',
        #HFC
         'HFC23'='Industry',
         'HFC32'='Industry',
         'HFC4310'='Industry',
         'HFC125'='Industry',
         'HFC134a'='Industry',
         'HFC143a'='Industry',
         'HFC152'='Industry',
         'HFC227'='Industry',
         'HFC236'='Industry',
         'HFC245'='Industry',
        #PFC
         'CF4'='Industry',
         'C2F6'='Industry',
         'SF6'='Industry',
         'C6F14'='Industry')
}

wait <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}