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
          'biomass burning'='Land-use',
          'burning of traditional biomass'='Land-use',
          'timber pool (short lifetime)'='Land-use',
          'timber pool (long lifetime)'='Land-use',
          'carbon release by regrowing vegetation'='Land-use',
         #LUEMCH4
          #'biomass burning'='Land-use',
          'fuelwood burning'='Land-use',
          'agricultural waste burning'='Land-use',
          'savanna burning'='Land-use',
          'landfills'='Waste',
          'sewage'='Waste',
          'wetland rice'='Agriculture',
          'animals'='Agriculture',
          'animal waste'='Agriculture',
        #LUEMN2O
         'deforestation/biomass burning'='Land-use',
         'traditional biomass/fuelwood burning'='Land-use',
          #'agricultural waste burning'
          #'savanna burning'
         'land clearing'='Land-use',
         'fertilizers'='Agriculture',
         'stables'='Agriculture',
         'grazing'='Agriculture',
         'manure application'='Agriculture',
         'fertilizers indirect'='Agriculture',
         'domestic sewage'='Waste',
         'from residues'='Land-use',
         'biological N-fixation'='Land-use',
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