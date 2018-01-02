# purpose: a single canonical place from where to get MyM dimension labels, e.g. for regions, crops, techs, etc.

##### IMAGE / TIMER -----------------------------------------------------------------------------------

NR27T  = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","TOTAL")
NR27   = NR27T[1:27]
NR26   = NR27T[1:26]
NR26T  = NR27T[c(1:26,28)]
NRC    = NR27
NRCT   = NR27T

NR27T.nice = c('Canada','USA','Mexico','Rest C.Am.','Brazil','Rest S.Am.','N.Africa','W.Africa','E.Africa','South Africa','W.Europe','C.Europe','Turkey','Ukraine','Stan','Russia','M.East','India','Korea','China','SE.Asia','Indonesia','Japan','Oceania','Rest S.Asia','Rest S.Africa','-','World')

NUFPT  = c('food', 'feed' , 'other' , 'stock change' , 'total')
NUFPST = NUFPT[c(1:3,5)]

crops8 = c('temperate cereals','rice','maize','tropical cereals','pulses','roots and tubers','oilcrops','total vegetal')
NFCO   = c(crops8[1:7],'other')
NFCOT  = c(NFCO,crops8[8])
NFCT   = NFCOT[c(1:7,9)]
NFC    = NFCOT[1:7]

NAPT   = c('beef','milk','pork','mutton and goat','poultry and eggs','total animal')

NGST   = c('intensive','extensive','total')
NGS    = NGST[1:2]

biofuels5 = c('sugar cane biofuels','maize biofuels','woody biofuels','non woody biofuels','total biofuels')
biofuels4 = biofuels5[1:4]

NFFBC  = c('grass',NFC,biofuels4)

TURQ   = c('Total','Urban','Rural','Urban q1 (poor)','Urban q2','Urban q3','Urban q4','Urban q5 (rich)','Rural q1 (poor)','Rural q2','Rural q3','Rural q4','Rural q5 (rich)')
popseg = c(TURQ, 'Non-Quintile')

NTCT = c('PV','CSP','Wind onshore','Wind offshore','Hydropower','Geothermal','Nuclear','-','Conv. Coal','Conv. Oil','Natural Gas CT','Conv.Bio/Waste','IGCC','OGCC','NG(CC)','Biomass CC','Coal + CS','Oil + CS','Natural Gas + CS','Biomass + CS','CHP Coal','CHP Oil','CHP Natural Gas','CHP Biomass','CHP Coal + CS','CHP Oil + CS','CHP Natural Gas + CS','CHP Biomass + CS','Total')
NTC2 = NTCT[1:28]

FEEDSRCS = c('food crops','animal products','residues','scavenging','grass and fodder','total')

NFCAREAT = c('rainfed','irrigated','total')

NACT = c('food crops','grass and fodder','biofuel crops','total')

#Total number of Energy-Emissions Sources
NEES	= c('industry-heat', 'transport-heat', 'residential-heat', 'services-heat', 'other-heat', 'transformation', 'power-generation', 'losses',	'bunkers')

#! Number of Energy-Emissions Carriers (related to RSE)
#Transport: geen HLF/LLF, maar diesel/benzine
NEC8 	= c('coal', 'oil HLF', 'oil LLF', 'nat. gas', 'modern biofuel', 'traditional biofuels', 'electricity and heat', 'total')

# Number of Energy-Emissions Carriers (related to transformation)
NEC4 	= c('coal', 'oil HLF', 'oil LLF', 'nat. gas')

# Number of Energy-Emissions Carriers (related to primary energy production)
NECE7 	= c('coal', 'oil HLF', 'oil LLF', 'nat. gas', 'modern biofuel', 'non-thermal energy', 'hydro')

# industrial processes
NICO2T = c('Cement production', 'feedstocks', 'total')
NIN2OT	= c('Adipic acid', 'nitric acid', 'chemicals', 'total')
NICH4T = c('Steel production', 'chemical production', 'Bulk chemical production', 'total')

# NICOT	=4,	! 1. Iron&Steel; 2. paper; 3. Bulk chemicals; 4. total
#NINOxT	=6,	! 1. Iron&Steel; 2. chemicals; 3. cement; 4. paper; 5. Adipic Acid; 6. total
#NISO2T	=7,	! 1. Iron&Steel; 2. copper; 3. cement; 4. chemicals; 5. lead and zinc; 6. paper; 7. total
#NIVOCT	=5,	! 1. Iron&Steel; 2. Bulk chemicals; 3: paper; 4. Chemicals; 5. total

##### Water Demand Model -----------------------------------------------------------------------------------

sectors.eimat = c('Elec','Ind','Muni','Agriculture','Total non-agri')
sectors.eimt  = sectors.eimat[c(1,2,3,5)]

NCool  = c('dry','once','sea','pond','wet')
NCoolT = c(NCool,'total')
NCool.nice = c('Dry tower','Once through','Sea water','Pond','Wet tower')
NCoolT.nice = c(NCool.nice,'Total')

NEst = max.med.min = c('max','med','min')


##### Food Demand Model -----------------------------------------------------------------------------------

FOODS = c('animal','fruit and vegetables','luxuries','oils and oilcrops','pulses','staples')

# BFOODS = c('butter and cream','milk','eggs','cattle meat','fish and seafood','other meat and animal fat','pig meat','poultry meat','sheep and goat meat','aquatic plants','fruits','vegetables','honey','spices','stimulants','alcohol','sugar','fish oils','oilcrops','oils','pulses','roots and tubers','maize','plantains','rice','temperate cereals','tropical cereals')

MFOODS = c('cattle meat','butter and cream','milk','pig meat','sheep and goat meat','eggs','poultry meat','
fish and seafood','fish oils','other meat and animal fat','temperate cereals','barley','rice (milled eq)','maize','tropical cereals','pulses','roots and tubers','soyabeans','groundnuts','
sunflowerseed','coconuts','sesameseed','olives','other oilcrops (incl. palm)','soya oil','groundnut oil','sunflower oil','coconut oil','sesame oil','olive oil','other oilcrop oils','
aquatic plants','fruits','grapes','vegetables','beer','wine','other alcohol','spices','stimulants','sugar beet','sugar cane','sugar raw','sugar other','nuts','plantains')

