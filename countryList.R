#' Selected Countries in the list
#' Based on the Data completeness and total energy consumption

country_list <- c("AUS", "AUT", "BEL", "BRA", "CAN", "CHL", "CHN","DEU", "ESP", "FIN", "DNK", 
                  "FRA", "GBR", "GRC", "HUN", "IRL", "IND", "IDN" ,"ITA" ,"ISL", "JPN",  
                  "KOR", "MEX", "NLD", "NZL", "POL", "PRT", "SGP", "SWE", "USA", "ZAF")

# Exporter List
exporter_list <- dset %>% select(Exporter, exporter) %>% distinct()
rem_exporter_list <- c("AES", "AFA", "AFN", "AFS", "AFR", "AFZ", # Remove group of countries so that the exporter is at country level
                       "AMA", "AME", "AMR", "AMS", "AMN" ,"ANE", "AOZ", "APE", "AOA",
                       "ASE", "ASO", "ASS", "BAL", "BLX", "BRI", "CAU",
                       "CEA", "CIS", "CEI", "DEV", "E15", "EM1", "ENM", "GLF", "GOA",
                       "EUA", "EUR", "EUC", "G20", "G8", "MED", "MEN", "MER",
                       "MOA", "NRD", "OCD", "OCN", "OPN", "OPP", "PED", "PMA", "PMO",
                       "SUN", "UE", "WLD", "XXX", "YUG") 
