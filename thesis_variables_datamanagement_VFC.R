#/**************************
# Thesis Data Prep
# 
#
# Date: Feb 2023
# By: Valeria Flores-Cadena
# 
# 
## ****************************

#clear everything
rm(list=ls())

# install package
# install.packages("peacesciencer")
# install.packages("countrycode")
# install.packages('openxlsx')

# load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(peacesciencer)
library(readxl)
library(countrycode)
library(Hmisc)
library(openxlsx)

setwd("/Volumes/GoogleDrive-115054236824584854554/My Drive/Senior Thesis")
getwd()

############################################################################
## Independent Variable 
inflation <- read_excel("Variables/INDEPENDENT/World_Development_Indicators.xlsx")
# calulating nas
#rowSums(is.na(inflation))
#table(is.na(inflation))

#pivot to make year column rather than having each individual year as a column name
inflation <- 
  pivot_longer(data = inflation, 
               cols = '1960':'2021',
               names_to = 'year',
               values_to = "inflation, consumer prices (annual %)")

#removing columns
inflation <- inflation %>% 
  select(-c('Country Code', 'Indicator Name', 'Indicator Code'))

#pivot wider to pair inflation values with country names 
inflation <- 
  pivot_wider(data = inflation, 
              names_from = "Country Name",
              values_from = "inflation, consumer prices (annual %)")

#removing unnecessary columns
inflation = select(inflation, -c('American Samoa','Africa Western and Central', 
                                 'Central Europe and the Baltics',
                                 'Africa Western and Central',
                                 'Africa Eastern and Southern',
                                 'Central Europe and the Baltics',
                                 'Caribbean small states',
                                 'Early-demographic dividend',
                                 'East Asia & Pacific',
                                 'East Asia & Pacific (excluding high income)',
                                 'East Asia & Pacific (IDA & IBRD countries)',
                                 'Euro area','Europe & Central Asia',
                                 'Europe & Central Asia (excluding high income)',
                                 'Europe & Central Asia (IDA & IBRD countries)',
                                 'European Union','Fragile and conflict affected situations',
                                 'Heavily indebted poor countries (HIPC)',
                                 'High income','IBRD only','IDA & IBRD total',
                                 'IDA blend','IDA only','IDA total','Late-demographic dividend',
                                 'Latin America & Caribbean',
                                 'Latin America & Caribbean (excluding high income)',
                                 "Not classified",
                                 'Least developed countries: UN classification',
                                 'Low income',
                                 'Lower middle income',
                                 'Low & middle income',
                                 'Middle East & North Africa',
                                 'Middle East & North Africa (excluding high income)' ,
                                 'Middle income',
                                 'OECD members',
                                 'Other small states',
                                 'Pre-demographic dividend',
                                 'Post-demographic dividend',
                                 'West Bank and Gaza',
                                 'Pacific island small states',
                                 'Sub-Saharan Africa (excluding high income)',
                                 'Sint Maarten (Dutch part)',
                                 'Latin America & the Caribbean (IDA & IBRD countries)',
                                 'Middle East & North Africa (IDA & IBRD countries)',
                                 'Sub-Saharan Africa (IDA & IBRD countries)',
                                 'Upper middle income',
                                 'Small states',
                                 'Sub-Saharan Africa',
                                 'South Asia (IDA & IBRD)',
                                 'World',
                                 'Arab World',
                                 'North America',
                                 'Puerto Rico',
                                 'South Asia', 
                                 'Bermuda', 'British Virgin Islands', 'Cayman Islands',
                                 'Channel Islands', 'Curacao', 'New Caledonia',
                                 'Northern Mariana Islands', 'Faroe Islands',
                                 'French Polynesia', 'Gibraltar', 'Greenland',
                                 'Guam', "St. Martin (French part)", "Turks and Caicos Islands",
                                 "Virgin Islands (U.S.)", "Isle of Man", "Guam", "Aruba",
                                 "Dominica", 'Grenada', 'St. Lucia', 'St. Vincent and the Grenadines',
                                 'Antigua and Barbuda', 'St. Kitts and Nevis', "American Samoa",
                                 "Kiribati", 'Liechtenstein', "Samoa",
                                 "Palau", "San Marino", "Nauru", "Marshall Islands",
                                 "Micronesia, Fed. Sts.", "Sao Tome and Principe", "Seychelles", 
                                 "Tonga", "Tuvalu", "Vanuatu"))

####INFLATION 2
  #inflation2 <- inflation %>%
  #filter(gwno == 352 & year == 1975) 
  #sum(is.na(inflation$'inflation, consumer prices (annual %)'))
  #inflation2 <- inflation %>% distinct()
  #inflation3 <- inflation2 %>% 
  #filter(year > 1980)
  #inflation4 <- inflation %>% 
  #drop_na()

#putting column names again as just country with countries as values (had to undo this before to easily remove the countries that we arent going to use)
inflation <- 
  pivot_longer(data = inflation, 
               cols = 'Afghanistan':'Zimbabwe',
               names_to = 'countryname_raw',
               values_to = "inflation, consumer prices (annual %)")

#using countrycode to add gwno
inflation <-inflation %>% 
  mutate(gwno = `countryname_raw`)

inflation$gwno <- (countrycode(inflation$`countryname_raw`, origin = 'country.name', destination = 'gwn'))

inflation$gwno[inflation$countryname_raw == "Yemen, Rep."] <- 678
inflation$gwno[inflation$countryname_raw == "Andorra"] <- 232
inflation$gwno[inflation$countryname_raw == "Hong Kong SAR"] <- 708
inflation$gwno[inflation$countryname_raw == "China"] <- 710
inflation$gwno[inflation$countryname_raw == "Macao SAR"] <- 709
inflation$gwno[inflation$countryname_raw == "Monaco"] <- 221
inflation$gwno[inflation$countryname_raw == "Turkiye"] <- 640

#making gwno and year columns numeric
inflation$gwno <- as.numeric(inflation$gwno)
inflation$year<- as.numeric(inflation$year)

#removing unncessary columns (again)
inflation <- inflation %>%
  relocate('year', 'gwno')



############################################################################
## Dependent Variables
# Political Instability proxies: Cabinet Changes, coups, demonstrations, riots, assassinations

## FROM DATABASE OF POLITICAL INSTITUTIONS - cabinet changes/veto players drop 
# For variables that have binary values, “1” is equivalent to “yes,” while “0” is equivalent to “no.”
# Note: With a few exceptions (noted below) the event where no information was available the cells were left
# blank whereas in the case where the information was not applicable cells were marked with “NA” or “-
  # 999” for numeric variables.
# NA is recorded in the following cases: when a country is a colony, even if it has internal self-government
# within a commonwealth; for the Soviet Republics while they were part of the USSR; for countries in the
# midst of civil war or political crisis.

# STABILITY
# STABS
# STABS_STRICT
# These variables count the percent of veto players who drop from the government in any given year. Veto
# players are defined as in CHECKS.
# If LIEC is less than 5 (6 for STABS_STRICT) in year t-1, then it is assumed that the only veto player in year
# t-1 is the executive. STABS in year t is 1 if chief executive changes in year t, 0 otherwise.
# If LIEC is 5 or greater (6 or greater for STABS_STRICT):
  # In presidential systems, if the president does not control the legislature (via closed list and a majority), then
  # veto players are the president, and each chamber. If presidents gain control of the legislature at time t, then
  # the chambers are counted as no longer being veto players. Similarly, if the president changes. If the largest
  # opposition party has a majority in the legislature at time t-1 but not in time t, a change in veto players is again
  # recorded. If the largest government party has a majority in the legislature (and there is no closed list) at time
  # t-1 but not at time t, a change in veto player is again recorded.
  # In parliamentary systems, if members of the government coalition at t-1 are no longer in government at time
  # t, that number of veto players changes, similarly if the prime minister changes. If an opposition party has a
  # majority at t-1 but that same party does not have a majority at t, then one veto player is said to have dropped.
  # If parliamentary systems go from no government majority or no closed list to government majority and closed
  # list at time t, then the chambers are counted as no longer being veto players.
# STABNS
# STABNS_STRICT
  # Calculated like STABS and STABNS_STRICT, but ignores the presence of a second chamber in the
  # calculation of CHECKS in period t-1.

DPI <- read.csv("Variables/DEPENDENT/Database of Political Institutions/DPI2020.csv")
DPI <- DPI %>% 
  select(c("countryname_raw" = "countryname", "ifs", "year", "system", "stabs", "stabs_strict"))

#making new column for gwno
DPI <- DPI %>% 
  mutate(gwno = countryname_raw)

# using countrycode package to add gwno codes
DPI$gwno <- (countrycode(DPI$countryname, origin = 'country.name', destination = 'gwn'))

#RESULTING WARNING: 
#Some values were not matched unambiguously: Cent. Af. Rep., Dom. Rep., GDR, 
#Grenada, PRC, PRK, ROK, S. Africa, Samoa, St. Lucia, Vanuatu, Yemen, Yemen (AR)

# fixing ambigious results
DPI$gwno[DPI$countryname == "Cent. Af. Rep"] <- 482 #Central African Republic
DPI$gwno[DPI$countryname == "Dom. Rep."] <- 42 #Dominican Republic
DPI$gwno[DPI$countryname == "GDR"] <- 265 #Germany Dem Rep
DPI$gwno[DPI$countryname == "Grenada"] <- 55 #Grenada
DPI$gwno[DPI$countryname == "PRC"] <- 710 #China
DPI$gwno[DPI$countryname == "PRK"] <- 731 #North Korea
DPI$gwno[DPI$countryname == "ROK"] <- 732 #South Korea
DPI$gwno[DPI$countryname == "S. Africa"] <- 560 #South Africa
DPI$gwno[DPI$countryname == "Samoa"] <- 990 #Samoa
DPI$gwno[DPI$countryname == "St. Lucia"] <- 56 #St. Lucia
DPI$gwno[DPI$countryname == "Vanuatu"] <- 935 #Vanuatu
DPI$gwno[DPI$countryname == "Yemen"] <- 678 #Yemen
DPI$gwno[DPI$countryname == "Yemen (AR)"] <- 678 #Yemen

DPI <- DPI%>% 
  select(-c("ifs")) %>%
  relocate('year', 'gwno')

## FROM SYSTEMIC PEACE
MEPV <- read_excel("Variables/DEPENDENT/Systemic Peace/MEPV2012ex.xls")
MEPV <- MEPV %>%
  select(c("scode" = "SCODE", "ccode" = "CCODE", "countryname" = "COUNTRY", "year" = "YEAR", "ind" = "IND", "civviol" = "CIVVIOL", "civwar" = "CIVWAR", "ethviol" = "ETHVIOL", "ethwar" = "ETHWAR"))

MEPV <- MEPV %>% 
  mutate(gwno = countryname)

MEPV$gwno <- (countrycode(MEPV$countryname, origin = 'country.name', destination = 'gwn'))

#Warning message:
#In countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest,  :
#Some values were not matched unambiguously: RUSRia, Serbia and Montenegro, 
#Vietnam South, Vietnam, South, Yemen

#fixing ambiguous
MEPV$gwno[MEPV$countryname == "Yemen"] <- 678 #Yemen 
MEPV$gwno[MEPV$countryname == "RUSRia"] <- 365 #russia
MEPV$gwno[MEPV$countryname == "Serbia and Montenegro"] <- 340 #serbia and montenegro (1991-2005)
MEPV$gwno[MEPV$countryname == "Vietnam South"] <- 817
MEPV$gwno[MEPV$countryname == "Vietnam, South"] <- 817

MEPV <- MEPV %>% 
  rename("countryname_raw" = "countryname") %>%
  select(-c("ccode", "scode")) %>%
  relocate('year', 'gwno')

# CIVVIOL (2-numeric) Magnitude score of episode(s) of civil violence involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes
# CIVWAR (2-numeric) Magnitude score of episode(s) of civil warfare involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes
# ETHVIOL (2-numeric) Magnitude score of episode(s) of ethnic violence involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes
# ETHWAR (2-numeric) Magnitude score of episode(s) of ethnic warfare involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

CSPCoups <- read_excel("Variables/DEPENDENT/Systemic Peace/CSPCoupsAnnualv2021.xls")
CSPCoups <- CSPCoups %>% 
  select(c("ccode", "scode", "countryname_raw" = "country", "year", "scoup1", "atcoup2", "pcoup3", "reboutex", "assassex", "resignex"))

CSPCoups <- CSPCoups %>% 
  mutate(gwno = countryname_raw)
CSPCoups$gwno <- (countrycode(CSPCoups$countryname_raw, origin = 'country.name', destination = 'gwn'))

#Warning message:
#In countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest,  :
#Some values were not matched unambiguously: Serbia and Montenegro, Vietnam South, Yemen

#fixing ambiguous
CSPCoups$gwno[CSPCoups$countryname_raw == "Yemen"] <- 678 #Yemen 
CSPCoups$gwno[CSPCoups$countryname_raw == "Serbia and Montenegro"] <- 340 #serbia and montenegro (1991-2005)
CSPCoups$gwno[CSPCoups$countryname_raw == "Vietnam South"] <- 817

CSPCoups <- CSPCoups %>%
  select(-c('ccode', 'scode')) %>%
  relocate('year', 'gwno')

# SCOUP1 Successful Coups: Number of successful coups d’état that occurred in the year of record
# ATCOUP2 Attempted Coups: Number of attempted (but ultimately unsuccessful) coups d’état that occurred in the year of record
# PCOUP3 Coup Plots: Number of (thwarted) coup plots that were reported by government officials during the year of record
# APCOUP4 Alleged Coup Plots: Number of alleged coup plots announced by government officials during the year of record (allegations of coup plots usually involve less
  # formal adjudication procedures and are often used by authorities to justify repression of opposition leaders)
# REBOUTEX Ouster of Leadership by Rebel Forces: Indicator of the forced ouster of a ruling executive as a direct result of armed action by rebel forces fighting
  # against forces loyal to the regime (rebel leaders then assume regime leadership often resulting in a political, or socio-political, revolution)
# ASSASSEX Assassination of Executive: Indicator of the assassination of the ruling executive during the year of record (assassinations are perpetrated by
  # persons acting outside the ruling elite and do not result in a substantive change in regime leadership)
# RESIGNEX Resignation of Executive Due to Poor Performance and/or Loss of Authority: Indicator of the coerced resignation of the ruling executive due to poor
  # performance and accompanied by increasing public discontent and popular demonstrations calling for the ouster of the executive leadership (including
  # impeachment proceedings which may or may not be successful in forcing executive removal); like assassinations, coerced resignations of an executive
  # do not result in a substantive change in regime leadership (although they may lead to new elections)

## FROM CROSS-NATIONAL TIME-SERIES DATA ARCHIVE
CNTS <- read_excel("Variables/DEPENDENT/Cross-National Time-Series Data Archive/2022 Edition CNTSDATA.xlsx")
CNTS <- CNTS %>% 
  select(c("Country Code", "Country", "Year", "Assassinations", "Guerrilla Warfare", "Government Crises", 
           "Purges", "Riots", "Revolutions", "Anti-Government Demonstrations"))
CNTS = CNTS[-1,] # remove the first row 

CNTS$Year = as.numeric(as.character(CNTS$Year))
CNTS$Assassinations = as.numeric(as.character(CNTS$Assassinations))
CNTS$`Guerrilla Warfare` = as.numeric(as.character(CNTS$`Guerrilla Warfare`))
CNTS$`Government Crises` = as.numeric(as.character(CNTS$`Government Crises`))
CNTS$Purges = as.numeric(as.character(CNTS$Purges))
CNTS$Riots = as.numeric(as.character(CNTS$Riots))
CNTS$Revolutions = as.numeric(as.character(CNTS$Revolutions))
CNTS$`Anti-Government Demonstrations` = as.numeric(as.character(CNTS$`Anti-Government Demonstrations`))

CNTS <- CNTS %>%
  rename("countrycode" = "Country Code", "countryname" = "Country", "year" = "Year", 
         "assassinations" = "Assassinations", "guerilla warfare" = "Guerrilla Warfare", 
         "government crises" = "Government Crises", 
         "purges" = "Purges", "riots" = "Riots", "revolutions" = "Revolutions", 
         "anti-government demonstrations" = "Anti-Government Demonstrations")

CNTS <- CNTS %>% 
 mutate(gwno = countryname)

CNTS$gwno <- (countrycode(CNTS$countryname, origin = 'country.name', destination = 'gwn'))
CNTS$countryname <- as.character(CNTS$countryname)
# fixing ambiguous
CNTS$gwno[CNTS$countryname == "Antigua and Barbuda"] <- 58
CNTS$gwno[CNTS$countryname == "Aruba"] <- "-999" # no GWNO? - ask ben
CNTS$gwno[CNTS$countryname == "Antigua and Barbuda"] <- 58
CNTS$gwno[CNTS$countryname == "Andorra"] <- 232
CNTS$gwno[CNTS$countryname == "Anjouan"] <- "-999" #no GWNO?
CNTS$gwno[CNTS$countryname == "Austria-Hungary"] <- 300
CNTS$gwno[CNTS$countryname == "Baden"] <- 267
CNTS$gwno[CNTS$countryname == "Bavaria"] <- 245
CNTS$gwno[CNTS$countryname == "Bophutswana"] <- "-999" #no GWNO
CNTS$gwno[CNTS$countryname == "Bavaria"] <- 245
CNTS$gwno[CNTS$countryname == "Central African Empire"] <- 482
CNTS$gwno[CNTS$countryname == "Ciskei"] <- "-999" #no gwno
CNTS$gwno[CNTS$countryname == "Dominica"] <- 54
CNTS$gwno[CNTS$countryname == "Federation of Malaya"] <- 820 #malaysia
CNTS$gwno[CNTS$countryname == "German DR"] <- 265
CNTS$gwno[CNTS$countryname == "German FR"] <- 260
CNTS$gwno[CNTS$countryname == "Grand Duchy of Hesse"] <- 275
CNTS$gwno[CNTS$countryname == "Grenada"] <- 55
CNTS$gwno[CNTS$countryname == "Hanover"] <- 240
CNTS$gwno[CNTS$countryname == "Hesse Electorate"] <- 273
CNTS$gwno[CNTS$countryname == "Holy See (Vatican City)"] <- "-999"
CNTS$gwno[CNTS$countryname == "Kiribati"] <- 920
CNTS$gwno[CNTS$countryname == "Liechtenstein"] <- 223
CNTS$gwno[CNTS$countryname == "Marshall Islands"] <- 983
CNTS$gwno[CNTS$countryname == "Mecklenburg"] <- 280
CNTS$gwno[CNTS$countryname == "Micronesia"] <- 987
CNTS$gwno[CNTS$countryname == "Fed States"] <- 987
CNTS$gwno[CNTS$countryname == "Modena"] <- 332
#Monaco, Nauru, Netherlands Antilles, Ottoman Empire, 
CNTS$gwno[CNTS$countryname == "Monaco"] <- 221
CNTS$gwno[CNTS$countryname == "Nauru"] <- 971
CNTS$gwno[CNTS$countryname == "Netherlands Antilles"] <- "-999"
CNTS$gwno[CNTS$countryname == "Ottoman Empire"] <- 640
#Palau, Palestinian Autonomous Areas, Papal States, 
CNTS$gwno[CNTS$countryname == "Palau"] <- 986
CNTS$gwno[CNTS$countryname == "Palestinian Autonomous Areas"] <- "-999"
CNTS$gwno[CNTS$countryname == "Papal States"] <- 327
#Parma, Prussia, Saint Kitts and Nevis, Saint Lucia, 
CNTS$gwno[CNTS$countryname == "Parma"] <- 335
CNTS$gwno[CNTS$countryname == "Prussia"] <- 255
CNTS$gwno[CNTS$countryname == "Saint Kitts and Nevis"] <- 60
CNTS$gwno[CNTS$countryname == "Saint Lucia"] <- 56
#Saint Vincent and the Grenadines, Samoa, San Marino, 
CNTS$gwno[CNTS$countryname == "Saint Vincent and the Grenadines"] <- 57
CNTS$gwno[CNTS$countryname == "Samoa"] <- 990
CNTS$gwno[CNTS$countryname == "San Marino"] <- 331
#Sao Tome and Principe, Sardinia, Saxony, Serbia/Montenegro, 
CNTS$gwno[CNTS$countryname == "Sao Tome and Principe"] <- 403
CNTS$gwno[CNTS$countryname == "Sardinia"] <- 325 #italy
CNTS$gwno[CNTS$countryname == "Saxony"] <- 269
CNTS$gwno[CNTS$countryname == "Serbia/Montenegro"] <- 340
#Seychelles, Somaliland, Tanganyika, Tonga, Transkei, 
CNTS$gwno[CNTS$countryname == "Seychelles"] <- 591
CNTS$gwno[CNTS$countryname == "Somaliland"] <- 520 
CNTS$gwno[CNTS$countryname == "Tanganyika"] <- 510
CNTS$gwno[CNTS$countryname == "Tonga"] <- 971
CNTS$gwno[CNTS$countryname == "Transkei"] <- "-999"
#Tuscany, Tuvalu, Two Sicilies, UAR, Vanuatu, Venda, 
CNTS$gwno[CNTS$countryname == "Tuscany"] <- 337
CNTS$gwno[CNTS$countryname == "Tuvalu"] <- 973 
CNTS$gwno[CNTS$countryname == "Two Sicilies"] <- 329
CNTS$gwno[CNTS$countryname == "UAR"] <- 696
CNTS$gwno[CNTS$countryname == "Vanuatu"] <- 935
CNTS$gwno[CNTS$countryname == "Venda"] <- "-999"
#Western Samoa, Wurttemberg, Yemen, Yemen AR, Yemen Republic
CNTS$gwno[CNTS$countryname == "Western Samoa"] <- 990
CNTS$gwno[CNTS$countryname == "Wurttemberg"] <- "-999"
CNTS$gwno[CNTS$countryname == "Yemen"] <- 678 #Yemen
CNTS$gwno[CNTS$countryname == "Yemen AR"] <- 678 #Yemen
CNTS$gwno[CNTS$countryname == "Yemen Repubic"] <- 678 #Yemen

CNTS$gwno <- as.numeric(CNTS$gwno)

CNTS <- CNTS %>% 
  select(-c('countrycode')) %>%
  relocate('year', 'gwno')

CNTS <- CNTS %>%
  rename("countryname_raw" = "countryname")


#############################################################################################
## INSITUTIONAL STRENGTH PROXIES DATA MANAGEMENT 

CBI <- read_excel("Variables/INSTITUTIONAL STRENGTH PROXIES/CBIData_Romelli2022.xlsx", sheet = 2)

CBI <- CBI %>% 
  mutate(gwno = Country)

CBI$gwno <- (countrycode(CBI$Country, origin = "country.name", destination = "gwn"))

# Fixing ambiguous values within gwno
#Some values were not matched unambiguously: 
  #Anguilla, Antigua and Barbuda, Dominica, Grenada, Macao S.A.R, 
  #Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines, Seychelles, Yemen

CBI$gwno[CBI$Country == "Anguilla"] <- NA
CBI$gwno[CBI$Country == "Antigua and Barbuda"] <- 58
CBI$gwno[CBI$Country == "Dominica"] <- 54
CBI$gwno[CBI$Country == "Grenada"] <- 55
CBI$gwno[CBI$Country == "Macao SAR"] <- 709
CBI$gwno[CBI$Country == "Saint Kitts and Nevis"] <- 60
CBI$gwno[CBI$Country == "Saint Lucia"] <- 56
CBI$gwno[CBI$Country == "St. Vincent and the Grenadines"] <- 57
CBI$gwno[CBI$Country == "Seychelles"] <- 591
CBI$gwno[CBI$Country == "Yemen"] <- 678 #Yemen

# selecting and renaming
CBI <- CBI %>%
  select(-c('iso_a2', 'iso_a3')) %>% #dropping columns
  select("year", "gwno",
         "CBIE", "CBIEBoard", "CBIEPolicy", "CBIEObj", "CBIELending",
         "CBIEFinances", "CBIEReport", "GMT", "LVAU", "LVAW", "CWNE",
         "countryname_raw" = "Country")

#labelling for clarity

#adding suffixes
colnames(CBI)[c(3,4,5,6,7,8,9,10,11,12,13)] <- paste(colnames(CBI)[c(3,4,5,6,7,8,9,10,11,12,13)], 'CBI', sep = '_')

#adding sub-labels
label(CBI$CBIE_CBI) <- "CBIE (Central Bank Independence - Exteded) Index proposed in the Romelli paper [0;1]."
label(CBI$CBIEBoard_CBI) <- "Degree of independence of the 'Governor and central bank board' dimension of the  CBIE Index [0;1]."
label(CBI$CBIEPolicy_CBI) <- "Degree of independence of the 'Monetary policy and conflicts resolution' dimension of the  CBIE Index [0;1]."
label(CBI$CBIEObj_CBI) <- "Degree of independence of the 'Objectives' dimension of the CBIE Index [0;1]."
label(CBI$CBIELending_CBI) <- "Degree of independence of the 'Limitations on lending to the government' dimension of the  CBIE Index [0;1]."
label(CBI$CBIEFinances_CBI) <- "Degree of independence of the 'Financial independence' dimension of the  CBIE Index [0;1]."
label(CBI$CBIEReport_CBI) <- "Degree of independence of the 'Reporting and disclosure' dimension of the  CBIE Index [0;1]."
label(CBI$GMT_CBI) <- "Grilli, Masciandaro and Tabellini (1991) Index of Central Bank Independence [0;1]."
label(CBI$CWNE_CBI) <- "Jácome and Vázquez (2008) Index of Central Bank Independence [0;1]."
label(CBI$LVAU_CBI) <- "Cukierman et al. (1992) Unweighted Index of Central Bank Independence [0;1]."
label(CBI$LVAW_CBI) <- "Cukierman et al. (1992) Weighted Index of Central Bank Independence [0;1]."


#####PROBLEM AREA!!#####################################################################
#################################################################################################
# MERGING

#drop countryname_raw at the start
CNTS <- CNTS %>% 
  select(-c(countryname_raw))

CSPCoups <- CSPCoups %>% 
  select(-c(countryname_raw))

DPI <- DPI %>% 
  select(-c(countryname_raw))

MEPV <- MEPV %>% 
  select(-c(countryname_raw))

CNTSandCSPCoups <- full_join(CNTS,CSPCoups, by = c('gwno', 'year'))

DPIandMEPV <- full_join(DPI, MEPV, by = c('gwno', 'year'))

#BEAR IN MIND: left_join takes obs of first dataframe and match to records in next dataframe - YC 03022023
#CODE BELOW can be problematic: DPIandMEPV has shorter time frame (1975-) so left_join() drops all pre-1975 data in CNTSandCSPCoups (1815-)
merged <- full_join(DPIandMEPV, CNTSandCSPCoups, by = c('gwno', 'year'))

merged <- merged %>%
  select(-c(
    'system', 
    'stabs_strict', 
    'ind')) %>% #dropping columns
  relocate('year', 'gwno')
  #filter(year > 1959) #figure out better span

#merged <- full_join(merged, inflation, by = c('gwno', 'year')) #- creating duplicates? 
#POSSIBLY due to using the name "merged" twice (within full_join function and outside) 
#TRY INSTEAD: 

merged <- full_join(merged,inflation,by=c('gwno','year')) %>% relocate(gwno,year) #YC 03022023 

#adding labels to column names
#label(inflation$`inflation, consumer prices (annual %)`) <- "[WDI]"

label(merged$stabs) <- "Percent of veto players who drop from the government in any given year [DPI]"
colnames(merged)[c(3)] <- paste(colnames(merged)[c(3)], 'DPI', sep = '_')


label(merged$civviol) <- "Magnitude score of episode(s) of civil violence involving that state in that year. Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes [MEPV]"
label(merged$civwar) <- "Magnitude score of episode(s) of civil warfare involving that state in that year. Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes [MEPV]"
label(merged$ethviol) <- "Magnitude score of episode(s) of ethnic violence involving that state in that year. Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes [MEPV]"
label(merged$ethwar) <- "Magnitude score of episode(s) of ethnic warfare involving that state in that year. Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes [MEPV]"
colnames(merged)[c(4,5,6,7)] <- paste(colnames(merged)[c(4,5,6,7)], 'MEPV', sep = '_')


label(merged$assassinations) <- "[CNTS]"
label(merged$`guerilla warfare`) <- "[CNTS]"
label(merged$`government crises`) <- "[CNTS]"
label(merged$purges) <- "[CNTS]"
label(merged$riots) <- "[CNTS]"
label(merged$revolutions) <- "[CNTS]"
label(merged$`anti-government demonstrations`) <- "[CNTS]"
colnames(merged)[c(8,9,10,11,12,13,14)] <- paste(colnames(merged)[c(8,9,10,11,12,13,14)], 'CNTS', sep = '_')

label(merged$scoup1) <- "Successful Coups: Number of successful coups d’état that occurred in the year of record [CSP]"
label(merged$atcoup2) <- "Number of attempted (but ultimately unsuccessful) coups d’état that occurred in the year of record [CSP]"
label(merged$pcoup3) <- "Number of (thwarted) coup plots that were reported by government officials during the year of record [CSP]"
label(merged$reboutex) <- "Ouster of Leadership by Rebel Forces: Indicator of the forced ouster of a ruling executive as a direct result of armed action by rebel forces fighting against forces loyal to the regime (rebel leaders then assume regime leadership often resulting in a political, or socio-political, revolution) [CSP]"
label(merged$assassex) <- "Assassination of Executive: Indicator of the assassination of the ruling executive during the year of record (assassinations are perpetrated by persons acting outside the ruling elite and do not result in a substantive change in regime leadership) [CSP]"
label(merged$resignex) <- "Resignation of Executive Due to Poor Performance and/or Loss of Authority: Indicator of the coerced resignation of the ruling executive due to poor performance and accompanied by increasing public discontent and popular demonstrations calling for the ouster of the executive leadership (including impeachment proceedings which may or may not be successful in forcing executive removal) [CSP]"
colnames(merged)[c(15,16,17,18,19,20)] <- paste(colnames(merged)[c(15,16,17,18,19,20)], 'CSP', sep = '_')

################################
# putting back countrynames
#merged <- merged %>% 
  #mutate(countryname_raw = gwno)

#merged$countryname_raw <- (countrycode(merged$gwno, origin = 'gwn', destination = 'country.name'))
#################################

merged = merged[merged$gwno != 54,] 
merged = merged[merged$gwno != 55,]
merged = merged[merged$gwno != 56,]
merged = merged[merged$gwno != 57,]
merged = merged[merged$gwno != 58,]
merged = merged[merged$gwno != 60,]
merged = merged[merged$gwno != 221,]
merged = merged[merged$gwno != 223,]
merged = merged[merged$gwno != 331,]
merged = merged[merged$gwno != 232,]
merged = merged[merged$gwno != 403,]
merged = merged[merged$gwno != 591,]
merged = merged[merged$gwno != 935,]
merged = merged[merged$gwno != 971,]
merged = merged[merged$gwno != 973,]
merged = merged[merged$gwno != 983,]
merged = merged[merged$gwno != 986,]
merged = merged[merged$gwno != 990,]
merged = merged[merged$gwno != '-999',]


#### CONTROLS

load("Variables/CONTROLS/Graham_Tucker_IPE_v5.RDATA") #comes up as ipe_v5

ipe_controls <- ipe_v5 %>%
  select(c( #"countryname_raw" = "country",
            'gwno', 'year', 
            'gdppc_WDI',
            'growth_WDI',
            'trade_WDI'
  )) %>% #dropping columns
  filter(year > 1959) %>%
  relocate('year', 'gwno')


#Controls: 
  ## Graham, Benjamin A.T.; Tucker, Jacob R., 2016, "The International Political Economy Data Resource", https://doi.org/10.7910/DVN/X093TV, Harvard Dataverse, V5
#Real GDP per capita, -- Global Development Network Growth Database from the World Bank
    #IPE
  
#GDP growth of trading partners, IPE -- Global Development Network Growth Database from the World Bank
  #IPE

#exchange rate regime, and --  Ilzetzki, Reinhart, and Rogoff (2021) -- SPEC!! 
#class_IRK	Exchange Rate Arrangement Classification (1=peg, 2=crawling peg, 3=managed floating, 4=freely floating, 5=freely falling, 6=dual market)
#anchor_IRK	Anchor Currency
#unified_IRK	Unified Market (1=unified, 0=not unified)

#### WDI from IPE (1960-2019)

#gdp_WDI	GDP (constant 2010 US$) [WDI]       
#growth_WDI	GDP growth (annual %) [WDI]       
#gdppc_WDI	GDP per capita (constant 2010 US$) [WDI] 
#reer_WDI	Real effective exchange rate index (2010 = 100) [WDI] 
#trade_WDI	Trade (% of GDP) [WDI] 

## Merge controls in 
merged_controls <- full_join(merged,ipe_controls,by=c('gwno','year')) %>% 
  relocate(gwno,year) %>% relocate(countryname_raw, .after =last_col())

#logs of controls 
merged_controls$log_gdppc_WDI = 
  log10(merged_controls$'gdppc_WDI')

merged_controls$log_growth_WDI = 
  log10(merged_controls$'growth_WDI')

merged_controls$log_trade_WDI = 
  log10(merged_controls$'trade_WDI')

#dummy variables for inflation 

merged_controls$inflation_rate_over_10percent <- 
  ifelse(merged_controls$`inflation, consumer prices (annual %)` >= 10, 1, 0)

merged_controls$inflation_rate_over_20percent <- 
  ifelse(merged_controls$`inflation, consumer prices (annual %)` >= 20, 1, 0)

merged_controls$inflation_rate_over_50percent <- 
  ifelse(merged_controls$`inflation, consumer prices (annual %)` >= 50, 1, 0)


# making logarithms for inflation

## if values are equal to 10 or less than replace with 10 
merged_controls <- merged_controls %>% 
  mutate(inflation_rate_10min = 'inflation, consumer prices (annual %)')
merged_controls$inflation_rate_10min <- 
  ifelse(merged_controls$'inflation, consumer prices (annual %)' <= 10 | is.na(merged_controls$'inflation, consumer prices (annual %)'), 10, merged_controls$'inflation, consumer prices (annual %)')

#making log
merged_controls$log_inflation = 
  log10(merged_controls$'inflation_rate_10min')

#rename variables controls 
merged_controls <- merged_controls %>%
  rename("anti_government_demonstrations_CNTS" = "anti-government demonstrations_CNTS",
         "guerilla_warfare_CNTS" = "guerilla warfare_CNTS",
         "government_crises_CNTS" = "government crises_CNTS")

# limit timespan of database (1960-2010)
  #MEPV finishes in 2012

merged_controls <- merged_controls %>% 
  subset(year > 1959 & year < 2011) 

#Save as RDATA
saveRDS(merged_controls, file = 'thesis_variables.rds')

# dataset with CBI (institutional strenght proxies) and smaller time period (1972-2010)
# just CBIE - proposed in Romelli paper 

CBI <- CBI %>%
  select(-c('countryname_raw'))

merged_controls_with_CBI <- full_join(merged_controls, CBI, by = c('gwno', 'year'))

merged_controls_with_CBI <- merged_controls_with_CBI %>% 
  select(-c('CBIEBoard_CBI', 'CBIEPolicy_CBI', 'CBIEObj_CBI', 'CBIELending_CBI',
            'CBIELending_CBI', 'CBIEFinances_CBI', 'CBIEReport_CBI', 
            'GMT_CBI', 'LVAU_CBI', 'LVAW_CBI', 'CWNE_CBI')) %>%
  subset(year > 1971 & year < 2011 )

saveRDS(merged_controls_with_CBI, file = 'thesis_variables_inst_prox.rds')








