#/**************************
# Thesis Data Regression Prep
# 
#
# Date: Feb/March 2023
# By: Valeria Flores-Cadena
# 
# 
## ****************************

#clear everything
rm(list=ls())

# install package
# install.packages("peacesciencer")
# install.packages("countrycode")
# install.packages("stargazer")
# install.packages("aod")
# install.packages("sandwich")
# install.packages("msm")
# install.packages("texreg")

# load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(peacesciencer)
library(Hmisc)
library(stargazer)
library(ggplot2)
library(aod)
library(texreg)
library(sandwich)
library(msm)

# Setting Working Directory
setwd("/Volumes/GoogleDrive-115054236824584854554/My Drive/Senior Thesis")
getwd()

## Using Data Management RDS files 
thesis_variables <- readRDS(file = 'thesis_variables.rds')
thesis_variables_interactionterm <- readRDS(file = 'thesis_variables_inst_prox.rds')

DES_thesis_variables_interactionterm <- thesis_variables_interactionterm %>%
  select(-c("gwno", "year", "inflation_rate_over_50percent", "inflation_rate_10min", "log_gdppc_WDI",
            "log_growth_WDI", "log_trade_WDI", "inflation, consumer prices (annual %)", 
            "countryname_raw"))

## TABLE 1. Descriptive Statistics Table
labels_for_descriptive_stats <- c('Dropped Veto Players',
                                  "Magnitude of civil violence",
                                  "Magnitude of civil warfare",
                                  "Magnitude of ethnic violence",
                                  "Magnitude of ethnic warfare",
                                  "Assassinations",
                                  "Guerilla Warfare",
                                  "Government Crises",
                                  "Purges",
                                  "Riots",
                                  "Revolutions",
                                  "Anti-government Demonstrations",
                                  "Successful Coups", 
                                  "Attempted Coups",
                                  "Thwarted Coup Plots",
                                  "Ousted Leadership by Rebel Forces",
                                  "Assassination of Executive",
                                  "Resignation of Executive", 
                                  "GDPpc",
                                  "Growth",
                                  "Trade",
                                  "Binary Inflation for 10 percent",
                                  "Binary Inflation for 20 percent",
                                  "ln(inflation)",
                                  "CBIE index")

stargazer(DES_thesis_variables_interactionterm, 
          type = "html", 
          title = "Descriptive Statistics for Variables", 
          digits = 1, 
          covariate.labels = labels_for_descriptive_stats,
          out = "/Users/valeriaflores-cadena/Desktop/Descriptive_statistics_ALL.html")

# FIG 1 in Appendix. inflation rate raw values histogram
  ggplot(data = thesis_variables, aes(x = thesis_variables$'inflation, consumer prices (annual %)')) +
  geom_histogram(aes(y=..density..), color = "black", fill = "white") + 
  geom_density(alpha=.2, color = "blue", fill="#56B4E9")  +
  labs(x="Inflation (%)", y = "Density")+
  xlim(-15,50) 

#FIG 2 in Appendix. inflation rate log histogram
ggplot(data = thesis_variables, aes(x = thesis_variables$'log_inflation')) +
  geom_histogram(aes(y=..density..), color = "black", fill = "white") + 
  geom_density(alpha=.2, color = "blue", fill="#56B4E9")  +
  labs(x="ln(Inflation)", y = "Density") + 
  xlim(0.8,2) 


#FIG 3 in Appendix. Correlation of Variables

thesis_variables_corr <- DES_thesis_variables_interactionterm 
thesis_variables_corr <- thesis_variables_corr %>%
  rename(
  "Dropped Veto Players" = stabs_DPI,
  "Magnitude of Civil Violence" = civviol_MEPV,
  "Magnitude of Civil Warfare" = civwar_MEPV,
  "Magnitude of Ethnic Violence" = ethviol_MEPV,
  "Magnitude of Ethnic Warfare" = ethwar_MEPV,
  "Assassinations" = assassinations_CNTS,
  "Guerilla Warfare" = guerilla_warfare_CNTS,
  "Government Crises" = government_crises_CNTS,
  "Purges" = purges_CNTS,
  "Riots" = riots_CNTS,
  "Revolutions" = revolutions_CNTS,
  "Anti-government Demonsrations" = anti_government_demonstrations_CNTS,
  "Successful Coups" = scoup1_CSP,
  "Attempted Coups" = atcoup2_CSP,
  "Thwarted Coup Plots" = pcoup3_CSP,
  "Ousted Leadership by Rebel Forces" = reboutex_CSP,
  "Assassination of Executive" = assassex_CSP,
  "Resignation of Executive" = resignex_CSP,
  "GDPpc" = gdppc_WDI,
  "Growth" = growth_WDI,
  "Trade" = trade_WDI,
  "Binary Inflation for 10 percent" = inflation_rate_over_10percent,
  "Binary Inflation for 20 percent" = inflation_rate_over_20percent,
  "ln(Inflation)" = log_inflation,
  "CBIE index" = CBIE_CBI)

#install.packages("corrplot")
library(corrplot)
res <- cor(na.omit(thesis_variables_corr))
round(res,2)
#corrplot(res, type = "upper", order = "hclust",
         #tl.col = "black", tl.srt = 45,)
corrplot(res, method = 'ellipse', order = 'AOE', type = 'upper', 
         tl.cex = 0.5,
         col = COL2('RdBu', 10), 
         tl.col = "black", 
         tl.srt = 45)

######################################################################################################
# Code below is for Tables 1,2,3 in Main Paper
#H1: Does inflation serve as an indicator of political instability? 
######################################################################################################
# DV: CSP, count data
# SCOUP1 Successful Coups: Number of successful coups d’état that occurred in the year of record

reg_SCOUP1_loginflation <- glm(scoup1_CSP ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_SCOUP1_inflation_dummy10 <- glm(scoup1_CSP ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_SCOUP1_inflation_dummy20 <- glm(scoup1_CSP ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

# ATCOUP2 Attempted Coups: Number of attempted (but ultimately unsuccessful) coups d’état that occurred in the year of record

reg_ATCOUP2_loginflation <- glm(atcoup2_CSP ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_ATCOUP2_inflation_dummy10 <- glm(atcoup2_CSP ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_ATCOUP2_inflation_dummy20 <- glm(atcoup2_CSP ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

# PCOUP3 Coup Plots: Number of (thwarted) coup plots that were reported by government officials during the year of record

reg_PCOUP3_loginflation <- glm(pcoup3_CSP ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_PCOUP3_inflation_dummy10 <- glm(pcoup3_CSP ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_PCOUP3_inflation_dummy20 <- glm(pcoup3_CSP ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

# REBOUTEX Ouster of Leadership by Rebel Forces: Indicator of the forced ouster of a ruling executive as a direct result of armed action by rebel forces fighting
  # against forces loyal to the regime (rebel leaders then assume regime leadership often resulting in a political, or socio-political, revolution)

reg_REBOUTEX_loginflation <- glm(reboutex_CSP ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_REBOUTEX_inflation_dummy10 <- glm(reboutex_CSP ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_REBOUTEX_inflation_dummy20 <- glm(reboutex_CSP ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

# ASSASSEX Assassination of Executive: Indicator of the assassination of the ruling executive during the year of record (assassinations are perpetrated by
  # persons acting outside the ruling elite and do not result in a substantive change in regime leadership)

reg_ASSASSEX_loginflation <- glm(assassex_CSP ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_ASSASSEX_inflation_dummy10 <- glm(assassex_CSP ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_ASSASSEX_inflation_dummy20 <- glm(assassex_CSP ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

# RESIGNEX Resignation of Executive Due to Poor Performance and/or Loss of Authority: Indicator of the coerced resignation of the ruling executive due to poor
  # performance and accompanied by increasing public discontent and popular demonstrations calling for the ouster of the executive leadership (including
  # impeachment proceedings which may or may not be successful in forcing executive removal); like assassinations, coerced resignations of an executive
  # do not result in a substantive change in regime leadership (although they may lead to new elections)

reg_RESIGNEX_loginflation <- glm(resignex_CSP ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_RESIGNEX_inflation_dummy10 <- glm(resignex_CSP ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_RESIGNEX_inflation_dummy20 <- glm(resignex_CSP ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

######################################################################################################
# DV: MEPV, index data (magnitude score) - linear
# CIVVIOL (2-numeric) Magnitude score of episode(s) of civil violence involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_CIVVIOL_loginflation <- lm(civviol_MEPV ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_CIVVIOL_inflation_dummy10 <- lm(civviol_MEPV ~ inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_CIVVIOL_inflation_dummy20 <- lm(civviol_MEPV ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)

# CIVWAR (2-numeric) Magnitude score of episode(s) of civil warfare involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_CIVWAR_loginflation <- lm(civwar_MEPV ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_CIVWAR_inflation_dummy10 <- lm(civwar_MEPV ~ inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_CIVWAR_inflation_dummy20 <- lm(civwar_MEPV ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)

# ETHVIOL (2-numeric) Magnitude score of episode(s) of ethnic violence involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_ETHVIOL_loginflation <- lm(ethviol_MEPV ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_ETHVIOL_inflation_dummy10 <- lm(ethviol_MEPV ~ inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_ETHVIOL_inflation_dummy20 <- lm(ethviol_MEPV~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)

# ETHWAR (2-numeric) Magnitude score of episode(s) of ethnic warfare involving that state in that year
  # Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_ETHWAR_loginflation <- lm(ethwar_MEPV ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_ETHWAR_inflation_dummy10 <- lm(ethwar_MEPV ~ inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_ETHWAR_inflation_dummy20 <- lm(ethwar_MEPV~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)

######################################################################################################
# DV: STABS - continuous data, linear regression
  # These variables count the percent of veto players who drop from the government in any given year. Veto
  # players are defined as in CHECKS.

reg_STABS_loginflation <- lm(stabs_DPI ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_STABS_inflation_dummy10 <- lm(stabs_DPI ~ inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)
reg_STABS_inflation_dummy20 <- lm(stabs_DPI ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables)


######################################################################################################
# DV: CNTS, count data

## assassinations

reg_assassinations_loginflation <- glm(assassinations_CNTS ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_assassinations_inflation_dummy10 <- glm(assassinations_CNTS  ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_assassinations_inflation_dummy20 <- glm(assassinations_CNTS  ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

## guerilla warfare

reg_guerilla_warfare_loginflation <- glm(guerilla_warfare_CNTS ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_guerilla_warfare_inflation_dummy10 <- glm(guerilla_warfare_CNTS  ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_guerilla_warfare_inflation_dummy20 <- glm(guerilla_warfare_CNTS  ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

## government crises

reg_government_crises_loginflation <- glm(government_crises_CNTS ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_government_crises_inflation_dummy10 <- glm(government_crises_CNTS  ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_government_crises_inflation_dummy20 <- glm(government_crises_CNTS  ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

## purges 

reg_purges_loginflation <- glm(purges_CNTS ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_purges_inflation_dummy10 <- glm(purges_CNTS  ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_purges_inflation_dummy20 <- glm(purges_CNTS  ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

## riots 

reg_riots_loginflation <- glm(riots_CNTS ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_riots_inflation_dummy10 <- glm(riots_CNTS  ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_riots_inflation_dummy20 <- glm(riots_CNTS  ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

## revolutions

reg_revolutions_loginflation <- glm(revolutions_CNTS ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_revolutions_inflation_dummy10 <- glm(revolutions_CNTS  ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_revolutions_inflation_dummy20 <- glm(revolutions_CNTS  ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))

## anti-government demonstrations
reg_antigovdem_loginflation <- glm(anti_government_demonstrations_CNTS ~ log_inflation + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_antigovdem_inflation_dummy10 <- glm(anti_government_demonstrations_CNTS  ~  inflation_rate_over_10percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))
reg_antigovdem_inflation_dummy20 <- glm(anti_government_demonstrations_CNTS  ~ inflation_rate_over_20percent + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables, family = poisson(link = "log"))


# Table 2: The effect of the rate of inflation on political instability
htmlreg(list(reg_ASSASSEX_loginflation, 
             reg_assassinations_loginflation, 
             reg_ATCOUP2_loginflation, 
             reg_CIVVIOL_loginflation, 
             reg_CIVWAR_loginflation, 
             reg_ETHVIOL_loginflation, 
             reg_ETHWAR_loginflation,
             reg_government_crises_loginflation,
             reg_guerilla_warfare_loginflation,
             reg_PCOUP3_loginflation,
             reg_purges_loginflation,
             reg_REBOUTEX_loginflation,
             reg_RESIGNEX_loginflation,
             reg_revolutions_loginflation,
             reg_riots_loginflation,
             reg_SCOUP1_loginflation,
             reg_STABS_loginflation,
             reg_antigovdem_loginflation),
        file = "/Users/valeriaflores-cadena/Desktop/H1_politicalinstability_loginflation.html",
        custom.header = list("Assassination of Executive" = 1, 
                             "Assassinations" = 2,
                             "Attempted Coups" = 3,
                             "Magnitude score of episode(s) of civil violence" = 4, 
                             "Magnitude score of episode(s) of civil warfare" = 5,
                             "Magnitude score of episode(s) of ethnic violence" = 6,
                             "Magnitude score of episode(s) of ethnic warfare" = 7,
                             "Government Crises" = 8,
                             "Guerilla Warfare" = 9, 
                             "Thwarted Coup Plots" = 10, 
                             "Purges" = 11, 
                             "Ousted Leadership by Rebel Forces" = 12, 
                             "Resignation of Executive" = 13, 
                             "Revolutions" = 14, 
                             "Riots" = 15, 
                             "Successful Coups" = 16, 
                             "Dropped Veto Players" = 17,
                             "Anti-Government Demonstrations" = 18),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", "ln(inflation)", "GDPpc", "Growth", "Trade"),
        custom.note = "%stars. Values for Assassination of Executive, Resignations of Executive, Ousted Leadership by Rebel Forces, Successful Coups, Attempted Coups are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number of Assassinations, Anti-Government Demonstrations, Guerilla Warfare, Government Crises, Purges, Riots, and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence, civil warfare, ethnic violence, and ethnic warfare are from Systemic Peace.Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators.")


# Table 3. The effect of the presence of inflation greater than 10% on political instability
htmlreg(list(reg_ASSASSEX_inflation_dummy10, 
             reg_assassinations_inflation_dummy10, 
             reg_ATCOUP2_inflation_dummy10, 
             reg_CIVVIOL_inflation_dummy10, 
             reg_CIVWAR_inflation_dummy10, 
             reg_ETHVIOL_inflation_dummy10, 
             reg_ETHWAR_inflation_dummy10,
             reg_government_crises_inflation_dummy10,
             reg_guerilla_warfare_inflation_dummy10,
             reg_PCOUP3_inflation_dummy10,
             reg_purges_inflation_dummy10,
             reg_REBOUTEX_inflation_dummy10,
             reg_RESIGNEX_inflation_dummy10,
             reg_revolutions_inflation_dummy10,
             reg_riots_inflation_dummy10,
             reg_SCOUP1_inflation_dummy10,
             reg_STABS_inflation_dummy10,
             reg_antigovdem_inflation_dummy10),
        file = "/Users/valeriaflores-cadena/Desktop/H1_politicalinstability_inflationdummy10.html",
        custom.header = list("Assassination of Executive" = 1, 
                             "Assassinations" = 2,
                             "Attempted Coups" = 3,
                             "Magnitude score of episode(s) of civil violence" = 4, 
                             "Magnitude score of episode(s) of civil warfare" = 5,
                             "Magnitude score of episode(s) of ethnic violence" = 6,
                             "Magnitude score of episode(s) of ethnic warfare" = 7,
                             "Government Crises" = 8,
                             "Guerilla Warfare" = 9, 
                             "Thwarted Coup Plots" = 10, 
                             "Purges" = 11, 
                             "Ousted Leadership by Rebel Forces" = 12, 
                             "Resignation of Executive" = 13, 
                             "Revolutions" = 14, 
                             "Riots" = 15, 
                             "Successful Coups" = 16, 
                             "Dropped Veto Players" = 17,
                             "Anti-Government Demonstrations" = 18),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", "dummy variable for inflation over 10 percent", "GDPpc", "Growth", "Trade"),
        custom.note = "%stars. Values for Assassination of Executive, Resignations of Executive, Ousted Leadership by Rebel Forces, Successful Coups, Attempted Coups are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number of Assassinations, Anti-Government Demonstrations, Guerilla Warfare, Government Crises, Purges, Riots, and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence, civil warfare, ethnic violence, and ethnic warfare are from Systemic Peace.Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators.")


# Table 4. The effect of the presence of inflation greater than 20% on political instability

htmlreg(list(reg_ASSASSEX_inflation_dummy20, 
             reg_assassinations_inflation_dummy20, 
             reg_ATCOUP2_inflation_dummy20, 
             reg_CIVVIOL_inflation_dummy20, 
             reg_CIVWAR_inflation_dummy20, 
             reg_ETHVIOL_inflation_dummy20, 
             reg_ETHWAR_inflation_dummy20,
             reg_government_crises_inflation_dummy20,
             reg_guerilla_warfare_inflation_dummy20,
             reg_PCOUP3_inflation_dummy20,
             reg_purges_inflation_dummy20,
             reg_REBOUTEX_inflation_dummy20,
             reg_RESIGNEX_inflation_dummy20,
             reg_revolutions_inflation_dummy20,
             reg_riots_inflation_dummy20,
             reg_SCOUP1_inflation_dummy20,
             reg_STABS_inflation_dummy20,
             reg_antigovdem_inflation_dummy20),
        file = "/Users/valeriaflores-cadena/Desktop/H1_politicalinstability_inflationdummy20.html",
        custom.header = list("Assassination of Executive" = 1, 
                             "Assassinations" = 2,
                             "Attempted Coups" = 3,
                             "Magnitude score of episode(s) of civil violence" = 4, 
                             "Magnitude score of episode(s) of civil warfare" = 5,
                             "Magnitude score of episode(s) of ethnic violence" = 6,
                             "Magnitude score of episode(s) of ethnic warfare" = 7,
                             "Government Crises" = 8,
                             "Guerilla Warfare" = 9, 
                             "Thwarted Coup Plots" = 10, 
                             "Purges" = 11, 
                             "Ousted Leadership by Rebel Forces" = 12, 
                             "Resignation of Executive" = 13, 
                             "Revolutions" = 14, 
                             "Riots" = 15, 
                             "Successful Coups" = 16, 
                             "Dropped Veto Players" = 17,
                             "Anti-Government Demonstrations" = 18),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", "dummy variable for inflation over 20 percent", "GDPpc", "Growth", "Trade"),
        custom.note = "%stars. Values for Assassination of Executive, Resignations of Executive, Ousted Leadership by Rebel Forces, Successful Coups, Attempted Coups are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number of Assassinations, Anti-Government Demonstrations, Guerilla Warfare, Government Crises, Purges, Riots, and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence, civil warfare, ethnic violence, and ethnic warfare are from Systemic Peace.Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators.")

######################################################################################################
# Code below is for Tables 5,6,7 in Main Paper
#H2: Political instability increases the probability of hyperinflation in the following year.
######################################################################################################

thesis_variables_lead <- thesis_variables %>% 
  group_by(gwno) %>%
  dplyr::mutate(log_inflation_lead = dplyr::lead(log_inflation, n=1)) %>%
  dplyr::mutate('TEN_dummy_inflation_lead' = dplyr::lead(inflation_rate_over_10percent, n=1)) %>%
  dplyr::mutate('TWENTY_dummy_inflation_lead' = dplyr::lead(inflation_rate_over_10percent, n=1)) %>%
  as.data.frame()

thesis_variables_lead <- thesis_variables_lead %>% 
  select(-c(countryname_raw,
            'inflation, consumer prices (annual %)',
            inflation_rate_over_50percent,
            inflation_rate_10min,
            log_gdppc_WDI, log_growth_WDI, log_trade_WDI))

## DV: inflation in year t 
## IV: political instability year(t-1)

reg_inflation_log_lead_stabs <- lm(log_inflation_lead ~ stabs_DPI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_log_lead_civviol <- lm(log_inflation_lead ~ civviol_MEPV + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_log_lead_assassex <- lm(log_inflation_lead ~ assassex_CSP + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_log_lead_reboutex <- lm(log_inflation_lead ~ reboutex_CSP + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_log_lead_riots <- lm(log_inflation_lead ~ riots_CNTS + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_log_lead_antigovdem <- lm(log_inflation_lead ~ anti_government_demonstrations_CNTS + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

# Table 5. Effects of inflation rate on lagged political instability variables
htmlreg(list(reg_inflation_log_lead_stabs,
             reg_inflation_log_lead_civviol,
             reg_inflation_log_lead_assassex,
             reg_inflation_log_lead_reboutex,
             reg_inflation_log_lead_riots,
             reg_inflation_log_lead_antigovdem),
        file = "/Users/valeriaflores-cadena/Desktop/H2_log_lead_new.html",
        custom.header = list("ln(inflation rate)" = 1:6),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", 
                              "Dropped Veto Players",
                              "GDPpc",
                              "Growth",
                              "Trade",
                              "Magnitude score of episode(s) of civil violence", 
                              "Assassination of Executive",
                              "Resignation of Executive",
                              "Riots",
                              "Anti-Government Demonstrations"),
        custom.note = ("%stars. Inflation rates are in year t while political instability variables are in t-1.
        Values for Assassination of Executive and Resignations of Executive are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number Riots, Anti-Government Demonstrations and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence from Systemic Peace.
        Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators."))


reg_inflation_10dummy_lead_stabs <- glm(TEN_dummy_inflation_lead ~ stabs_DPI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_10dummy_lead_civviol <- glm(TEN_dummy_inflation_lead ~ civviol_MEPV + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_10dummy_lead_assassex <- glm(TEN_dummy_inflation_lead ~ assassex_CSP + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_10dummy_lead_reboutex <- glm(TEN_dummy_inflation_lead ~ reboutex_CSP + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_10dummy_lead_riots <- glm(TEN_dummy_inflation_lead ~ riots_CNTS + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_10dummy_lead_antigovdem <- glm(TEN_dummy_inflation_lead ~ anti_government_demonstrations_CNTS + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

# Table 6. Effects of inflation rate on lagged political instability variables
htmlreg(list(reg_inflation_10dummy_lead_stabs,
             reg_inflation_10dummy_lead_civviol,
             reg_inflation_10dummy_lead_assassex,
             reg_inflation_10dummy_lead_reboutex,
             reg_inflation_10dummy_lead_riots,
             reg_inflation_10dummy_lead_antigovdem),
        file = "/Users/valeriaflores-cadena/Desktop/H2_10dummy_lead_new.html",
        custom.header = list("Binary Inflation for 10%" = 1:6),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", 
                              "Dropped Veto Players",
                              "GDPpc",
                              "Growth",
                              "Trade",
                              "Magnitude score of episode(s) of civil violence", 
                              "Assassination of Executive",
                              "Resignation of Executive",
                              "Riots",
                              "Anti-Government Demonstrations"),
        custom.note = ("%stars. Inflation rates are in year t while political instability variables are in t-1.
        Values for Assassination of Executive and Resignations of Executive are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number Riots, Anti-Government Demonstrations and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence from Systemic Peace.
        Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators."))


reg_inflation_20dummy_lead_stabs <- glm(TWENTY_dummy_inflation_lead ~ stabs_DPI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_20dummy_lead_civviol <- glm(TWENTY_dummy_inflation_lead ~ civviol_MEPV + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_20dummy_lead_assassex <- glm(TWENTY_dummy_inflation_lead ~ assassex_CSP + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_20dummy_lead_reboutex <- glm(TWENTY_dummy_inflation_lead ~ reboutex_CSP + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_20dummy_lead_riots <- glm(TWENTY_dummy_inflation_lead ~ riots_CNTS + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)

reg_inflation_20dummy_lead_antigovdem <- glm(TWENTY_dummy_inflation_lead ~ anti_government_demonstrations_CNTS + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_lead)


# Table 7. Effects of inflation rate on lagged political instability variables
htmlreg(list(reg_inflation_20dummy_lead_stabs,
             reg_inflation_20dummy_lead_civviol,
             reg_inflation_20dummy_lead_assassex,
             reg_inflation_20dummy_lead_reboutex,
             reg_inflation_20dummy_lead_riots,
             reg_inflation_20dummy_lead_antigovdem),
        file = "/Users/valeriaflores-cadena/Desktop/H2_20dummy_lead_new.html",
        custom.header = list("Binary Inflation for 20%" = 1:6),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", 
                              "Dropped Veto Players",
                              "GDPpc",
                              "Growth",
                              "Trade",
                              "Magnitude score of episode(s) of civil violence", 
                              "Assassination of Executive",
                              "Resignation of Executive",
                              "Riots",
                              "Anti-Government Demonstrations"),
        custom.note = ("%stars. Inflation rates are in year t while political instability variables are in t-1.
        Values for Assassination of Executive and Resignations of Executive are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number Riots, Anti-Government Demonstrations and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence from Systemic Peace.
        Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators."))


######################################################################################################
# H3:The effect of inflation on political instability is stronger in countries with lower levels of central bank independence.
  # interaction variable
  ## INSTITUTIONAL STRENGTH PROXIES - CBI index! 
  # data = thesis_variables_interactionterm 
######################################################################################################
# DV: CSP count data
# SCOUP1 Successful Coups: Number of successful coups d’état that occurred in the year of record

reg_SCOUP1_loginflation_inter <- glm(scoup1_CSP ~ log_inflation * CBIE_CBI  + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_SCOUP1_inflation_dummy10_inter <- glm(scoup1_CSP ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_SCOUP1_inflation_dummy20_inter <- glm(scoup1_CSP ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


# ATCOUP2 Attempted Coups: Number of attempted (but ultimately unsuccessful) coups d’état that occurred in the year of record

reg_ATCOUP2_loginflation_inter <- glm(atcoup2_CSP ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_ATCOUP2_inflation_dummy10_inter <- glm(atcoup2_CSP ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_ATCOUP2_inflation_dummy20_inter <- glm(atcoup2_CSP ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


# PCOUP3 Coup Plots: Number of (thwarted) coup plots that were reported by government officials during the year of record

reg_PCOUP3_loginflation_inter <- glm(pcoup3_CSP ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_PCOUP3_inflation_dummy10_inter <- glm(pcoup3_CSP ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_PCOUP3_inflation_dummy20_inter <- glm(pcoup3_CSP ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


# REBOUTEX Ouster of Leadership by Rebel Forces: Indicator of the forced ouster of a ruling executive as a direct result of armed action by rebel forces fighting
# against forces loyal to the regime (rebel leaders then assume regime leadership often resulting in a political, or socio-political, revolution)

reg_REBOUTEX_loginflation_inter <- glm(reboutex_CSP ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_REBOUTEX_inflation_dummy10_inter <- glm(reboutex_CSP ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_REBOUTEX_inflation_dummy20_inter <- glm(reboutex_CSP ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))

## received warning message: glm.fit: fitted rates numerically 0 occurred 

# ASSASSEX Assassination of Executive: Indicator of the assassination of the ruling executive during the year of record (assassinations are perpetrated by
# persons acting outside the ruling elite and do not result in a substantive change in regime leadership)

reg_ASSASSEX_loginflation_inter <- glm(assassex_CSP ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_ASSASSEX_inflation_dummy10_inter <- glm(assassex_CSP ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_ASSASSEX_inflation_dummy20_inter <- glm(assassex_CSP ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


# RESIGNEX Resignation of Executive Due to Poor Performance and/or Loss of Authority: Indicator of the coerced resignation of the ruling executive due to poor
# performance and accompanied by increasing public discontent and popular demonstrations calling for the ouster of the executive leadership (including
# impeachment proceedings which may or may not be successful in forcing executive removal); like assassinations, coerced resignations of an executive
# do not result in a substantive change in regime leadership (although they may lead to new elections)

reg_RESIGNEX_loginflation_inter <- glm(resignex_CSP ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_RESIGNEX_inflation_dummy10_inter <- glm(resignex_CSP ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_RESIGNEX_inflation_dummy20_inter <- glm(resignex_CSP ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


######################################################################################################
# DV: MEPV, index data (magnitude score) - linear
# CIVVIOL (2-numeric) Magnitude score of episode(s) of civil violence involving that state in that year
# Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_CIVVIOL_loginflation_inter <- lm(civviol_MEPV ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_CIVVIOL_inflation_dummy10_inter <- lm(civviol_MEPV ~ inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_CIVVIOL_inflation_dummy20_inter <- lm(civviol_MEPV ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)


# CIVWAR (2-numeric) Magnitude score of episode(s) of civil warfare involving that state in that year
# Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_CIVWAR_loginflation_inter <- lm(civwar_MEPV ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_CIVWAR_inflation_dummy10_inter <- lm(civwar_MEPV ~ inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_CIVWAR_inflation_dummy20_inter <- lm(civwar_MEPV ~ inflation_rate_over_20percent* CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)

# ETHVIOL (2-numeric) Magnitude score of episode(s) of ethnic violence involving that state in that year
# Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_ETHVIOL_loginflation_inter <- lm(ethviol_MEPV ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_ETHVIOL_inflation_dummy10_inter <- lm(ethviol_MEPV ~ inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_ETHVIOL_inflation_dummy20_inter <- lm(ethviol_MEPV~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)


# ETHWAR (2-numeric) Magnitude score of episode(s) of ethnic warfare involving that state in that year
# Scale: 1 (lowest) to 10 (highest) for each MEPV; Magnitude scores for multiple MEPV are summed; 0 denotes no episodes

reg_ETHWAR_loginflation_inter <- lm(ethwar_MEPV ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_ETHWAR_inflation_dummy10_inter <- lm(ethwar_MEPV ~ inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_ETHWAR_inflation_dummy20_inter<- lm(ethwar_MEPV~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)


######################################################################################################
# DV: STABS - continuous data, linear regression
# These variables count the percent of veto players who drop from the government in any given year. Veto
# players are defined as in CHECKS.

reg_STABS_loginflation_inter <- lm(stabs_DPI ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_STABS_inflation_dummy10_inter <- lm(stabs_DPI ~ inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)
reg_STABS_inflation_dummy20_inter <- lm(stabs_DPI ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm)


######################################################################################################
# DV: CNTS, count data

## assassinations

reg_assassinations_loginflation_inter <- glm(assassinations_CNTS ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_assassinations_inflation_dummy10_inter <- glm(assassinations_CNTS  ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_assassinations_inflation_dummy20_inter <- glm(assassinations_CNTS  ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


## guerilla warfare

reg_guerilla_warfare_loginflation_inter <- glm(guerilla_warfare_CNTS ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_guerilla_warfare_inflation_dummy10_inter <- glm(guerilla_warfare_CNTS  ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_guerilla_warfare_inflation_dummy20_inter <- glm(guerilla_warfare_CNTS  ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


## government crises

reg_government_crises_loginflation_inter <- glm(government_crises_CNTS ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_government_crises_inflation_dummy10_inter <- glm(government_crises_CNTS  ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_government_crises_inflation_dummy20_inter <- glm(government_crises_CNTS  ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


## purges 

reg_purges_loginflation_inter <- glm(purges_CNTS ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_purges_inflation_dummy10_inter <- glm(purges_CNTS  ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_purges_inflation_dummy20_inter <- glm(purges_CNTS  ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))

## riots 

reg_riots_loginflation_inter <- glm(riots_CNTS ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_riots_inflation_dummy10_inter <- glm(riots_CNTS  ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_riots_inflation_dummy20_inter <- glm(riots_CNTS  ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))

## revolutions

reg_revolutions_loginflation_inter <- glm(revolutions_CNTS ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_revolutions_inflation_dummy10_inter <- glm(revolutions_CNTS  ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_revolutions_inflation_dummy20_inter <- glm(revolutions_CNTS  ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))

## antigovernment demonstrations

reg_antigovdem_loginflation_inter <- glm(anti_government_demonstrations_CNTS ~ log_inflation * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_antigovdem_dummy10_inter <- glm(anti_government_demonstrations_CNTS  ~  inflation_rate_over_10percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))
reg_antigovdem_dummy20_inter <- glm(anti_government_demonstrations_CNTS  ~ inflation_rate_over_20percent * CBIE_CBI + gdppc_WDI + growth_WDI + trade_WDI, data = thesis_variables_interactionterm, family = poisson(link = "log"))


# Table 8. The interaction effect of the rate of inflation and CBIE on political instability 
htmlreg(list(reg_ASSASSEX_loginflation_inter, 
             reg_assassinations_loginflation_inter, 
             reg_ATCOUP2_loginflation_inter, 
             reg_CIVVIOL_loginflation_inter, 
             reg_CIVWAR_loginflation_inter, 
             reg_ETHVIOL_loginflation_inter, 
             reg_ETHWAR_loginflation_inter,
             reg_government_crises_loginflation_inter,
             reg_guerilla_warfare_loginflation_inter,
             reg_PCOUP3_loginflation_inter,
             reg_purges_loginflation_inter,
             reg_REBOUTEX_loginflation_inter,
             reg_RESIGNEX_loginflation_inter,
             reg_revolutions_loginflation_inter,
             reg_riots_loginflation_inter,
             reg_SCOUP1_loginflation_inter,
             reg_STABS_loginflation_inter,
             reg_antigovdem_loginflation_inter),
        file = "/Users/valeriaflores-cadena/Desktop/H3_politicalinstability_loginflation_inter.html",
        custom.header = list("Assassination of Executive" = 1, 
                             "Assassinations" = 2,
                             "Attempted Coups" = 3,
                             "Magnitude score of episode(s) of civil violence" = 4, 
                             "Magnitude score of episode(s) of civil warfare" = 5,
                             "Magnitude score of episode(s) of ethnic violence" = 6,
                             "Magnitude score of episode(s) of ethnic warfare" = 7,
                             "Government Crises" = 8,
                             "Guerilla Warfare" = 9, 
                             "Thwarted Coup Plots" = 10, 
                             "Purges" = 11, 
                             "Ousted Leadership by Rebel Forces" = 12, 
                             "Resignation of Executive" = 13, 
                             "Revolutions" = 14, 
                             "Riots" = 15, 
                             "Successful Coups" = 16, 
                             "Dropped Veto Players" = 17,
                             "Anti-Government Demonstrations" = 18),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", "ln(inflation)", "CBIE", "GDPpc", "Growth", "Trade", "ln(inflation) * CBIE"),
        custom.note = "%stars. Values for Assassination of Executive, Resignations of Executive, Ousted Leadership by Rebel Forces, Successful Coups, Attempted Coups are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number of Assassinations, Anti-Government Demonstrations, Guerilla Warfare, Government Crises, Purges, Riots, and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence, civil warfare, ethnic violence, and ethnic warfare are from Systemic Peace.Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators.
        Values for Central Bank Independence Index - extended (CBIE) are from Romelli (2022) 
        and provides information on 42 criteria of central bank institutional design across six dimensions: (1) governor and
        central bank board, (2) monetary policy and conflict resolution, (3) objectives, (4) limitations on lending to the government, (5) financial independence and (6) reporting and disclosure.")


# Table 9. Interaction effect of the presence of inflation at 10% or greater  and CBIE on political instability
htmlreg(list(reg_ASSASSEX_inflation_dummy10_inter, 
             reg_assassinations_inflation_dummy10_inter, 
             reg_ATCOUP2_inflation_dummy10_inter, 
             reg_CIVVIOL_inflation_dummy10_inter, 
             reg_CIVWAR_inflation_dummy10_inter, 
             reg_ETHVIOL_inflation_dummy10_inter, 
             reg_ETHWAR_inflation_dummy10_inter,
             reg_government_crises_inflation_dummy10_inter,
             reg_guerilla_warfare_inflation_dummy10_inter,
             reg_PCOUP3_inflation_dummy10_inter,
             reg_purges_inflation_dummy10_inter,
             reg_REBOUTEX_inflation_dummy10_inter,
             reg_RESIGNEX_inflation_dummy10_inter,
             reg_revolutions_inflation_dummy10_inter,
             reg_riots_inflation_dummy10_inter,
             reg_SCOUP1_inflation_dummy10_inter,
             reg_STABS_inflation_dummy10_inter,
             reg_antigovdem_dummy10_inter),
        file = "/Users/valeriaflores-cadena/Desktop/H3_politicalinstability_inflationdummy10_inter.html",
        custom.header = list("Assassination of Executive" = 1, 
                             "Assassinations" = 2,
                             "Attempted Coups" = 3,
                             "Magnitude score of episode(s) of civil violence" = 4, 
                             "Magnitude score of episode(s) of civil warfare" = 5,
                             "Magnitude score of episode(s) of ethnic violence" = 6,
                             "Magnitude score of episode(s) of ethnic warfare" = 7,
                             "Government Crises" = 8,
                             "Guerilla Warfare" = 9, 
                             "Thwarted Coup Plots" = 10, 
                             "Purges" = 11, 
                             "Ousted Leadership by Rebel Forces" = 12, 
                             "Resignation of Executive" = 13, 
                             "Revolutions" = 14, 
                             "Riots" = 15, 
                             "Successful Coups" = 16, 
                             "Dropped Veto Players" = 17,
                             "Anti-Government Demonstrations" = 18),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", "dummy variable for inflation over 10 percent", "CBIE", "GDPpc", "Growth", "Trade", "inflation dummy * CBIE"),
        custom.note = "%stars. Values for Assassination of Executive, Resignations of Executive, Ousted Leadership by Rebel Forces, Successful Coups, Attempted Coups are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number of Assassinations, Anti-Government Demonstrations, Guerilla Warfare, Government Crises, Purges, Riots, and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence, civil warfare, ethnic violence, and ethnic warfare are from Systemic Peace.
        Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators.
        Values for Central Bank Independence Index - extended (CBIE) are from Romelli (2022) 
        and provides information on 42 criteria of central bank institutional design across six dimensions: (1) governor and
        central bank board, (2) monetary policy and conflict resolution, (3) objectives, (4) limitations on lending to the government, (5) financial independence and (6) reporting and disclosure.")


# Table 10. Interaction effect of the presence of inflation at 20% or greater  and CBIE on political instability

htmlreg(list(reg_ASSASSEX_inflation_dummy20_inter, 
             reg_assassinations_inflation_dummy20_inter, 
             reg_ATCOUP2_inflation_dummy20_inter, 
             reg_CIVVIOL_inflation_dummy20_inter, 
             reg_CIVWAR_inflation_dummy20_inter, 
             reg_ETHVIOL_inflation_dummy20_inter, 
             reg_ETHWAR_inflation_dummy20_inter,
             reg_government_crises_inflation_dummy20_inter,
             reg_guerilla_warfare_inflation_dummy20_inter,
             reg_PCOUP3_inflation_dummy20_inter,
             reg_purges_inflation_dummy20_inter,
             reg_REBOUTEX_inflation_dummy20_inter,
             reg_RESIGNEX_inflation_dummy20_inter,
             reg_revolutions_inflation_dummy20_inter,
             reg_riots_inflation_dummy20_inter,
             reg_SCOUP1_inflation_dummy20_inter,
             reg_STABS_inflation_dummy20_inter,
             reg_antigovdem_dummy20_inter),
        file = "/Users/valeriaflores-cadena/Desktop/H3_politicalinstability_inflationdummy20_inter.html",
        custom.header = list("Assassination of Executive" = 1, 
                             "Assassinations" = 2,
                             "Attempted Coups" = 3,
                             "Magnitude score of episode(s) of civil violence" = 4, 
                             "Magnitude score of episode(s) of civil warfare" = 5,
                             "Magnitude score of episode(s) of ethnic violence" = 6,
                             "Magnitude score of episode(s) of ethnic warfare" = 7,
                             "Government Crises" = 8,
                             "Guerilla Warfare" = 9, 
                             "Thwarted Coup Plots" = 10, 
                             "Purges" = 11, 
                             "Ousted Leadership by Rebel Forces" = 12, 
                             "Resignation of Executive" = 13, 
                             "Revolutions" = 14, 
                             "Riots" = 15, 
                             "Successful Coups" = 16, 
                             "Dropped Veto Players" = 17,
                             "Anti-Government Demonstrations" = 18),
        stars = c(0.001, 0.01, 0.05),
        custom.coef.names = c("Intercept", "dummy variable for inflation over 20 percent", "CBIE", "GDPpc", "Growth", "Trade", "inflation dummy * CBIE"),
        custom.note = "%stars. Values for Assassination of Executive, Resignations of Executive, Ousted Leadership by Rebel Forces, Successful Coups, Attempted Coups are from Systemic Peace. 
        Values for number of Dropped Veto Players are from Database of Political Institutions. 
        Values for number of Assassinations, Anti-Government Demonstrations, Guerilla Warfare, Government Crises, Purges, Riots, and Revolutions are from Cross-National Time Series. 
        Values for Magnitude Score of episode(s) of civil violence, civil warfare, ethnic violence, and ethnic warfare are from Systemic Peace.
        Values for inflation, GDP per capita, growth, and trade are from the World Development Indicators.
        Values for Central Bank Independence Index - extended (CBIE) are from Romelli (2022) 
        and provides information on 42 criteria of central bank institutional design across six dimensions: (1) governor and
        central bank board, (2) monetary policy and conflict resolution, (3) objectives, (4) limitations on lending to the government, (5) financial independence and (6) reporting and disclosure.")







