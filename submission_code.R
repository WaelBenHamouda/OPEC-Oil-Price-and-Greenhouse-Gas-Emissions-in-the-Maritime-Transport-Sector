############################################################################################################################## 

################################### APPLIED ECONOMETRICS PROJETCT : Code Submission ################################### 

#Article Name : Oil price and Greenhouse Gas emissions : an investigation 
# on maritime transport sector.
#Question : What is the impact of OPEC oil prices on Greenhouse Gas Emissions 
#in the maritime transportation sector ?

############################################################################################################################## 

# Members of the group :

# BEN HAMOUDA Wael
# JOLIVET Nathan
# MONGEAUD Nelson
# CHAIBI Zidane

# Academic curriculum : M1 Economie Appliquée - Standard track


############################################################################################################################## 

# Context of the study :
# To conduct our study, we decided to divide our initial outcome variable 
# "Greenhouse gas emissions" by the GDP to normalize each observation in order to make
# them comparable to each other and to really measure the impact of OPEC Oil price 
# on Greenhouse gas emissions per million dollars of GDP. In fact, this operation
# in necessary to conduct our study because otherwise, countries' size are too different
# either economically or geographically.


# OUTLINE OF THE R SCRIPT:
# • Line 55-225 : Creation of the database
# • Line 231-End : Analysis



############################################################################################################################## 

#Upploading the needed libraries
library(lmtest)
library(plm)
library(tidyverse)
library(readr)
library(stargazer)
library(xtsum)
library(kableExtra)
library(ggplot2)
library(xtable)
library(pastecs)
library(AER)

################################### APPLIED ECONOMETRICS PROJETCT : DATABASE CREATION PART ################################### 

# Setting the directory path
setwd("/Users/waelbenhamouda/Documents/Master 1/Applied Econometrics/zip_file_submission/data_source x final_data x log_file")



##Importing the dataset
#Population variable. Unit : thousand of individuals
pop <- read_csv("population_thousands.csv.gz")
pop <- pop[,-c(1:5,9)]
pop <- pop %>% rename(Pop="OBS_VALUE")
pop$TIME_PERIOD <- as.numeric(pop$TIME_PERIOD)
pop$Pop <- as.numeric(pop$Pop)
str(pop)
#Convert in unit of individuals
pop$Pop <- pop$Pop*1000
str(pop)

#Greenhouse Gas Emissions variable : million tonnes
ghg_emissions <- read_csv("ghg_emissions.csv.gz")
ghg_emissions <- ghg_emissions[,-c(1:6,10)]
ghg_emissions <- ghg_emissions %>% rename(GHG="OBS_VALUE")
ghg_emissions$TIME_PERIOD <- as.numeric(ghg_emissions$TIME_PERIOD)
ghg_emissions$GHG <- as.numeric(ghg_emissions$GHG)
str(ghg_emissions)
#Convert in Kg
ghg_emissions$GHG <- ghg_emissions$GHG*1000000000
str(pop)

#Real GDP in constant 2015 dollars variable
gdp_constant_2015_dollars <- read_csv("gdp_constant_2015_dollars.csv")
gdp_constant_2015_dollars <- gdp_constant_2015_dollars[,-c(2:4,68)]
gdp_constant_2015_dollars <- gdp_constant_2015_dollars %>% gather("TIME_PERIOD","GDP",2:64)
gdp_constant_2015_dollars <- gdp_constant_2015_dollars %>% rename(Name="Country Name")
gdp_constant_2015_dollars$TIME_PERIOD <- as.numeric(gdp_constant_2015_dollars$TIME_PERIOD)
gdp_constant_2015_dollars$GDP <- as.numeric(gdp_constant_2015_dollars$GDP)
str(gdp_constant_2015_dollars)
#Convert in million euros
gdp_constant_2015_dollars$GDP <- gdp_constant_2015_dollars$GDP/1000000
str(gdp_constant_2015_dollars)

#Share of renewable energy variable in %
share_renewable_energy <- read_delim("share_of_renewable_energy.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
share_renewable_energy <- share_renewable_energy[,-c(2:32)]
share_renewable_energy <- share_renewable_energy %>% gather("TIME_PERIOD","SRE",2:34)
share_renewable_energy <- share_renewable_energy %>% rename(Name="...1")
share_renewable_energy$TIME_PERIOD <- as.numeric(share_renewable_energy$TIME_PERIOD)
share_renewable_energy$SRE <- as.numeric(share_renewable_energy$SRE)
str(share_renewable_energy)


#Oil dependency variable in % of gross available energy
oilDependency <- read_csv("oil_dependency.csv.gz")
oilDependency <- oilDependency[,-c(1:5,9)]
oilDependency <- oilDependency %>% rename(OilDep="OBS_VALUE")
oilDependency$TIME_PERIOD <- as.numeric(oilDependency$TIME_PERIOD)
oilDependency$OilDep <- as.numeric(oilDependency$OilDep)
str(oilDependency)


#OPEC oil price adjusted from inflation and exchange rates
## The original table used is "oil_price_adjusted.xlsx"
oil_price_adjusted <- read_delim("oil_price_adjusted.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
oil_price_adjusted <- oil_price_adjusted[-c(1:4),c(2:50)]
oil_price_adjusted <- oil_price_adjusted %>% gather("TIME_PERIOD","PriceAdjOil", 1:49)
oil_price_adjusted$TIME_PERIOD <- as.numeric(oil_price_adjusted$TIME_PERIOD)
oil_price_adjusted$PriceAdjOil <- as.numeric(oil_price_adjusted$PriceAdjOil)
#We need to divide by 100 because the decimals (2 for each value) have been omitted when we imported the database
oil_price_adjusted$PriceAdjOil <- oil_price_adjusted$PriceAdjOil/100
str(oil_price_adjusted)


#Crude oil consumption. Unit : thousand tonnes
oil_cons <- read_csv("oil_cons_dom_nav.csv.gz")
oil_cons <- oil_cons[,-c(1:6,10)]
oil_cons <- oil_cons %>% rename(OilCons="OBS_VALUE")
#Convert in Kg
oil_cons$OilCons <- oil_cons$OilCons*1000000
str(oil_cons)


##############################################################################################################################################################################################
#Creation of the Identification Country Table in order to identify each value of each variable by its Code (called "geo" in the data) and its Name
countries <- as.data.frame(c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","EU27_2020","FI","FR","HR","HU","IE","IS","IT","LT","LU","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK"))
countries_names <- as.data.frame(c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Germany","Denmark","Estonia","Greece","Spain","European Union - 27 countries (from 2020)",
                                   "Finland","France","Croatia","Hungary","Ireland","Iceland","Italy","Lithuania","Luxembourg","Latvia","Malta","Netherlands","Norway","Poland","Portugal","Romania","Sweden","Slovenia","Slovakia"))

geo <- cbind(countries,countries_names)
colnames(geo)[1] <- "geo"
colnames(geo)[2] <- "Name"

##############################################################################################################################################################################################
#Merge all the databases
data <- inner_join(oilDependency,ghg_emissions, by=c("geo","TIME_PERIOD")) %>%
  inner_join(pop, by=c("geo","TIME_PERIOD")) %>% inner_join(oil_cons, by=c("geo","TIME_PERIOD"))

data <- inner_join(data, geo, by=c("geo"))

data <- inner_join(data,gdp_constant_2015_dollars, by=c("Name","TIME_PERIOD")) %>% inner_join(share_renewable_energy, by=c("Name", "TIME_PERIOD"))

data <- inner_join(data, oil_price_adjusted,by=c("TIME_PERIOD"))

##############################################################################################################################################################################################
#To see what time range is available for each country
range <- data %>% group_by(geo) %>% summarise(min(TIME_PERIOD),max(TIME_PERIOD))

#Keeping the observations that have values between 1995 and 2020
data <- data %>% filter(TIME_PERIOD>1994 & TIME_PERIOD<2021)

#To check if there are some remaining NAs in our data
sum(is.na(data))
#No NAs remaining

##############################################################################################################################################################################################
##Creation of variables
#GHG by million dollars of GDP
data <- data %>% mutate(GHGperGDP=GHG/GDP)


#Positive oil supply shocks variable created with data based on an FMI document (Source in the Appendix)
data <- data %>% group_by(geo) %>% mutate(PosSupplyShock=ifelse(TIME_PERIOD %in% c(1996, 1998, 2000, 2003, 2004, 2005, 2008, 2018), 1, 0))


#Negative oil supply shocks variable created with data based on an FMI document (Source in the Appendix)
data <- data %>% group_by(geo) %>% mutate(NegSupplyShock=ifelse(TIME_PERIOD %in% c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2007, 2009, 2017, 2019), 1, 0))


#GDP growth rate
data <- data %>% group_by(geo) %>% arrange(geo,TIME_PERIOD) %>% 
  mutate(GDPgrowth = 100*(GDP-(dplyr::lag(GDP)))/(dplyr::lag(GDP)))
data <- data %>% mutate(GDPgrowth=ifelse(is.na(GDPgrowth),0,GDPgrowth))
#We put a "0" at the 1995 value for each country because there are some NAs in the GDP of year 1994, 
#then we can't compute the 1995 growth rate value (We can interpret it also as : since our time range starts in 1995, 
#the GDP growth rate is equal to 0 [at the first period])

#GDP per capita. Unit : $ per Capita (Multiplying GDP by 1 000 000 to convert in $)
data <- data %>% mutate(GDPperCap=1000000*GDP/Pop)

#Oil consumption by million dollars of GDP
data <- data %>% mutate(OilCperGDP=OilCons/GDP)

##############################################################################################################################################################################################
#Arrange the order of the variables in the database
data <- data %>% select(geo,Name,TIME_PERIOD,GHGperGDP,PriceAdjOil,GDPperCap,OilCperGDP,GDPgrowth,OilDep,SRE,PosSupplyShock,NegSupplyShock,GHG,GDP,Pop,OilCons)

##############################################################################################################################################################################

# "0" values issue
stat.desc(data)
#There are 113 "0" values for the Oil Consumption variable (and consequently for the Oil Consumption per GDP
#variable)) and 7 for the SRE (share of renewable energy variable).

#Let's see which country has "0" values :
a <- filter(data,OilCperGDP==0)
b <- filter(data,SRE==0)
#Countries that have "0" values are : "Malta","Cyprus","Hungary","Bulgaria","Czechia","Luxembourg","Slovenia","Latvia"

#Let's erase them :
data <- data %>% filter(!(Name %in%  c("Malta","Cyprus","Hungary","Bulgaria","Czechia","Luxembourg","Slovenia","Latvia")))

#No unexpected 0 values anymore
stat.desc(data)


# Save the data
# write_csv(data,"")

##############################################################################################################################################################################


##############################################################################################################################################################################


################################### APPLIED ECONOMETRICS PROJETCT : ANALYSIS PART ################################### 



#Importing the final data
data <- read_csv("final_data.csv")


#Identification of the panel data
pdata <- pdata.frame(data[,-c(2)], index = c("geo", "TIME_PERIOD"))
# We take out the "Name" column that is useless


################################################################################################################################################

##################################################### Descriptive Statistics ############################################################

#Calculate summary statistics for each numeric variable in data frame
descriptive_stats <- data[,-c(1:3,11)] %>% 
  summarise(across(everything(), list(
    min = ~min(.x, na.rm = TRUE),
    q25 = ~quantile(.x, 0.25, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    mean = ~mean(.x, na.rm = TRUE),
    stdev = ~sd(.x, na.rm = TRUE),
    q75 = ~quantile(.x, 0.75, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_sep = "_", names_to = c("variable", ".value"))

#To get the latex code of the descriptive statistics above
kable(descriptive_stats, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  column_spec(1, width = "3cm") %>% kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>% pack_rows(index = descriptive_stats$variable)



## Within, between and general summary statistics for the variable GHGperGDP
xtsum(pdata, "GHGperGDP", id = "geo", t = "TIME_PERIOD", na.rm = T, dec = 6)

## Within, between and general summary statistics for the variable Oil Price Adjusted
xtsum(pdata, "PriceAdjOil", id = "geo", t = "TIME_PERIOD", na.rm = T, dec = 6)




##################################################### Some plots ############################################################



## Oil consumption per GDP plot
# Computing the average of Oil Consumption per GDP for each year
mean_OilCperGDP_per_year <- data %>%
  group_by(TIME_PERIOD) %>%
  summarise(mean_OilCperGDP = mean(OilCperGDP, na.rm = TRUE))
# Plotting the results
ggplot(mean_OilCperGDP_per_year, aes(x = TIME_PERIOD, y = mean_OilCperGDP)) +
  geom_point() +
  geom_line() +
  theme_minimal() + 
  xlab("Year") + 
  ylab("Average Oil consumption per GDP") 


## Plotting GHG per GDP and Oil Price Adjusted over year on the same graph
# Computing the average of GHG per GDP for each year
mean_GHGperGDP_per_year <- data %>%
  group_by(TIME_PERIOD) %>%
  summarise(mean_GHGperGDP = mean(GHGperGDP, na.rm = TRUE))
# Fixing a range for our values in the graph
conversion_factor <- max(data$PriceAdjOil) / max(mean_GHGperGDP_per_year$mean_GHGperGDP)
# Plotting the results
ggplot() +
  geom_line(data = data, aes(x = TIME_PERIOD, y = PriceAdjOil), color = "blue") +
  geom_point(data = mean_GHGperGDP_per_year, aes(x = TIME_PERIOD, y = mean_GHGperGDP * conversion_factor), color = "red") +
  geom_line(data = mean_GHGperGDP_per_year, aes(x = TIME_PERIOD, y = mean_GHGperGDP * conversion_factor), color = "red") +
  scale_y_continuous(
    "Oil Price Adjusted", 
    sec.axis = sec_axis(~ . / conversion_factor, name = "Average GHG per GDP")) +
  theme_minimal() +
  xlab("Time") + ylab("Oil Price Adjusted / Average GHG per GDP")



##################################################### Econometric study ############################################################


## Pooling model
pooling_model <- plm(data=pdata, GHGperGDP ~  PriceAdjOil + I(PriceAdjOil*OilDep) + GDPgrowth +
                       GDPperCap + OilCperGDP + SRE + OilDep, model="pooling")
stargazer(pooling_model,type="text",title="Pooling model")


## Within model - Individual effects
within_model <- plm(data=pdata, GHGperGDP ~  PriceAdjOil + I(PriceAdjOil*OilDep) + GDPgrowth +
                      GDPperCap + OilCperGDP + SRE + OilDep, model="within",effect = "individual")
stargazer(within_model,type="text",title="Within model - Individual effect")
#To control the individual effects over time and to analyze
#the changes within the individuals



## Random effect
re_model <- plm(data=pdata, GHGperGDP ~  PriceAdjOil + I(PriceAdjOil*OilDep) + GDPgrowth +
                  GDPperCap + OilCperGDP + SRE + OilDep, model="random")
stargazer(re_model,type="text",title="Random effects model")




# Storing the results of these 3 estimations in a single table
stargazer(pooling_model,within_model,re_model,type="text",title = "Results of pooled, within and random estimations",
          column.labels = c("Pooled model", "Within model", "Random model"),
          intercept.bottom = FALSE,
          digits = 4)
# Latex code
stargazer(pooling_model,within_model,re_model,type="latex",title = "Results of pooled, within and random estimations",
          column.labels = c("Pooled model", "Within model", "Random model"),
          intercept.bottom = FALSE,
          digits = 4)




## Which model to use ?
#Hausmann test
phtest(within_model,re_model)
#Reject H0, then we do a fixed effect model
# Latex code
xtable(data.frame(Metric = c("Test statistic", "P-value"),Value = c("111.54","< 2.2e-16")), 
       caption = "Hausman test results", label = "tab:hausman_results") #Latex code





##############################################################################################################################################################################################
## 2SLS estimation
#Since our price variable is supposed endogenous due to a simultaneous equation issue, 
#we will test if our instruments are exogenous and relevant. We estimate only the supply side of
#the price equilibrium by using a negative supply shock variable (NegSupplyShock) as an instrument.




# First stage 2SLS with positive and negative supply shocks
first_stage_TwoSLS_Pos_Neg_Sup_shock <- plm(data=pdata, PriceAdjOil ~ PosSupplyShock + NegSupplyShock + GDPgrowth +
                            GDPperCap + OilCperGDP + SRE + OilDep, model="within", effect="individual")
summary(first_stage_TwoSLS_Pos_Neg_Sup_shock)
# Interpretation : 
# • The sign of the Positive and Negative Supply Shock variables estimate is negative as expected because when
#   a supply shock occurs, quantities and price move in opposite directions.
# • The Positive Supply Shock variable is not significant. We hence decided to keep only the 
#   Negative Supply Shock variable and drop Positive Supply Shock variable.


# First stage 2SLS with only negative supply shocks
first_stage_TwoSLS <- plm(data=pdata, PriceAdjOil ~ NegSupplyShock + GDPgrowth +
                            GDPperCap + OilCperGDP + SRE + OilDep, model="within", effect="individual")
summary(first_stage_TwoSLS,type="text")
# Interpretation : 
# • The sign of the Supply Shock variable estimate is still the one we expected and significant.It means that
#   our instrument is relevant for our estimation.  


# Store the first stage 2SLS method with and without positive supply shocks in a same table
stargazer(first_stage_TwoSLS_Pos_Neg_Sup_shock,first_stage_TwoSLS,type="text",title="First stage regression of the 2SLS method with and without positive supply shocks",
          column.labels = c("First stage with positive and negative supply shocks", "First stage with only negative supply shocks"),
          intercept.bottom = FALSE,
          digits = 4)
# Latex code
stargazer(first_stage_TwoSLS_Pos_Neg_Sup_shock,first_stage_TwoSLS,type="latex",title="First stage regression of the 2SLS method with and without positive supply shocks",
          column.labels = c("First stage with positive and negative supply shocks", "First stage with only negative supply shocks"),
          intercept.bottom = FALSE,
          digits = 4) #Latex code


# Store the fitted values of the price from the first stage 2SLS
pdata$FittedPrice <- fitted(first_stage_TwoSLS)


##############################################################################################################################################################################################
# Extra Analysis : Regression without oil dependency
extra_2SLS <- plm(data=pdata, GHGperGDP ~  FittedPrice + GDPgrowth +
      GDPperCap + OilCperGDP + SRE, model="within",effect="individual")
summary(extra_2SLS)
# Robust estimation 2SLS without Oil Dependency
coeftest(extra_2SLS,vcov=vcovBK)
# We use vcovBK to correct from heteroskedasticity and autocorrelation

# Store the results with and without heteroskedasticity correction
stargazer(extra_2SLS,coeftest(extra_2SLS,vcov=vcovBK),type="text",  title="2SLS without Oil Dependency", 
          column.labels = c("2SLS", "Robust"),
          intercept.bottom = FALSE,
          digits = 4)
# Latex code
stargazer(extra_2SLS,coeftest(extra_2SLS,vcov=vcovBK),type="latex",  title="2SLS without Oil Dependency", 
          column.labels = c("2SLS", "Robust"),
          intercept.bottom = FALSE,
          digits = 4)



##############################################################################################################################################################################################
## Second stage 2SLS with Oil Dependency
Two_SLS <- plm(data=pdata, GHGperGDP ~  FittedPrice + I(FittedPrice*OilDep) + GDPgrowth +
                 GDPperCap + OilCperGDP + SRE + OilDep, model="within",effect="individual")
summary(Two_SLS)
# SRE is now significant at 5% level.

## Robust estimation 2SLS with Oil Dependency
coeftest(Two_SLS,vcov=vcovBK)
# SRE no longer significant. Oil price is not significant.
# Crossed variable Oil Price Adjusted and Oil Dependency is significant at 10% level.


# Store the results with and without heteroskedasticity correction
stargazer(Two_SLS,coeftest(Two_SLS,vcov=vcovBK),type="text",title="2SLS with Oil Dependency",
          column.labels = c("2SLS", "Robust"),
          intercept.bottom = FALSE,
          digits = 4)
# Latex code
stargazer(Two_SLS,coeftest(Two_SLS,vcov=vcovBK),type="latex",title="2SLS with Oil Dependency",
          column.labels = c("2SLS", "Robust"),
          intercept.bottom = FALSE,
          digits = 4)


##############################################################################################################################################################################################
## Exogeneity verification

pdata$res2SLS <- residuals(Two_SLS)
exo <- plm(data=pdata, res2SLS ~ NegSupplyShock + GDPgrowth +
      GDPperCap + OilCperGDP + SRE + OilDep)
stargazer(exo,type="text",title="Exogeneity verification model of the instrument",digits = 18)
## Instrument is exogenous but all coefficients equal 0.
exo
## In reality, they are not equal to 0, but very small and the table above doesn't show a large number decimals.
# In fact, in the 2SLS regression, the R squared is about 85% which means that the model explains very well
# the dependent variable, and hence provides small residuals (because fitted values are close to the truth).
# Then, when we regress these residuals on the instrument and the exogenous variables (regressing very small 
# values of Y on very big values of X), it then provides very small estimates which are close to 0, but not 
# equal to 0 ! 

# Latex code of the exogeneity results
stargazer(exo,type="latex",title="Exogeneity verification model of the instrument",digits = 4)

# Latex code of the estimates
coefficients <- data.frame(
  Term = c("NegSupplyShock", "GDPgrowth", "GDPperCap", "OilCperGDP", "SRE", "OilDep"),
  Estimate = c("2.9693e-14", "1.3120e-15", "7.0982e-18", "6.1695e-17", "-1.9037e-15", "-1.1264e-16"))

latex_table <- xtable(coefficients, caption = "Exogeneity verification model of the instrument", 
                      label = "tab:exogeneity_verification", digits = c(0,0,18))
print(latex_table, include.rownames = FALSE, type = "latex", floating = TRUE, 
      caption.placement = "top")


##############################################################################################################################################################################################
## Fixed effect estimates histogram
hist(fixef(Two_SLS,type="dmean"),breaks=37,freq=FALSE,density=20,col="blue",
     xlab="fixed effect",main="Fixed effects distribution of the 2SLS robust estimation (histogram and 
     kernel density)")
lines(density(fixef(Two_SLS,type="dmean")),col="red",lwd=2)



##############################################################################################################################################################################################
## 2SLS without 2 supposed outliers : Norway and Denmark 
extra_2SLS_reduced <- plm(data=filter(pdata, !geo %in% c("DK","NO")), GHGperGDP ~  FittedPrice + I(FittedPrice*OilDep) + GDPgrowth +
                            GDPperCap + OilCperGDP + SRE + OilDep, model="within",effect="individual")
summary(extra_2SLS_reduced)
# Oil Price and crossed variable are significant
## Robust estimation 2SLS without Norway and Denmark
coeftest(extra_2SLS_reduced,vcov=vcovHC)
# Oil Price and crossed variable no longer significant

## Store the results with and without heteroskedasticity correction
stargazer(extra_2SLS_reduced,coeftest(extra_2SLS_reduced,vcov=vcovBK),type="text", title ="2SLS without Norway and Denmark",
          column.labels = c("2SLS", "Robust"),
          intercept.bottom = FALSE,
          digits = 4)
## Latex code
stargazer(extra_2SLS_reduced,coeftest(extra_2SLS_reduced,vcov=vcovBK),type="latex", title ="2SLS without Norway and Denmark",
          column.labels = c("2SLS", "Robust"),
          intercept.bottom = FALSE,
          digits = 4)

