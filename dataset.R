install.packages("eurostat")
install.packages("RJSONIO")
install.packages("wbstats")
install.packages('OECD')
install.packages("usethis")
install.packages(c('tibble', 'dplyr', 'readr'))
remotes::install_github("https://github.com/expersso/OECD")
install_github("expersso/OECD")
library(remotes)
remotes::install_github("ropengov/eurostat")
library(dplyr)
library(readr)
library(tibble)
library(data.table)
library(usethis)
library(devtools)
library(OECD)
library(eurostat)
library(httr)
library(RJSONIO)
library(tidyverse)
library(wbstats)

# Get Eurostat data listing
toc <- get_eurostat_toc()
year <-  seq(from =1995, to=2021)
countries = c("AT","BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EA19", "EA20", "EE", "EL", "ES", 
              "EU27_2020", "FI", "FR", "HR", "HU", "IE", "IS","IT", "LT", "LU", "LV", "MT", "NL",
              "NO", "PL", "PT", "RO", "SE", "SI", "SK", "TR", "UK")
country_name <- efta_countries
country_nam <- eu_countries
# `government expenditure Totale, general gov, millions euro

filters1 = list( cofog99= "Total",
                 na_item = "TE",
                 sector = "S13",
                unit = "MIO_EUR")
data_exp <- get_eurostat("GOV_10A_EXP", filters = filters1)
data_exp = rename(data_exp,exp_tot = values)
data_exp$time = format(as.Date(data_exp$time, format ="%d/%m/%Y"),"%Y")

warning()
# Government final consumption  + investment grant (has to be deflated, current millions euros)
# P3 final consumption expenditure ,D92 investment grant

filter_c = list(cofog99 = "TOTAL",
                na_item = c("P3","D92"),
                sector = "S13",
                unit = "MIO_EUR")
exp_consinv<- get_eurostat("GOV_10A_EXP", filters = filter_c)

inv_grant <- subset(exp_consinv,na_item == "D92")
inv_grant = rename(inv_grant,inv_grants = values)
inv_grant$time = format(as.Date(inv_grant$time, format ="%d/%m/%Y"),"%Y")

fin_cons <- subset(exp_consinv,na_item == "P3")
fin_cons = rename(fin_cons,final_cons=values)
fin_cons$time = format(as.Date(fin_cons$time, format ="%d/%m/%Y"),"%Y")
# National expenditure on environmental protection, total economy, millions euro to be deflated 

filter2 = list(sector = "S1",
               unit = "MIO_EUR")
data_env <- get_eurostat("ENV_AC_EPNEIS", filters = filter2)
data_env = rename(data_env,exp_env=values)

data_env$time = format(as.Date(data_env$time, format="%d/%m/%Y"),"%Y")

data_join <-merge(data_exp,data_env, by = c("geo", "time"), all = TRUE)
data_join = rename(data_join,exp_env=values.y)
data_join$time = format(as.Date(data_join$time, format="%d/%m/%Y"),"%Y")

# GDP 

gdp_base <- wb_search("unemployment")

## Unemployment 
oecddataset_list <- get_datasets()

dataset_list <- get_datasets()
  # Thousand person working 

data_employ <- get_eurostat("NAMA_10_A10_E", filters = list(na_item = "EMP_DC", nace_r2 = "TOTAL", unit = "THS_PER"))
colnames(data_employ)[colnames(data_employ) == 'values'] <- 'ths_wpeople'
data_employ$time = format(as.Date(data_employ$time, format ="%d/%m/%Y"),"%Y")
emp <- search_dataset("work", data =dataset_list)


  # thousand hour worker 
data_hour <- get_eurostat("NAMA_10_A10_E", filters = list(na_item = "EMP_DC", nace_r2 = "TOTAL", unit = "THS_WH"))
colnames(data_hour)[colnames(data_hour) == 'values'] <- 'ths_whour'
data_hour$time = format(as.Date(data_hour$time, format ="%d/%m/%Y"),"%Y")


df2 <- "MIG_NUP_RATES_GENDER"

emp <- "GENDER_EMP"

dstruc4 <- get_data_structure(emp)   # list of dataframes with human-readable values for variable names and values
str(dstruc4, max.level = 1)
dstruc$VAR_DESC
dstruc$RATE
dstruc$COUNTRY
dstruc$GENDER
dstruc$UNIT

dstruc4$VAR_DESC
dstruc4$IND
dstruc$OBS_STATUS 
dstruc$GENDER
dstruc4$UNIT

filter_list <- "U_RATE"
df <- get_dataset(dataset = df2, RATE = "U_RATE")
df4 <- get_dataset(dataset = emp, IND = "EMP15_T")
df_emp = subset(df, RATE == "U_RATE")
df_int = subset(df_emp, GENDER == "TOT")
colnames(df_emp)[colnames(df_emp) == 'COUNTRY'] <- 'geo'
colnames(df_emp)[colnames(df_emp) == 'Time'] <- 'time'
colnames(df_int)[colnames(df_int)=='ObsValue'] <- 'unemp_tot'

# countries = c("AT","BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EA19", "EA20", "EE", "EL", "ES", 
              #"EU27_2020", "FI", "FR", "HR", "HU", "IE", "IS","IT", "LT", "LU", "LV", "MT", "NL",
              #"NO", "PL", "PT", "RO", "SE", "SI", "SK", "TR", "UK")
# adapting country code to eurostat's code 
df_int$geo[df_int$geo == "AUT"] = "AT"
df_int$geo[df_int$geo == "BEL"] = "BE"
df_int$geo[df_int$geo == "CZE"] = "CZ"
df_int$geo[df_int$geo == "DNK"] = "DK"
df_int$geo[df_int$geo == "EST"] = "EE"
df_int$geo[df_int$geo == "FIN"] = "FI"
df_int$geo[df_int$geo == "FRA"] = "FR"
df_int$geo[df_int$geo == "DEU"] = "DE"
df_int$geo[df_int$geo == "GRC"] = "EL"
df_int$geo[df_int$geo == "HUN"] = "HU"
df_int$geo[df_int$geo == "IRL"] = "IE"
df_int$geo[df_int$geo == "ISL"] = "IS"
df_int$geo[df_int$geo == "ITA"] = "IT"
df_int$geo[df_int$geo == "LVA"] = "LV"
df_int$geo[df_int$geo == "LTU"] = "LT"
df_int$geo[df_int$geo == "LUX"] = "LU"
df_int$geo[df_int$geo == "NOR"] = "NO"
df_int$geo[df_int$geo == "POL"] = "PL"
df_int$geo[df_int$geo == "SVK"] = "SK"
df_int$geo[df_int$geo == "SVN"] = "SI"
df_int$geo[df_int$geo == "ESP"] = "ES"
df_int$geo[df_int$geo == "SWE"] = "SE"
df_int$geo[df_int$geo == "CHE"] = "CH"
df_int$geo[df_int$geo == "GRB"] = "UK"


data_join <-merge(data_join,df_int, by = c("geo", "time"), all = TRUE)

df_emp <- df_emp[ , time = as.Date(time)]
df_int$time = format(as.Date(df_int$time, format="%d/%m/%Y"),"%Y")
df_emp$time <- as.Date(df_emp$time,format="%Y")
data_join$time <- as.Date(data_join$time,format="%Y")
mutate(data_join, time = as.Date(time, format= "%Y"))
class(Date$ArrestDate)

sapply(df_emp, class)
sapply(data_join,class)

# growth rate / GDP


pub <- search_dataset("expenditure", data = dataset_list)
dataset = "SNA_TABLE1" # Gross domestic Product data base 

dstruc2 <- get_data_structure(dataset)   # list of dataframes with human-readable values for variable names and values
str(dstruc2, max.level = 1)
dstruc2$UNIT          # GRWH : growth rate, USD : dollar 
dstruc2$MEASURE 
dstruc2$VAR_DESC
dstruc2$OBS_STATUS
dstruc2$LOCATION

filter_gdp <- c( MEASURE = )
df_gdp <- get_dataset(dataset ="SNA_TABLE1", LOCATION = "FRA" )

search_dataset("GDP", data = dataset_list)
filter_gdp <- c( MEASURE = )

# CP_MEUR : current price million euros, B1QG : Gross domestic product at market price 

data_gdp <- get_eurostat("nama_10_gdp", filters = list(geo = countries, na_item = "B1GQ", unit = "CP_MEUR"))
data_gdp <- rename(data_gdp,gdp = values)
data_gdp$time = format(as.Date(data_gdp$time, format="%d/%m/%Y"),"%Y")

# Price index : implicit deflator 2010 = 100 euros PD10EUR 
data_gdp_deflator10 <- get_eurostat("nama_10_gdp", filters = list(geo = countries, na_item = "B1GQ", unit = "PD110_EUR"))
data_gdp_deflator10$time = format(as.Date(data_gdp_deflator10$time, format="%d/%m/%Y"),"%Y")

data_gdp_deflator15 <- get_eurostat("nama_10_gdp", filters = list(geo = countries, na_item = "B1GQ", unit = "PD15_EUR"))
data_gdp_deflator15 <- rename(data_gdp_deflator15,deflator15 = values)
data_gdp_deflator15$time = format(as.Date(data_gdp_deflator15$time, format="%d/%m/%Y"),"%Y")


# Interest rate 

rate <- "KEI"
strucrate <- get_data_structure(rate)
str(strucrate, max.level = 1)
strucrate$MEASURE  
strucrate$SUBJECT  # IRLTLT01   Long-term interest rate
strucrate$FREQUENCY # A annual 
strucrate$OBS_STATUS
strucrate$UNIT
df_int <- get_dataset(dataset="KEI",filter = "IR+IRLTLT01.AUT+BEL+CHL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+LTU+LVA+LUX+NLD+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+IND+IDN.ST.A",
                      pre_formatted = TRUE)
df_int$LOCATION[df_int$LOCATION == "AUT"] = "AT"
df_int$LOCATION[df_int$LOCATION == "BEL"] = "BE"
df_int$LOCATION[df_int$LOCATION == "CZE"] = "CZ"
df_int$LOCATION[df_int$LOCATION == "DNK"] = "DK"
df_int$LOCATION[df_int$LOCATION == "EST"] = "EE"
df_int$LOCATION[df_int$LOCATION == "FIN"] = "FI"
df_int$LOCATION[df_int$LOCATION == "FRA"] = "FR"
df_int$LOCATION[df_int$LOCATION == "DEU"] = "DE"
df_int$LOCATION[df_int$LOCATION == "GRC"] = "EL"
df_int$LOCATION[df_int$LOCATION == "HUN"] = "HU"
df_int$LOCATION[df_int$LOCATION == "IRL"] = "IE"
df_int$LOCATION[df_int$LOCATION == "ISL"] = "IS"
df_int$LOCATION[df_int$LOCATION == "ITA"] = "IT"
df_int$LOCATION[df_int$LOCATION == "LVA"] = "LV"
df_int$LOCATION[df_int$LOCATION == "LTU"] = "LT"
df_int$LOCATION[df_int$LOCATION == "LUX"] = "LU"
df_int$LOCATION[df_int$LOCATION == "NOR"] = "NO"
df_int$LOCATION[df_int$LOCATION == "POL"] = "PL"
df_int$LOCATION[df_int$LOCATION == "SVK"] = "SK"
df_int$LOCATION[df_int$LOCATION == "SVN"] = "SI"
df_int$LOCATION[df_int$LOCATION == "ESP"] = "ES"
df_int$LOCATION[df_int$LOCATION == "SWE"] = "SE"
df_int$LOCATION[df_int$LOCATION == "CHE"] = "CH"
df_int$LOCATION[df_int$LOCATION == "GRB"] = "UK"

colnames(df_int)[colnames(df_int) == 'LOCATION'] <- 'geo'
colnames(df_int)[colnames(df_int) == 'Time'] <- 'time'
df_int <- rename(df_int,long_int=ObsValue)

#Building all dataset
fin_cons <- subset(fin_cons,select = c(time,geo,final_cons))
inv_grant <- subset(inv_grant,select = c(time,geo,inv_grants))
data_env <- subset(data_env, select = c(time,geo,exp_env))
data_employ <- subset(data_employ,select =c(time,geo, ths_wpeople))
data_hour <- subset(data_hour,select = c(time,geo, ths_whour))
data_gdp <- subset(data_gdp,select = c(time,geo,gdp))
data_gdp_deflator15 <- subset(data_gdp_deflator15,select=c(geo,time,deflator15))
df_int <- subset(df_int,select = c(geo,time,long_int))

data_join <- merge(fin_cons,inv_grant, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_env, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_employ, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_hour, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_gdp, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_gdp_deflator15, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,df_int, by = c("geo", "time"), all = TRUE)

data_join = data_join[rowSums(is.na(data_join))<8,]




