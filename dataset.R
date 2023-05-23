install.packages("eurostat")
install.packages("RJSONIO")
install.packages("wbstats")
install.packages('OECD')
install.packages("usethis")
install.packages("openxlsx")
install.packages('IMFData')
devtools::install_github('mingjerli/IMFData')
library(IMFData)
install.packages(c('tibble', 'dplyr', 'readr'))

remotes::install_github("https://github.com/expersso/OECD")
install_github("expersso/OECD")
library(remotes)
remotes::install_github("ropengov/eurostat",force = TRUE)
library(openxlsx)
library(imfr)
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
ap <- available.packages()
# Get Eurostat data listing
toc <- get_eurostat_toc()
year <-  seq(from =1995, to=2021)


countries = c("AT","BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE","EU", "EL", "ES", 
               "FI", "FR", "HR", "HU", "IE", "IS","IT", "LT", "LU", "LV", "MT", "NL",
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

filter_c = list(cofog99 = "GF05",
                na_item = c("P3","P51G","TE"),
                sector = "S13",
                unit = "MIO_EUR")
exp_consinv<- get_eurostat("GOV_10A_EXP", filters = filter_c)

env_tot <- subset(exp_consinv, na_item == "TE")
env_inv <- subset(exp_consinv,na_item == "P51G")
env_cons <- subset(exp_consinv,na_item == "P3")
env_tot$time = format(as.Date(env_tot$time, format ="%d/%m/%Y"),"%Y")
env_inv$time = format(as.Date(env_inv$time, format ="%d/%m/%Y"),"%Y")
env_cons$time = format(as.Date(env_cons$time, format ="%d/%m/%Y"),"%Y")

env_tot <- rename(env_tot,env_total = values )
env_inv <- rename(env_inv, env_invest = values)
env_cons <- rename(env_cons, env_consu = values)

env_tot <- subset(env_tot,select = c(time,geo,env_total))
env_inv <- subset(env_inv,select = c(time,geo,env_invest))
env_cons <- subset(env_cons,select = c(time,geo,env_consu))


dat_gfcf<- get_eurostat("GOV_10A_EXP", filters = filter_f)


filter_f = list(cofog99 = "Total",
                na_item = "P51G",# Total public invstment Gross Fixed Capital Formation 
                sector = "S13",
                unit = "MIO_EUR")

dat_gfcf <- rename(dat_gfcf,gfcf_pub = values )



dat_gfcf <- subset(dat_gfcf,select = c(geo,time, gfcf_pub))
dat_gfcf$time = format(as.Date(dat_gfcf$time, format ="%d/%m/%Y"),"%Y")
data_join <- merge(data_join,dat_gfcf, by = c("geo", "time"), all = TRUE)


inv_grant <- subset(exp_consinv,na_item == "D92")
inv_grant = rename(inv_grant,inv_grants = values)
inv_grant$time = format(as.Date(inv_grant$time, format ="%d/%m/%Y"),"%Y")

fin_cons <- subset(exp_consinv,na_item == "P3")
fin_cons = rename(fin_cons,final_cons=values)
fin_cons$time = format(as.Date(fin_cons$time, format ="%d/%m/%Y"),"%Y")
# National expenditure on environmental protection, total economy, millions euro to be deflated 

filter2 = list(sector = "S13",
               cofog99 = "GF05", 
               na_item = "P51G",
               unit = "MIO_EUR")
data_env_tot <- get_eurostat("GOV_10A_EXP", filters = c(sector = "S13",
                                                         cofog99 = "GF05", 
                                                         unit = "MIO_EUR"))

filter2 = list(sector = "S13",
               cofog99 = "P51G",
               na_item = "TE",
               unit = "MIO_EUR")
data_env_inv <- get_eurostat("GOV_10A_EXP", filters = filter2)
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

df_int_short <- get_dataset(dataset="MEI_FIN",filter = "IR3TIB.AUT+BEL+CHL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+LTU+LVA+LUX+NLD+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+IND+IDN.A",
                      pre_formatted = TRUE)
df_int_short <- subset(df_int_short,select = c(geo,time,short_int))

df_int_short$geo[df_int_short$geo== "AUT"] = "AT"
df_int_short$geo[df_int_short$geo == "BEL"] = "BE"
df_int_short$LOCATION[df_int_short$LOCATION == "CZE"] = "CZ"
df_int_short$LOCATION[df_int_short$LOCATION == "DNK"] = "DK"
df_int_short$LOCATION[df_int_short$LOCATION == "EST"] = "EE"
df_int_short$LOCATION[df_int_short$LOCATION == "FIN"] = "FI"
df_int_short$LOCATION[df_int_short$LOCATION == "FRA"] = "FR"
df_int_short$LOCATION[df_int_short$LOCATION == "DEU"] = "DE"
df_int_short$LOCATION[df_int_short$LOCATION == "GRC"] = "EL"
df_int_short$LOCATION[df_int_short$LOCATION == "HUN"] = "HU"
df_int_short$LOCATION[df_int_short$LOCATION == "IRL"] = "IE"
df_int_short$LOCATION[df_int_short$LOCATION == "ISL"] = "IS"
df_int_short$LOCATION[df_int_short$LOCATION == "ITA"] = "IT"
df_int_short$LOCATION[df_int_short$LOCATION == "LVA"] = "LV"
df_int_short$LOCATION[df_int_short$LOCATION == "LTU"] = "LT"
df_int_short$LOCATION[df_int_short$LOCATION == "LUX"] = "LU"
df_int_short$LOCATION[df_int_short$LOCATION == "NOR"] = "NO"
df_int_short$LOCATION[df_int_short$LOCATION == "POL"] = "PL"
df_int_short$LOCATION[df_int_short$LOCATION == "SVK"] = "SK"
df_int_short$LOCATION[df_int_short$LOCATION == "SVN"] = "SI"
df_int_short$LOCATION[df_int_short$LOCATION == "ESP"] = "ES"
df_int_short$LOCATION[df_int_short$LOCATION == "SWE"] = "SE"
df_int_short$LOCATION[df_int_short$LOCATION == "CHE"] = "CH"
df_int_short$LOCATION[df_int_short$LOCATION == "GRB"] = "UK"

colnames(df_int_short)[colnames(df_int_short) == 'LOCATION'] <- 'geo'
colnames(df_int_short)[colnames(df_int_short) == 'Time'] <- 'time'
df_int_short <- rename(df_int_short,short_int=ObsValue)
dat_gfcf <- merge(dat_gfcf,data_gdp_deflator15, by = c("geo","time"), all=TRUE)
dat_gfcf <- dat_gfcf %>% 
  mutate(gfcf = if_else(gfcf == "NA",  0,100*gfcf/deflator15))



# GFCF, private investment 

# Get dimension code of IFS dataset
IFS.available.codes <- DataStructureMethod("IFS")
# Available dimension code
names(IFS.available.codes)

#  732         NFI_XDC  Gross fixed capital formation 

databaseID <- "IFS"
checkquery = FALSE 
startdate = "1995-01-01"
enddate = "2022-12-31"
queryfilter <- list(CL_FREQ = "A",  CL_AREA_IFS = countries, CL_INDICATOR_IFS = "NFI_XDC")
CodeSearch(IFS.available.codes, "CL_AREA_IFS")

data_imf <- IMFData::DataStructureMethod("IFS")
GR.NGDP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                   checkquery,tidy = TRUE)
GR.NGDP.query[, 1:5]


databaseID <- "IFS"
startdate = "2001-01-01"
enddate = "2016-12-31"
checkquery = FALSE

## Germany, Norminal GDP in Euros, Norminal GDP in National Currency
queryfilter <- list(CL_FREA = "", CL_AREA_IFS = "GR", CL_INDICATOR_IFS = c("NGDP_EUR", 
                                                                           "NGDP_XDC"))
GR.NGDP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                   checkquery,tidy=TRUE)
colnames(GR.NGDP.query)[colnames(GR.NGDP.query) == '@REF_AREA'] <- 'geo'
colnames(GR.NGDP.query)[colnames(GR.NGDP.query) == '@TIME_PERIOD'] <- 'time'
colnames(GR.NGDP.query)[colnames(GR.NGDP.query)=='@OBS_VALUE'] <- 'gfcf'

dim(GR.NGDP.query)

data_gfcf <- subset(data_gfcf,select = c(geo,time,gfcf))
data_gfcf <- merge(data_gfcf,data_gdp_deflator15,by = c("geo", "time"), all = TRUE)
data_gfcf <- data_gfcf %>% 
  mutate(gfcf = if_else(gfcf == "NA",  0, 100*gfcf/deflator15))
data_gfcf$gfcf <- as.numeric(data_gfcf$gfcf)


# Search code contains GDP
CodeSearch(IFS.available.codes, "CL_INDICATOR_IFS", "Gross Fixed Capital Formation")


#Building all dataset
fin_cons <- subset(fin_cons,select = c(time,geo,final_cons))
inv_grant <- subset(inv_grant,select = c(time,geo,inv_grants))
data_env <- subset(data_env, select = c(time,geo,exp_env))
data_employ <- subset(data_employ,select =c(time,geo, ths_wpeople))
data_hour <- subset(data_hour,select = c(time,geo, ths_whour))
data_gdp <- subset(data_gdp,select = c(time,geo,gdp))
data_gdp_deflator15 <- subset(data_gdp_deflator15,select=c(geo,time,deflator15))
df_int <- subset(df_int,select = c(geo,time,long_int))
data_gdp_deflator15 <- subset(data_gdp_deflator15, select = c(geo,time,deflator15))


data_join <- merge(fin_cons,inv_grant, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_env, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_employ, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_hour, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_gdp, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_gdp_deflator15, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,df_int, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_gdp_deflator,by = c("geo", "time"), all = TRUE)

data_join <- merge(data_join,env_inv,by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,env_tot,by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,env_cons,by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,data_gfcf, by = c("geo", "time"), all = TRUE)
data_join <- merge(data_join,df_int_short, by = c("geo", "time"), all = TRUE)

data_join <- subset(data_join,select = -c(inv_grants,deflator15))

data_join <- subset(data_join, select = -env_invest.x)
data_join <- rename(data_join,env_invest = env_invest.y)
data_join <- subset(data_join,select = -c(freq.x,unit.x,na_item.x,freq.y,unit.y,na_item.y))
data_join <- rename(data_join,deflator15 = values.y)
data_join = data_join[rowSums(is.na(data_join))<8,]
data_join <- data_join %>% 
  mutate(deflated_GDP = if_else(gdp == "NA",  0,100*gdp/deflator15))
data_join <- data_join %>% 
  mutate(final_cons = if_else(final_cons == "NA",  0,100*final_cons/deflator15))
data_join <- data_join %>% 
  mutate(inv_grants = if_else(inv_grants == "NA",  0,100*inv_grants/deflator15))
data_join <- data_join %>% 
  mutate(exp_env = if_else(exp_env == "NA",  0,100*exp_env/deflator15))
data_join <- data_join %>% 
  mutate(va_thwokers = if_else(deflated_GDP == "NA",  0, deflated_GDP/ths_wpeople))
data_join <- data_join %>% 
  mutate(env_inv = if_else(deflated_GDP == "NA",  0, 100*env_invest/deflator15))
data_join <- data_join %>% 
  mutate(env_total = if_else(deflated_GDP == "NA",  0, 100*env_total/deflator15))
data_join <- data_join %>% 
  mutate(env_consu = if_else(deflated_GDP == "NA",  0, 100*env_consu/deflator15))
data_join <- data_join %>% 
  mutate(gfcf_pub = if_else(gfcf_pub == "NA",  0, 100*gfcf_pub/deflator15))
data_join <- data_join %>% 
  mutate(long_int = long_int/100)
data_join <- subset(data_join,select = -c(exp_env))

data_join$long_int <- as.numeric(data_join$long_int)

sapply(data_join, class)


# for writing a data.frame or list of data.frames to an xlsx file
write.xlsx(data_join, 'data_set_new.xlsx')

