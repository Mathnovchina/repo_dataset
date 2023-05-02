install.packages("eurostat")
install.packages("RJSONIO")
install.packages("wbstats")
install.packages('OECD')
install.packages("usethis")

install_github("expersso/OECD")

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

# `government expenditure Totale, general gov, millions euro

filters1 = list( cofog99= "Total",
                 na_item = "TE",
                 sector = "S13",
                 unit = "MIO_EUR")
data_exp <- get_eurostat("GOV_10A_EXP", filters = filters1)
data_exp = rename(data_exp,exp_tot = values)

# National expenditure on environmental protection, total economy, millions euro

filter2 = list(sector = "S1",
               unit = "MIO_EUR")
data_env <- get_eurostat("ENV_AC_EPNEIS", filters = filter2)
data_env = rename(data_env,exp_env=values)

data_join <-merge(data_exp,data_env, by = c("geo", "time"), all = TRUE)
data_join = rename(data_join,exp_env=values.y)

# GDP 

gdp_base <- wb_search("unemployment")

# Unemployment 
oecddataset_list <- get_datasets()

dataset_list <- get_datasets()
search_dataset("unemployment", data = dataset_list)

df2 <- "MIG_NUP_RATES_GENDER"

# df2 <- subset(df2, SEX == "TT")
# data_unemp <- get_eurostat("med_ps421")
dstruc <- get_data_structure(df2)
str(dstruc, max.level = 1)
dstruc$VAR_DESC
dstruc$RATE
dstruc$GENDER

filter_list <- "U_RATE"
df <- get_dataset(dataset = df2, RATE = "U_RATE")
df_emp = subset(df, RATE == "U_RATE")
colnames(df_emp)[colnames(df_emp) == 'COUNTRY'] <- 'geo'
colnames(df_emp)[colnames(df_emp) == 'Time'] <- 'time'
data_join <-merge(data_join,df_emp, by = c("geo", "time"), all = TRUE)

df_emp <- df_emp[ , time = as.Date(time)]

df_emp$time <- as.Date(df_emp$time,format="%Y")
data_join$time <- as.Date(data_join$time,format="%Y")
mutate(data_join, time = as.Date(time, format= "%Y"))
class(Date$ArrestDate)

sapply(df_emp, class)
sapply(data_join,class)
