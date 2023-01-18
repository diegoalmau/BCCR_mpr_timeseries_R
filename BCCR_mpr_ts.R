# Goal of the script:-----
## This script transforms into timeseries format the data extracted from the website of the Central Bank of Costa Rica of Monetary Policy Rate from 2009 to today (deleting February 29s)
## The textreadr package must be installed locally, every other package should be installed using the function install.packages('name_of_the_package")


# Install libraries -------
library(readxl)
library(textreadr)
library(tidyverse)
library(writexl)
library(plyr)


# Load data directly from the website --------
today <- format(Sys.Date(), format = '%Y/%m/%d')
url <- paste0('https://gee.bccr.fi.cr/indicadoreseconomicos/Cuadros/frmVerCatCuadro.aspx?CodCuadro=779&Idioma=1&FecInicial=2009/01/01&FecFinal=',today,'&Exportar=True&Excel=True')
raw <- read_html(url)

# Standardize dates -------
raw1 <- str_replace(raw, 'Ene', '01')
raw1 <- str_replace(raw1, 'Feb', '02')
raw1 <- str_replace(raw1, 'Mar', '03')
raw1 <- str_replace(raw1, 'Abr', '04')
raw1 <- str_replace(raw1, 'May', '05')
raw1 <- str_replace(raw1, 'Jun', '06')
raw1 <- str_replace(raw1, 'Jul', '07')
raw1 <- str_replace(raw1, 'Ago', '08')
raw1 <- str_replace(raw1, 'Set', '09')
raw1 <- str_replace(raw1, 'Oct', '10')
raw1 <- str_replace(raw1, 'Nov', '11')
raw1 <- str_replace(raw1, 'Dic', '12')


# 2009-2022 matrix-----
## Delete february 29s -------
feb_29 <- which(str_detect(raw1, '29 02'))
mar_1_str <- which(str_detect(raw1, '1 03'))-1
mar_1 <- mar_1_str[1]
raw_29 <- raw1[-seq(from = feb_29, to = mar_1)]


# Create two dataframes, past_df y fut_df -----


# Past -----
## Slice vector to get the dates before today ------------
past_ini <- which(str_detect(raw_29, format(Sys.Date(), '%Y'))) + 1
past_end <- which(str_detect(raw_29, format(Sys.Date(), '%d %m'))) + 14
past_raw <- raw_29[past_ini:past_end]

## Select years before today ------
past_first_year <- which(str_detect(raw, '2009'))
past_last_year <- which(str_detect(raw, format(Sys.Date(), format = '%Y')))
past_years1 <- raw[past_first_year:past_last_year]
past_years <- c('date', past_years1)

## Transform vector to matrix and to dataframe ------
past_matrix <- matrix(past_raw, ncol = length(past_years), byrow = TRUE)
past_df <- data.frame(past_matrix)
colnames(past_df) <- past_years


# Future -----
## Slice vector to get the dates after today -------
fut_ini <- which(str_detect(raw_29, format(Sys.Date(), '%d %m'))) + 15
fut_end <- length(raw_29)
fut_raw <- raw_29[fut_ini:fut_end]

## Select years of interest ------
fut_first_year <- which(str_detect(raw1, '2009'))
fut_last_year <- which(str_detect(raw1, format(Sys.Date(), format = '%Y'))) - 1
fut_years1 <- raw1[fut_first_year:fut_last_year]
fut_years <- c('date', fut_years1)

## Transform vector to matrix and to dataframe ------
fut_matrix <- matrix(fut_raw, ncol = length(fut_years), byrow = TRUE)
fut_df <- data.frame(fut_matrix)
colnames(fut_df) <- fut_years



# Merge past_df con fut_df -----
df <- rbind.fill(past_df,fut_df)

# Transform to timeseries format ------
ts <- pivot_longer(df, cols = colnames(df)[2:length(colnames(df))], names_to = 'year', values_to = 'mpr')

## Transform commas to dots ------
mmpr1 <- str_replace(ts$mpr, ',', '.')

## Add column with dots and commas in US format  ------
ts$mmpr <- mmpr1

## Join the column of days and months with the column of years ------
ts$joint_dates <- paste(ts$date, ts$year)

## Transform to date and numeric formats ------
ts$dates <- as.Date(ts$joint_dates, format = '%d %m %Y')
ts$mpr <- as.numeric(ts$mmpr)

## Slice dataframe to store only the variables of dates and mpr ------
final_df <- ts %>%
  select(dates, mpr) %>%
  arrange(ts$dates) %>%
  drop_na()

# Store as excel file -----
write_xlsx(final_df,"mpr.xlsx")
