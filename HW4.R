# MA615_HW4_Haochen_LI
# 09272024

rm(list=ls())
setwd("E:/Desktop/BU/2024 Fall/MA615/HW4")

library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)

# Root & Tail
file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

# Fct loop

readfct <- function(year, skp) {
  path <- paste0(file_root, year, tail)
  header <- scan(path, what='character', nlines=1)
  buoy <- fread(path, header=FALSE, skip=skp, fill = Inf)
  colnames(buoy) <- header
  return(buoy)
}

############################  1985 - 1998 ############################

# Emty
D85_98 <- list()

# Loop
for (year in 1985:1998) {
  buoy <- readfct(year, skp=1)
  buoy$date <- ymd_h(paste(buoy$YY, buoy$MM, buoy$DD, buoy$hh))
  D85_98[[as.character(year)]] <- buoy
}

# Comb
D85_98 <- rbindlist(D85_98, use.names=TRUE, fill=TRUE)

# Add TIDE
D85_98 <- D85_98 %>%
  mutate(TIDE = NA) %>%
  relocate(TIDE, .before = date)

############################  1999 ###################################

# Emty
D99 <- list()

# Loop
for (year in 1999) {
  buoy <- readfct(year, skp=1)
  buoy$date <- ymd_h(paste(buoy$YYYY, buoy$MM, buoy$DD, buoy$hh))
  D99[[as.character(year)]] <- buoy
}
# Comb
D99 <- rbindlist(D99, use.names=TRUE, fill=TRUE)

# Add TIDE
D99 <- D99 %>%
  mutate(TIDE = NA) %>%
  relocate(TIDE, .before = date)


############################  2000 - 2004 ############################

# Emty
D00_04 = list()

# Loop
for (year in 2000:2004) {
  buoy <- readfct(year, skp=1)
  buoy$date <- ymd_h(paste(buoy$YY, buoy$MM, buoy$DD, buoy$hh))
  D00_04[[as.character(year)]] <- buoy
}
# Comb
D00_04 <- rbindlist(D00_04, use.names=TRUE, fill=TRUE)

############################  2005 - 2006 #############################

# Emty
D05_06 <- list()

# Loop
for (year in 2005:2006) {
  buoy <- readfct(year, skp=1)
  buoy$date <- ymd_hm(paste(buoy$YYYY, buoy$MM, buoy$DD, buoy$hh, buoy$mm))
  D05_06[[as.character(year)]] <- buoy
}

# Comb
D05_06 <- rbindlist(D05_06, use.names=TRUE, fill=TRUE)

############################  2007 - 2023 #############################

# Emty
D07_23 <- list()

# Loop
for (year in 2007:2023) {
  buoy <- readfct(year, skp=2)
  D07_23[[as.character(year)]] <- buoy
}

# Comb
D07_23 <- rbindlist(D07_23, use.names = TRUE, fill = TRUE)

# Change first column name for date
colnames(D07_23)[1] <- 'YYYY'

# Convert - Numeric 
D07_23 <- D07_23 %>%
  mutate(across(c(YYYY, MM, DD, hh, mm), as.numeric))

# Manually create column
D07_23 <- D07_23 %>%
  mutate(date = make_datetime(YYYY, MM, DD, hh, mm))

# delete mm for Comb_final
D07_23 <- D07_23 %>% select(-mm)

############################  Final ###################################

# Loop the groups for Comb_final
# 1
rename_columns <- function(df) {
  colnames(df)[1] <- 'YYYY'
  colnames(df)[5] <- 'WDIR'
  colnames(df)[12] <- 'PRES(BAR)'
  return(df)
}
# 2
D85_98 <- rename_columns(D85_98)
D99 <- rename_columns(D99)
D00_04 <- rename_columns(D00_04)
D05_06 <- rename_columns(D05_06)
D07_23 <- rename_columns(D07_23)
# delete the mm column
D05_06 <- D05_06 %>% select(-mm)
#D07_23 <- D07_23 %>% select(-mm)


D85_23 = list()
D85_23 <- rbindlist(list(D85_98, D99, D00_04, D05_06, D07_23), 
                        use.names=TRUE, fill=TRUE)

# b ############################################################################

# Replace extreme values-NA

D85_23 <- D85_23 %>%
  mutate(YYYY = ifelse(YYYY < 100, YYYY + 1900, YYYY))%>%
  mutate(across(where(is.numeric), ~ na_if(., 99))) %>%
  mutate(across(where(is.numeric), ~ na_if(., 999))) %>%
  mutate(across(where(is.numeric), ~ na_if(., 9999)))

D85_23 <- as.data.table(D85_23)
write.csv(D85_23, "Final.csv")

