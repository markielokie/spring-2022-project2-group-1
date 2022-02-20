print("Beginning.....................")

# install necessary packages
library(tidyverse)
library(dplyr)
library(corrr)
library(zoo)

source("cleaning.R")

print("Installed + loaded packages.....................")

output_folder = "../output/"

# getting all calls
calls_2020 = read.csv("../data/311_Service_Requests_2020.csv", na.strings=c("","N/A"))
calls_2020 = clean.data(calls_2020)
calls_2021 = read.csv("../data/311_Service_Requests_2021.csv", na.strings=c("","N/A"))
calls_2021 = clean.data(calls_2021)
calls_2022 = read.csv("../data/311_Service_Requests_2022.csv", na.strings=c("","N/A"))
calls_2022 = clean.data(calls_2022)

print("Loaded data.....................")

calls = rbind(calls_2020, calls_2021, calls_2022)

print("Merged data.....................")

# remove complaints with less than 1000 calls 
remove = calls %>%
  group_by(Complaint.Type) %>%
  summarise(ncalls=n()) %>%
  filter(ncalls < 1000)

remove_cols = remove$Complaint.Type
calls = calls %>%
  filter(!(Complaint.Type %in% remove_cols))

print("Filtered call data pt. 1.....................")

# get covid-related calls
covid_complaints = c("noncompliance with phased reopening", 
                     "vaccine mandate non-compliance", 
                     "mass gathering complaint", 
                     "covid-19 non-essential construction",
                     "face covering violation")

covid_calls = calls %>%
  filter(Complaint.Type %in% covid_complaints)

# SAVING COVID CALLS
write.csv(covid_calls, 
          paste0(output_folder, "covid_calls.csv"), 
          col.names = TRUE)

print("Saved covid call data.....................")

# filter all calls data to only covid dates (first day covid-related call: last day covid-related call)
first_day = covid_calls$date[1]
last_day = tail(covid_calls$date, n=1)

calls = calls %>%
  filter((date >= first_day) & (date <= last_day))

print("Filtered call data pt. 2.....................")

# SAVING ALL CALLS
write.csv(calls, 
          paste0(output_folder, "all_calls.csv"), 
          col.names = TRUE)

print("Saved all call data.....................")

# get number of calls for each complaint type in all calls data
calls_by_date_complaint = data.frame(unique(covid_calls$date)) %>% rename(date=unique.covid_calls.date.)
complaints = unique(calls$Complaint.Type)

for (complaint in complaints) {
  
  dates_df = data.frame(unique(covid_calls$date)) %>% rename(date=unique.covid_calls.date.)
  
  tmp = calls %>%
    filter(Complaint.Type == complaint) %>%
    group_by(date) %>%
    summarise(ncalls = n())
  
  first_day = tmp$date[1]
  last_day = tail(tmp$date, n=1)
  
  tmp_merge = merge(dates_df, tmp, by='date', all.x=TRUE) %>%
    mutate(ncalls = replace(ncalls, 
                            is.na(ncalls) & (date >= first_day) & (date <= last_day), 
                            0)) %>%
    mutate(ncalls_MA7 = rollmean(ncalls, k=7, fill=NA)) %>%
    select(ncalls_MA7) %>%
    rename(!!complaint := ncalls_MA7)
  
  calls_by_date_complaint = cbind(calls_by_date_complaint,
                                  tmp_merge)
}

print("Finished creating ncalls data.....................")

calls_by_date_complaint = calls_by_date_complaint %>%
  remove_rownames() %>%
  column_to_rownames(var="date")

# SAVING NCALLS BY COMPLAINT AND DATE
write.csv(calls_by_date_complaint, 
          paste0(output_folder, "ncalls_by_date_complaint.csv"), 
          row.names = TRUE,
          col.names = TRUE)

print("Saved ncalls data.....................")

# compute correlation scores and focus on covid-calls
complaint_corr = correlate(calls_by_date_complaint, use="pairwise.complete.obs", quiet = TRUE) %>% focus(covid_complaints)

complaint_corr = complaint_corr %>%
  remove_rownames() %>%
  column_to_rownames(var="term")

# SAVING CORR DF
write.csv(complaint_corr, 
          paste0(output_folder, "complaint_corr.csv"), 
          row.names = TRUE,
          col.names = TRUE)

print("Saved corr data.....................")
print("Complete.....................")
