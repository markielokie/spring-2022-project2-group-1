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
calls_2019 = read.csv("../data/311_Service_Requests_2019.csv", na.strings=c("","N/A"))
calls_2019 = clean.data(calls_2019)
calls_2020 = read.csv("../data/311_Service_Requests_2020.csv", na.strings=c("","N/A"))
calls_2020 = clean.data(calls_2020)
calls_2021 = read.csv("../data/311_Service_Requests_2021.csv", na.strings=c("","N/A"))
calls_2021 = clean.data(calls_2021)
calls_2022 = read.csv("../data/311_Service_Requests_2022.csv", na.strings=c("","N/A"))
calls_2022 = clean.data(calls_2022)

print("Loaded data.....................")

# Convert date column to date format
calls_2019$date = as.Date(calls_2019$date, "%m/%d/%Y")
calls_2020$date = as.Date(calls_2020$date, "%m/%d/%Y")
calls_2021$date = as.Date(calls_2021$date, "%m/%d/%Y")
calls_2022$date = as.Date(calls_2022$date, "%m/%d/%Y")

# Select date and description columns only
dat2019_new = select(calls_2019, date, Incident.Zip)
dat2020_new = select(calls_2020, date, Incident.Zip)
dat2021_new = select(calls_2021, date, Incident.Zip)
dat2022_new = select(calls_2022, date, Incident.Zip)


phase0 = rbind(filter(dat2019_new, date >= "2019-10-01"),
               filter(dat2020_new, date <= "2020-02-28"))%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase1 = filter(dat2020_new, date >= "2020-03-01" & date <= "2020-05-31")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase2 = filter(dat2020_new, date >= "2020-06-01" & date <= "2020-10-31")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase3 = rbind(filter(dat2020_new, date >= "2020-11-01"),
               filter(dat2021_new, date <= "2021-05-31"))%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase4 = filter(dat2021_new, date >= "2021-06-01" & date <= "2021-10-31")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase5 = rbind(filter(dat2021_new, date >= "2021-11-01"), dat2022_new)%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))


ncall_phase0= phase0%>%
  group_by(Incident.Zip)%>%
  summarise(phase0=n())%>%
  drop_na()

ncall_phase1= phase1%>%
  group_by(Incident.Zip)%>%
  summarise(phase1=n())%>%
  drop_na()

ncall_phase2= phase2%>%
  group_by(Incident.Zip)%>%
  summarise(phase2=n())%>%
  drop_na()

ncall_phase3= phase3%>%
  group_by(Incident.Zip)%>%
  summarise(phase3=n())%>%
  drop_na()


ncall_phase4= phase4%>%
  group_by(Incident.Zip)%>%
  summarise(phase4=n())%>%
  drop_na()

ncall_phase5= phase5%>%
  group_by(Incident.Zip)%>%
  summarise(phase5=n())%>%
  drop_na()

ncall_phase=left_join(ncall_phase0,ncall_phase1)%>%
  left_join(ncall_phase2)%>%
  left_join(ncall_phase3)%>%
  left_join(ncall_phase4)%>%
  left_join(ncall_phase5)%>%
  replace(is.na(.), 0)%>%
  mutate(phase0=0)

write.csv(ncall_phase, "..\\output\\ncall_phase_covid.csv", row.names = F)

ncalls_covid_phase = read.csv( "..\\output\\covid_calls.csv")

phase1 = filter(ncalls_covid_phase, date >= "2020-03-01" & date <= "2020-05-31")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase2 = filter(ncalls_covid_phase, date >= "2020-06-01" & date <= "2020-10-31")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase3 = filter(ncalls_covid_phase, date >= "2020-11-01" & date <= "2021-05-31")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase4 = filter(ncalls_covid_phase, date >= "2021-06-01" & date <= "2021-10-31")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))

phase5 = filter(ncalls_covid_phase, date >= "2021-11-01")%>%
  mutate(Incident.Zip=as.integer(Incident.Zip))










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
