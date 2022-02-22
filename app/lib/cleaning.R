library(tidyverse)

clean.data <- function(
  dataframe,
  columns_lower=c("Complaint.Type", "Descriptor")
) {
  # remove cases where no descriptors
  dataframe = dataframe[rowSums(is.na(dataframe[columns_lower])) < length(columns_lower), ]

  # lowercase and get rid of duplicate
  dataframe = dataframe %>% 
    mutate_at(vars(columns_lower), funs(tolower(.))) %>%
    mutate(Descriptor = coalesce(Descriptor, Complaint.Type)) %>%
    distinct() %>%
    mutate(date = as.Date(Created.Date, format="%m/%d/%Y")) %>%
    select(date, Complaint.Type, Descriptor, Incident.Zip, Borough) %>%
    mutate(Complaint.Type = replace(Complaint.Type, 
                                    Complaint.Type=="private school vaccine mandate non-compliance", 
                                    "vaccine mandate non-compliance"))
  
  return(dataframe)
}
