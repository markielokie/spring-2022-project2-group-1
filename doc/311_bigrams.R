### This R file analyzes the 311 call center inquiries
### Data is accessed from NYC OpenData: 
### https://data.cityofnewyork.us/City-Government/311-Call-Center-Inquiry/wewp-mm3p

# Load libraries
library(tidyverse)
library(tidytext)

# Increase R memory to handle large file processing
memory.limit(size = 56000)

# Load data
dat2019 = read_csv("../data/311_Call_Center_Inquiry_2019.csv")
dat2020 = read_csv("../data/311_Call_Center_Inquiry_2020.csv")
dat2021 = read_csv("../data/311_Call_Center_Inquiry_2021.csv")
dat2022 = read_csv("../data/311_Call_Center_Inquiry_2022.csv")

# Convert date column to date format
dat2019$DATE = as.Date(dat2019$DATE, "%m/%d/%Y")
dat2020$DATE = as.Date(dat2020$DATE, "%m/%d/%Y")
dat2021$DATE = as.Date(dat2021$DATE, "%m/%d/%Y")
dat2022$DATE = as.Date(dat2022$DATE, "%m/%d/%Y")

# Select date and description columns only
dat2019_new = select(dat2019, DATE, BRIEF_DESCRIPTION)
dat2020_new = select(dat2020, DATE, BRIEF_DESCRIPTION)
dat2021_new = select(dat2021, DATE, BRIEF_DESCRIPTION)
dat2022_new = select(dat2022, DATE, BRIEF_DESCRIPTION)

# Filter data into 6 major phases of pandemic, i.e.:
# Phase 0: Pre-pandemic (Oct 1, 2019 - Feb 28, 2020)
# Phase 1: Initial outbreak (Mar 1, 2020 - May 31, 2020)
# Phase 2: Cases go down  (Jun 1, 2020 - Oct 31, 2020)
# Phase 3: Cases go up + Delta variant (Nov 1, 2020 - May 31, 2021)
# Phase 4: Cases go down (Jun 1, 2021 - Oct 31, 2021)
# Phase 5: Cases go up + Omicron variant (Nov 1, 2021 - present)
phase0 = rbind(filter(dat2019_new, DATE >= "2019-10-01"),
               filter(dat2020_new, DATE <= "2020-02-28"))

phase1 = filter(dat2020_new, DATE >= "2020-03-01" & DATE <= "2020-05-31")

phase2 = filter(dat2020_new, DATE >= "2020-06-01" & DATE <= "2020-10-31")

phase3 = rbind(filter(dat2020_new, DATE >= "2020-11-01"),
               filter(dat2021_new, DATE <= "2021-05-31"))

phase4 = filter(dat2021_new, DATE >= "2021-06-01" & DATE <= "2021-10-31")

phase5 = rbind(filter(dat2021_new, DATE >= "2021-11-01"), dat2022_new)

# Remove raw data to save memory
rm(dat2019)
rm(dat2020)
rm(dat2021)
rm(dat2022)
rm(dat2019_new)
rm(dat2020_new)
rm(dat2021_new)
rm(dat2022_new)

### Phase 0
# Tokenize words into biwords for phase 0
biwords_p0 = phase0 %>%
  unnest_tokens(input = BRIEF_DESCRIPTION, output = word,
                token = "ngrams", n = 2)

# Separate words and count for phase 0
biwords_counts_p0 = biwords_p0 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  select(word1, word2) %>%
  count(word1, word2, sort = TRUE)

# Save counts of biwords to csv
write.csv(biwords_counts_p0, "..\\output\\biwords_phase0.csv", row.names = F)

# Remove biwords_p0 and raw data to save memory
rm(biwords_p0)
rm(phase0)

### Phase 1
# Tokenize words into biwords for phase 1
biwords_p1 = phase1 %>%
  unnest_tokens(input = BRIEF_DESCRIPTION, output = word,
                token = "ngrams", n = 2)

# Separate words and count for phase 1
biwords_counts_p1 = biwords_p1 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  select(word1, word2) %>%
  count(word1, word2, sort = TRUE)

# Save counts of biwords to csv
write.csv(biwords_counts_p1, "..\\output\\biwords_phase1.csv", row.names = F)

# Remove biwords_p1 and raw data to save memory
rm(biwords_p1)
rm(phase1)

### Phase 2
# Tokenize words into biwords for phase 2
biwords_p2 = phase2 %>%
  unnest_tokens(input = BRIEF_DESCRIPTION, output = word,
                token = "ngrams", n = 2)

# Separate words and count for phase 2
biwords_counts_p2 = biwords_p2 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  select(word1, word2) %>%
  count(word1, word2, sort = TRUE)

# Save counts of biwords to csv
write.csv(biwords_counts_p2, "..\\output\\biwords_phase2.csv", row.names = F)

# Remove biwords_p2 and raw data to save memory
rm(biwords_p2)
rm(phase2)

### Phase 3
# Tokenize words into biwords for phase 3
biwords_p3 = phase3 %>%
  unnest_tokens(input = BRIEF_DESCRIPTION, output = word,
                token = "ngrams", n = 2)

# Separate words and count for phase 3
biwords_counts_p3 = biwords_p3 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  select(word1, word2) %>%
  count(word1, word2, sort = TRUE)

# Save counts of biwords to csv
write.csv(biwords_counts_p3, "..\\output\\biwords_phase3.csv", row.names = F)

# Remove biwords_p3 and raw data to save memory
rm(biwords_p3)
rm(phase3)

### Phase 4
# Tokenize words into biwords for phase 4
biwords_p4 = phase4 %>%
  unnest_tokens(input = BRIEF_DESCRIPTION, output = word,
                token = "ngrams", n = 2)

# Separate words and count for phase 4
biwords_counts_p4 = biwords_p4 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  select(word1, word2) %>%
  count(word1, word2, sort = TRUE)

# Save counts of biwords to csv
write.csv(biwords_counts_p4, "..\\output\\biwords_phase4.csv", row.names = F)

# Remove biwords_p4 and raw data to save memory
rm(biwords_p4)
rm(phase4)

### Phase 5
# Tokenize words into biwords for phase 5
biwords_p5 = phase5 %>%
  unnest_tokens(input = BRIEF_DESCRIPTION, output = word,
                token = "ngrams", n = 2)

# Separate words and count for phase 5
biwords_counts_p5 = biwords_p5 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  select(word1, word2) %>%
  count(word1, word2, sort = TRUE)

# Save counts of biwords to csv
write.csv(biwords_counts_p5, "..\\output\\biwords_phase5.csv", row.names = F)

# Remove biwords_p5 and raw data to save memory
rm(biwords_p5)
rm(phase5)