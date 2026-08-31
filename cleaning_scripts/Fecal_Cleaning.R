library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(stringr)


fecal_data = read_excel('/Users/maddiethall/R Repos/SCI_data/RawData/Ring tailed lemur UMadison Aug 2026.xlsx')
fecal = fecal_data %>%
  select(`Animal ID`, Date, Time, `Fecal glucocorticoids (ng/g)`) %>%
  rename(
    animal_id = `Animal ID`,
    date = Date,
    time = Time,
    fecal_gc = `Fecal glucocorticoids (ng/g)`
  )

fecal <- fecal %>%
  mutate(
    collection_hour = NA_real_
  )

# Convert the decimal times
decimal_rows <- str_detect(fecal$time, "^\\d")

fecal$collection_hour[decimal_rows] <-
  as.numeric(fecal$time[decimal_rows]) * 24


fecal <- fecal %>%
  mutate(
    text_time = str_extract(
      time,
      "\\d{1,2}:\\d{2}(?::\\d{2})?\\s*(AM|PM)"
    )
  )

fecal <- fecal %>%
  mutate(
    parsed_time = parse_date_time(
      text_time,
      orders = c("I:M:S p", "I:M p")
    ),
    
    collection_hour = ifelse(
      !is.na(parsed_time),
      hour(parsed_time) +
        minute(parsed_time) / 60 +
        second(parsed_time) / 3600,
      collection_hour
    )
  )

fecal %>%
  summarise(
    total_rows = n(),
    missing_time = sum(is.na(collection_hour))
  )

fecal_clean = fecal %>%
  select(animal_id, date, fecal_gc, collection_hour) %>%
  rename(time = collection_hour)

saveRDS(fecal_clean, "fecal_clean.rds")
write.csv(fecal, "fecal_clean.csv", row.names = FALSE)
