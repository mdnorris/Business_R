# PATIENT CONVERSION FROM FIRST VISIT TO SECOND

# PROGRAM STRUCTURE: drops all sb with missing values
# orders by patient id and date of service
# assigns number to first visit, second visit, etc
# selects all patients who have had one visit,
# then selects all patients who have had 2 (or more)
# visits. it matches those two together, then 
# substracts the first visit from the second
# a boolean column is create with 1's where the
# diff_time column is 21 or less, and 0 if one and 
# done or the second visit was not completed within 
# three weeks. subsets are taken for each month. the 
# mean of the boolean vector is the percentage of patients
# who had their first visit within a month and completed 
# a second visit within 21 days. a note of caution is that
# there are many values of 0 in the diff_time column
# which is a result of providers submitting multiple 
# superbills


library(lubridate)
library(data.table)
library(dplyr)

# strings as factors will often be necessary as an import from csv exports
# from the database
df    <- read.csv("C:/Users/MNorris/Desktop/df.csv", stringsAsFactors = FALSE)

# df    <- subset(df, select = c('patient_id', 'date_of_service'))
df    <- subset(df, date_of_service != '')

summary(df)

df$date_of_service     <- lubridate::ymd(df$date_of_service)

df    <- dplyr::arrange(df, patient_id, date_of_service)
dt    <- data.table(df)            
dt    <- dt[, sess_numb := seq_len(.N), by = patient_id]

print(quantile(dt$sess_numb, prob = seq(0,1, length = 21)))

# 95% of the data has visits of 11 or less

# this section examines how many patients have a second visit
# there will always be a 2 if there was a 3 or more also, 
# so for the purposes of this metric, it's useful 

# this is total number of patients who have had 1 session
dt1   <- subset(dt, sess_numb == 1)

# this is the total number of patients who have had at least 
# 2 sessions
dt2   <- subset(dt, sess_numb == 2)

# test shows that the number of merged by id
# is the same as dt2, so this looks good
# need to merge by x so that NA's for patients 
# who never got to a second visit are included
dt_two_visits  <- merge(dt1, dt2, by = 'patient_id', all.x = TRUE)

dt_two_visits <- dt_two_visits %>%
  rename(
    first_date_of_service = date_of_service.x,
    second_date_of_service = date_of_service.y
  )

dt_two_visits  <- subset(dt_two_visits, 
                  select = c('patient_id', 'first_date_of_service',
                             'second_date_of_service'))


dt_two_visits$diff_time  <- as.numeric(dt_two_visits$second_date_of_service -
                            dt_two_visits$first_date_of_service)

summary(dt_two_visits)
# double check for negatives on each run

dt_two_visits$only_one     <- is.na(dt_two_visits$second_date_of_service)
dt_two_visits$three_weeks  <- ifelse(dt_two_visits$diff_time <= 21, 1, 0)

# monthly subsets
jan      <- subset(dt_two_visits, first_date_of_service >= '2019-01-01' &
                     first_date_of_service <= '2019-01-31')
feb      <- subset(dt_two_visits, first_date_of_service >= '2019-02-01' &
                     first_date_of_service <= '2019-02-28')
march    <- subset(dt_two_visits, first_date_of_service >= '2019-03-01' &
                     first_date_of_service <= '2019-03-31')
april    <- subset(dt_two_visits, first_date_of_service >= '2019-04-01' &
                     first_date_of_service <= '2019-04-30')

# need to research on how mutate and coalesce work together

jan      <- jan %>%
  mutate(three_weeks = coalesce(three_weeks, 0))
feb      <- feb %>%
  mutate(three_weeks = coalesce(three_weeks, 0))
march    <- march %>%
  mutate(three_weeks = coalesce(three_weeks, 0))
april    <- april %>%
  mutate(three_weeks = coalesce(three_weeks, 0))

print(mean(jan$only_one))
print(mean(feb$only_one))
print(mean(march$only_one))
print(mean(april$only_one))
print(mean(jan$three_weeks))
print(mean(feb$three_weeks))
print(mean(march$three_weeks))
print(mean(april$three_weeks))

# jan <- subset(jan, diff_time <= 21)
# h1 <- hist(jan$diff_time, breaks = c(0:21))
# print(h1)
# 
# feb <- subset(feb, diff_time <= 21)
# h2 <- hist(feb$diff_time, breaks = c(0:21))
# print(h2)
# 
# march <- subset(march, diff_time <= 21)
# h3 <- hist(march$diff_time, breaks = c(0:21))
# print(h3)
# 
# april <- subset(april, diff_time <= 21)
# h4 <- hist(april$diff_time, breaks = c(0:21))
# print(h4)

# this is repeated without the diff time values that
# are 0 to see what the difference might be

dt1   <- subset(dt, sess_numb == 1)

# this is the total number of patients who have had at least 
# 2 sessions
dt2   <- subset(dt, sess_numb == 2)

# test shows that the number of merged by id
# is the same as dt2, so this looks good
# need to merge by x so that NA's for patients 
# who never got to a second visit are included
dt_two_visits  <- merge(dt1, dt2, by = 'patient_id', all.x = TRUE)

dt_two_visits <- dt_two_visits %>%
  rename(
    first_date_of_service = date_of_service.x,
    second_date_of_service = date_of_service.y
  )

dt_two_visits  <- subset(dt_two_visits, 
                         select = c('patient_id', 'first_date_of_service',
                                    'second_date_of_service'))


dt_two_visits$diff_time  <- as.numeric(dt_two_visits$second_date_of_service -
                                         dt_two_visits$first_date_of_service)

dt_two_visits   <- subset(dt_two_visits, diff_time != 0 | is.na(diff_time))


# double check for negatives on each run

dt_two_visits$only_one     <- is.na(dt_two_visits$second_date_of_service)
dt_two_visits$three_weeks  <- ifelse(dt_two_visits$diff_time <= 21, 1, 0)

# monthly subsets
jan      <- subset(dt_two_visits, first_date_of_service >= '2019-01-01' &
                     first_date_of_service <= '2019-01-31')
feb      <- subset(dt_two_visits, first_date_of_service >= '2019-02-01' &
                     first_date_of_service <= '2019-02-28')
march    <- subset(dt_two_visits, first_date_of_service >= '2019-03-01' &
                     first_date_of_service <= '2019-03-31')
april    <- subset(dt_two_visits, first_date_of_service >= '2019-04-01' &
                     first_date_of_service <= '2019-04-30')

# need to research on how mutate and coalesce work together

jan      <- jan %>%
  mutate(three_weeks = coalesce(three_weeks, 0))
feb      <- feb %>%
  mutate(three_weeks = coalesce(three_weeks, 0))
march    <- march %>%
  mutate(three_weeks = coalesce(three_weeks, 0))
april    <- april %>%
  mutate(three_weeks = coalesce(three_weeks, 0))

print(mean(jan$only_one))
print(mean(feb$only_one))
print(mean(march$only_one))
print(mean(april$only_one))
print(mean(jan$three_weeks))
print(mean(feb$three_weeks))
print(mean(march$three_weeks))
print(mean(april$three_weeks))

# jan <- subset(jan, diff_time <= 21)
# h1 <- hist(jan$diff_time, breaks = c(0:21))
# print(h1)
# 
# feb <- subset(feb, diff_time <= 21)
# h2 <- hist(feb$diff_time, breaks = c(0:21))
# print(h2)
# 
# march <- subset(march, diff_time <= 21)
# h3 <- hist(march$diff_time, breaks = c(0:21))
# print(h3)
# 
# april <- subset(april, diff_time <= 21)
# h4 <- hist(april$diff_time, breaks = c(0:21))
# print(h4)
