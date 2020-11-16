# client_referral to date_of_service
library(lubridate)
library(dplyr)
library(data.table)
library(naniar)

setwd("C:/Users/usrMain/Desktop/")
# strings as factors will often be necessary as an import from csv exports
# from the database
df     <- read.csv("C:/Users/usrMain/Desktop/df.csv", stringsAsFactors = FALSE)
df     <- subset(df, inquiry_created_time >= '2019-01-01' &
                   inquiry_created_time <= '2019-04-30')

# converts to integers and dates, but more importantly converts NULLS to NA's
df$claim_patient_id       <- as.integer(df$claim_patient_id)
df$claim_service_date     <- as.Date(as.character(df$claim_service_date), 
                                  format="%Y-%m-%d")
df$inquiry_created_time   <- as.Date(as.character(df$inquiry_created_time),
                                  format="%Y-%m-%d")
df$inquiry_accepted_time  <- as.Date(as.character(df$inquiry_accepted_time),
                                     format="%Y-%m-%d")
df$inquiry_scheduled_time <- as.Date(as.character(df$inquiry_scheduled_time),
                                     format="%Y-%m-%d")
df$inquiry_notified_time  <- as.Date(as.character(df$client_ref_notified_at),
                                     format="%Y-%m-%d")

# date differences before subsets

df$visit_diff <- as.numeric(df$claim_service_date - df$inquiry_created_time)
df$sched_diff <- as.numeric(df$inquiry_scheduled_time - df$inquiry_created_time)
df$acc_diff   <- as.numeric(df$inquiry_accepted_time - df$inquiry_created_time)
df$noti_diff  <- as.numeric(df$inquiry_notified_time - df$inquiry_created_time)

# drop all with date differences of 0 or negative values
# negative is date creation error, 0 is duplicate that occasionally
# pops up, also subsets largest interval as smaller than 21
df     <- subset(df, (visit_diff >= 0 &
                      visit_diff <= 21) |
                   is.na(visit_diff))
df     <- subset(df, sched_diff >= 0 |
                   is.na(sched_diff))
df     <- subset(df, acc_diff >= 0 |
                   is.na(acc_diff))
df     <- subset(df, noti_diff >= 0 |
                   is.na(noti_diff))

# we want only the first inquiry (the client_referral_members table
# introduces many) and a NA or first visit completed for the other
# states


# first we subset the case where inquiry number is greater than one
# but at least one of the differences is non-zero and it is their 
# first visit. this is a result of double inquiries created in the
# members referral table
# temp1  <- subset(df, inquiry_number != 1 & (!is.na(visit_diff) |
#                                             !is.na(sched_diff) |
#                                             !is.na(acc_diff) |
#                                             !is.na(noti_diff) &
#                                             patient_visit_number == 1))

# next we subset for the cases where the inquiry number does equal one
# and either it is the patients first visit or the inquiry never resulted
# in a first visit. then we merge them vertically
# temp2  <- subset(df, inquiry_number == 1 & (patient_visit_number == 1 |
#                                             is.na(claim_patient_id)))
# temp3  <- subset(df, patient_visit_number == 1)
# temp4  <- subset(df, is.na(claim_patient_id) & inquiry_number == 1)
# summary(temp3)
# summary(temp4)
# df  <- rbind(temp3, temp4)

write.csv(df, file = "test.csv", row.names = FALSE)

# METRICS FINAL CALCULATIONS
# conversion percentages
visit   <- ((nrow(df) - nrow(df[is.na(df$claim_service_date),]))
                / nrow(df))
sched   <- ((nrow(df) - nrow(df[is.na(df$inquiry_scheduled_time),]))
                / nrow(df))
accep   <- ((nrow(df) - nrow(df[is.na(df$inquiry_accepted_time),]))
              / nrow(df)) 
notif   <- ((nrow(df) - nrow(df[is.na(df$inquiry_notified_time),]))
               / nrow(df)) 
# counts
total_count  <- nrow(df) 
visit_count  <- nrow(df) - nrow(df[is.na(df$claim_service_date),])
sched_count  <- nrow(df) - nrow(df[is.na(df$inquiry_scheduled_time),])
accep_count  <- nrow(df) - nrow(df[is.na(df$inquiry_accepted_time),])
notif_count  <- nrow(df) - nrow(df[is.na(df$inquiry_notified_time),])

# averages of time differences

visit_mean   <- mean(df$visit_diff, na.rm = TRUE)
sched_mean   <- mean(df$sched_diff, na.rm = TRUE)
accep_mean   <- mean(df$acc_diff, na.rm = TRUE)
notif_mean   <- mean(df$noti_diff, na.rm = TRUE)

print((sum(!is.na(df$visit_diff))))
print(nrow(df[!is.na(df$claim_service_date),]))
# subsets for monthly analysis
jan    <- subset(df, inquiry_created_time <= '2019-01-31' &
                   inquiry_created_time >= '2019-01-01')
feb    <- subset(df, inquiry_created_time <= '2019-02-28' &
                   inquiry_created_time >= '2019-02-01')
mar    <- subset(df, inquiry_created_time <= '2019-03-31' &
                   inquiry_created_time >= '2019-03-01')
apr    <- subset(df, inquiry_created_time <= '2019-04-30' &
                   inquiry_created_time >= '2019-04-01')

# inquiry to first visit, scheduled, accepted, and notified
# conversion percentages
jan_visit   <- ((nrow(jan) - nrow(jan[is.na(jan$claim_service_date),]))
               / nrow(jan))
feb_visit   <- ((nrow(feb) - nrow(feb[is.na(feb$claim_service_date),]))
               / nrow(feb))
mar_visit   <- ((nrow(mar) - nrow(mar[is.na(mar$claim_service_date),]))
               / nrow(mar))
apr_visit   <- ((nrow(apr) - nrow(apr[is.na(apr$claim_service_date),]))
               / nrow(apr)) 

jan_sched   <- ((nrow(jan) - nrow(jan[is.na(jan$inquiry_scheduled_time),]))
                / nrow(jan))
feb_sched   <- ((nrow(feb) - nrow(feb[is.na(feb$inquiry_scheduled_time),]))
                / nrow(feb))
mar_sched   <- ((nrow(mar) - nrow(mar[is.na(mar$inquiry_scheduled_time),]))
                / nrow(mar))
apr_sched   <- ((nrow(apr) - nrow(apr[is.na(apr$inquiry_scheduled_time),]))
                / nrow(apr))

jan_acc   <- ((nrow(jan) - nrow(jan[is.na(jan$inquiry_accepted_time),]))
                / nrow(jan)) 
feb_acc   <- ((nrow(feb) - nrow(feb[is.na(feb$inquiry_accepted_time),]))
                / nrow(feb)) 
mar_acc   <- ((nrow(mar) - nrow(mar[is.na(mar$inquiry_accepted_time),]))
                / nrow(mar)) 
apr_acc   <- (((nrow(apr) - nrow(apr[is.na(apr$inquiry_accepted_time),]))
                / nrow(apr))) 
# correction for error introduced by client_referral_members table

jan_noti   <- ((nrow(jan) - nrow(jan[is.na(jan$inquiry_notified_time),]))
                / nrow(jan))
feb_noti   <- ((nrow(feb) - nrow(feb[is.na(feb$inquiry_notified_time),]))
                / nrow(feb))
mar_noti   <- ((nrow(mar) - nrow(mar[is.na(mar$inquiry_notified_time),]))
                / nrow(mar))
apr_noti   <- ((nrow(apr) - nrow(apr[is.na(apr$inquiry_notified_time),]))
                / nrow(apr))

# monthly counts

jan_total_count  <- nrow(jan)
jan_visit_count  <- nrow(jan) - nrow(jan[is.na(jan$claim_service_date),])
jan_sched_count  <- nrow(jan) - nrow(jan[is.na(jan$inquiry_scheduled_time),])
jan_accep_count  <- nrow(jan) - nrow(jan[is.na(jan$inquiry_accepted_time),])
jan_notif_count  <- nrow(jan) - nrow(jan[is.na(jan$inquiry_notified_time),])

feb_total_count  <- nrow(feb)
feb_visit_count  <- nrow(feb) - nrow(feb[is.na(feb$claim_service_date),])
feb_sched_count  <- nrow(feb) - nrow(feb[is.na(feb$inquiry_scheduled_time),])
feb_accep_count  <- nrow(feb) - nrow(feb[is.na(feb$inquiry_accepted_time),])
feb_notif_count  <- nrow(feb) - nrow(feb[is.na(feb$inquiry_notified_time),])

mar_total_count  <- nrow(mar)
mar_visit_count  <- nrow(mar) - nrow(mar[is.na(mar$claim_service_date),])
mar_sched_count  <- nrow(mar) - nrow(mar[is.na(mar$inquiry_scheduled_time),])
mar_accep_count  <- nrow(mar) - nrow(mar[is.na(mar$inquiry_accepted_time),])
mar_notif_count  <- nrow(mar) - nrow(mar[is.na(mar$inquiry_notified_time),])

apr_total_count  <- nrow(apr)
apr_visit_count  <- nrow(apr) - nrow(apr[is.na(apr$claim_service_date),])
apr_sched_count  <- nrow(apr) - nrow(apr[is.na(apr$inquiry_scheduled_time),])
apr_accep_count  <- nrow(apr) - nrow(apr[is.na(apr$inquiry_accepted_time),])
apr_notif_count  <- nrow(apr) - nrow(apr[is.na(apr$inquiry_notified_time),])

# days between states monthly average

jan_visit_mean   <- mean(jan$visit_diff, na.rm = TRUE)
jan_sched_mean   <- mean(jan$sched_diff, na.rm = TRUE)
jan_accep_mean   <- mean(jan$acc_diff, na.rm = TRUE)
jan_notif_mean   <- mean(jan$noti_diff, na.rm = TRUE)

feb_visit_mean   <- mean(feb$visit_diff, na.rm = TRUE)
feb_sched_mean   <- mean(feb$sched_diff, na.rm = TRUE)
feb_accep_mean   <- mean(feb$acc_diff, na.rm = TRUE)
feb_notif_mean   <- mean(feb$noti_diff, na.rm = TRUE)

mar_visit_mean   <- mean(mar$visit_diff, na.rm = TRUE)
mar_sched_mean   <- mean(mar$sched_diff, na.rm = TRUE)
mar_accep_mean   <- mean(mar$acc_diff, na.rm = TRUE)
mar_notif_mean   <- mean(mar$noti_diff, na.rm = TRUE)

apr_visit_mean   <- mean(apr$visit_diff, na.rm = TRUE)
apr_sched_mean   <- mean(apr$sched_diff, na.rm = TRUE)
apr_accep_mean   <- mean(apr$acc_diff, na.rm = TRUE)
apr_notif_mean   <- mean(apr$noti_diff, na.rm = TRUE)



# prepares and outputs metrics as .csv
cols     <- c('Month', 'Notification Conv.', 'Accepted Conv.',
              'Scheduled Conv.', 'First Visit Conv.', 
              'Total Inquiries', 'Total Notifications',
              'Total Accepted', 'Total Scheduled', 'Total First Visits',
              'Ave. Days Until Notified', 'Ave. Days Until Accepted ',
              'Ave. Days Until Scheduled', 'Ave. Days Until First Visit')
months   <- c('January', 'February', 'March', 'April')
visits   <- c(jan_visit, feb_visit, mar_visit, apr_visit)
scheds   <- c(jan_sched, feb_sched, mar_sched, apr_sched)
accs     <- c(jan_acc, feb_acc, mar_acc, apr_acc)
notis    <- c(jan_noti, feb_noti, mar_noti, apr_noti)
tot_cnts <- c(jan_total_count, feb_total_count, mar_total_count, 
             apr_total_count)
vis_cnt  <- c(jan_visit_count, feb_visit_count, mar_visit_count, 
             apr_visit_count)
sch_cnt  <- c(jan_sched_count, feb_sched_count, mar_sched_count,
             apr_sched_count)
acc_cnt  <- c(jan_accep_count, feb_accep_count, mar_accep_count, 
             apr_accep_count)
not_cnt  <- c(jan_notif_count, feb_notif_count, mar_notif_count, 
             apr_notif_count)
vis_mn   <- c(jan_visit_mean, feb_visit_mean, mar_visit_mean, 
             apr_visit_mean)
sch_mn   <- c(jan_sched_mean, feb_sched_mean, mar_sched_mean, 
             apr_sched_mean)
acc_mn   <- c(jan_accep_mean, feb_accep_mean, mar_accep_mean, 
             apr_accep_mean)
not_mn   <- c(jan_notif_mean, feb_notif_mean, mar_notif_mean, 
             apr_notif_mean)

output <- data.frame(months, notis, accs, scheds, visits, tot_cnts,
                     not_cnt, acc_cnt, sch_cnt, vis_cnt, not_mn, acc_mn,
                     sch_mn, vis_mn                       )
names(output) <- cols

write.csv(output, file = "output.csv", row.names = FALSE)



# long-term integrate with shiny
# plot conv_rate here
# calculate other metrics - start with client_id creation
# make bar chart where bars are mix of referral sources