calendar <- data.frame(
  date = seq(as.Date('2015-05-29'),(Sys.Date()+(365*18)), 1),
  Subject = NA
)

# Day, month, year
calendar$day <- as.numeric(format(calendar$date, '%d'))
calendar$month <- as.numeric(format(calendar$date, '%m'))
calendar$year <- as.numeric(format(calendar$date, '%Y'))

# Make days
calendar$days <- 0:(nrow(calendar) - 1)

# Make weeks
calendar$weeks <- calendar$days %/% 7

# Make months
calendar$months <- -1 +
  ((calendar$year - 2015) * 12) + 
  ((calendar$month - 5)) +
  ifelse(calendar$day >= 29, 1, 0)

# Make first day of month
calendar$first_day_of_month <- format(calendar$date, '%d') == '29'

# Make days_in_previous_month
calendar$days_in_previous_month <- NA
for (i in 1:nrow(calendar)){
  month <- as.numeric(format(calendar$date[i], '%m'))
  previous_month <- ifelse(month == 1, 12,
                           month -1)
  calendar$days_in_previous_month[i] <- 
    length(calendar$date[which(calendar$year == 2016 &
                                 calendar$month == previous_month)])
}

# Make days_in_that_month
calendar$days_in_that_month <- 
  ifelse(calendar$day >= 29, calendar$day - 29,
         calendar$day + calendar$days_in_previous_month - 29)

# Make Subject
calendar$Subject <- paste0(
  calendar$months, 
  ifelse(calendar$months == 1,
         ' mes i ',
         ' messos i '),
  calendar$days_in_that_month, 
  ifelse(calendar$days_in_that_month == 1,
         ' dia',
         ' dies')
  )

# Make details
calendar$details <- paste0('mes: ', calendar$months, ' || ',
                           'setmana: ', calendar$weeks, ' || ',
                           'dia: ', calendar$days, ' || ')

####
# FINALIZE FORMAT
#####
calendar <- calendar[,c('Subject', 'date', 'details')]
names(calendar) <- c('Subject', 'Start Date', 'Description')

# Write csv
write.csv(calendar, '~/Desktop/ramona_calendar.csv', row.names = FALSE)
