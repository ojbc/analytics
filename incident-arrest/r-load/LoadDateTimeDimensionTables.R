# Unless explicitly acquired and licensed from Licensor under another license, the contents of
# this file are subject to the Reciprocal Public License ("RPL") Version 1.5, or subsequent
# versions as allowed by the RPL, and You may not copy or use this file in either source code
# or executable form, except in compliance with the terms and conditions of the RPL
# All software distributed under the RPL is provided strictly on an "AS IS" basis, WITHOUT
# WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY DISCLAIMS ALL SUCH
# WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT. See the RPL for specific language
# governing rights and limitations under the RPL.
# 
# http://opensource.org/licenses/RPL-1.5
# 
# Copyright 2012-2015 Open Justice Broker Consortium

DATE_ID_FORMAT <- "%Y%m%d"
FULL_MONTH_FORMAT <- "%m-%Y"

library(RMySQL)
library(data.table)
library(dplyr)
library(stringr)

loadDateDimensionTable <- function(conn) {
  dates <- seq(from=as.Date("2012-01-01"), to=as.Date("2032-12-31"), by="day")
  dateID <- format(dates, DATE_ID_FORMAT)
  fullMonth <- format(dates, FULL_MONTH_FORMAT)
  weekdaynames <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  Date <- data.table(DateID=dateID, CalendarDate=dates, DateMMDDYYYY=format(dates, "%m/%d/%Y"), Year=year(dates),
                     YearLabel=as.character(year(dates)), Month=month(dates), MonthName=months(dates), FullMonth=fullMonth, Day=mday(dates),
                     DayOfWeek=weekdays(dates), DayOfWeekSort=match(weekdays(dates), weekdaynames))
  Date <- mutate(Date, CalendarQuarter=as.integer(str_replace(quarters(CalendarDate), "Q([0-9])", "\\1")))
  dbSendQuery(conn, "delete from Date")
  dbWriteTable(conn, "Date", Date, append=TRUE, row.names=FALSE)
}

makeTimeID <- function(hours, minutes, seconds) {
  as.integer(paste(formatC(hours, digits=2, width=2, flag="0"), formatC(minutes, digits=2, width=2, flag="0"), formatC(seconds, digits=2, width=2, flag="0"), sep=""))
}

loadTimeDimensionTable <- function(conn) {
  
  hours <- rep(0:23, each=3600)
  minutes <- rep(rep(0:59, each=60), 24)
  seconds <- rep(0:59, 60*24)
  timeID <- makeTimeID(hours, minutes, seconds)
  
  Time <- data.table(TimeID=timeID, Hour=hours, Minute=minutes, Second=seconds)
  dbSendQuery(conn, "delete from Time")
  dbWriteTable(conn, "Time", Time, append=TRUE, row.names=FALSE)
}
