# f <- function(v) {
#   ret <- character()
#   for (vv in v) {
#     if (vv==1) {
#       ret <- c(ret, 'ha')
#     } else {
#       ret <- c(ret, 'nooooo')
#     }
#   }
#   ret
# }
#
# mapCodes <- function(sourceDf, sourceColumnName, newColumnName, lookupVector) {
#   sourceDf[, newColumnName] <- lookupVector[sourceDf[, sourceColumnName]]
#   sourceDf[is.na(sourceDf[, newColumnName]), newColumnName] <- 99999
#   sourceDf
# }
#
# f <- function() {
#   dflist <- sapply(seq(1000), function(x) {data.frame(A=seq(x), B=200*seq(x))}, simplify = FALSE)
#   bigdf <- bind_rows(dflist)
#   bigdf
# }
#
# microbenchmark::microbenchmark(f(), times=10)
#
# f <- function() {
#   dflist <- sapply(seq(1000), function(x) {data.frame(A=seq(x), B=200*seq(x))}, simplify = FALSE)
#   bigdf <- data.frame()
#   for (df in dflist) {
#     bigdf <- bind_rows(bigdf, df)
#   }
#   bigdf
# }
#
# microbenchmark::microbenchmark(f(), times=10)

#ggplot(data=pp, aes(x=President, y=ElderlyRatio, fill=PastPrime)) + geom_bar(stat="identity", position="identity") +
#  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=.25), axis.title.x = element_blank()) + scale_x_discrete(limits=pp$President) +
#  scale_y_continuous(labels=percent) + labs(y="Age at Inauguration as % of Life Expectancy", fill='"Elderly"?', title="Still Tickin' in the Oval Office: Advanced Ages of US Presidents")

# df <- data.frame(A=sample(LETTERS[1:10], size=500000, replace=T),
#                  B=sample(LETTERS[1:10], size=500000, replace=T),
#                  C=sample(LETTERS[1:10], size=500000, replace=T),
#                  D=sample(LETTERS[1:10], size=500000, replace=T),
#                  E=sample(LETTERS[1:10], size=500000, replace=T),
#                  F=sample(LETTERS[1:10], size=500000, replace=T),
#                  Z=sample(1:100, size=500000, replace=T))
#
# sdf <- df %>% group_by(A:F) %>% summarize(total=sum(Z))

# df <- data.frame(d1=c(as.Date("2016-01-01"), as.Date("2017-02-02")), ss=c('Apple','Banana'), d2=c(as.Date("2014-01-01"), as.Date("2014-02-02")))
# je <- as.data.frame(sapply(dfs$JailEpisode, function(x) {
#   ret <- x
#   if (inherits(x, "Date")) {
#     ret <- paste0("'", format(x, "%Y-%m-%d"), "'")
#   }
#   ret
# }, simplify = FALSE))
#
# df <- dfs$JailEpisode %>% mutate(Date2=EpisodeStartDate -1)
# tableName <- "JailEpisode"
# cn <- colnames(df)
# dateCols <- as.vector(sapply(df, function(col) {inherits(col, "Date")}))
# cne <- cn
# cne[dateCols] <- paste0('@', cne[dateCols])
# setString <- paste0("set ", paste0(cn[dateCols], "=str_to_date(", cne[dateCols], ", '%Y-%m-%d')", collapse=","))
# sql <- paste0("load data infile ", tempfile(tmpdir="/tmp"), "into table ", tableName, " fields terminated by '|' (",
#               paste0(cne, collapse=','), ") ", setString)

# df <- dfs$JailEpisode %>%
#   select(PersonID, EpisodeStartDate) %>%
#   inner_join(dfs$Person %>% select(PersonID, StagingPersonUniqueIdentifier), by=c("PersonID"="PersonID")) %>%
#   arrange(StagingPersonUniqueIdentifier, EpisodeStartDate) %>%
#   group_by(StagingPersonUniqueIdentifier) %>%
#   mutate(first=row_number()==1, last=row_number()==n(), recidivist=!(first & last), DaysToNextEpisode=NA, DaysSinceLastEpisode=NA)
#
# recidivistIndices <- which(df$recidivist)
#
# for (i in recidivistIndices) {
#
#   bookingDate <- df[[i, 'EpisodeStartDate']]
#   first <- df[[i, 'first']]
#   last <- df[[i, 'last']]
#   priorBookingDate <- as.Date(NA)
#   nextBookingDate <- as.Date(NA)
#
#   if (!first) {
#     priorBookingDate <- df[[i-1, "EpisodeStartDate"]]
#   }
#
#   if (!last) {
#     nextBookingDate <- df[[i+1, "EpisodeStartDate"]]
#   }
#
#   # lubridate took considerably longer
#   df[[i, 'DaysToNextEpisode']] <- as.numeric(nextBookingDate - bookingDate) # (bookingDate %--% nextBookingDate) %/% days(1)
#   df[[i, 'DaysSinceLastEpisode']] <- as.numeric(bookingDate - priorBookingDate) # (priorBookingDate %--% bookingDate) %/% days(1)
#
# }
library(lubridate)
DateDf <- data.frame(CalendarDate=seq(as.Date("2016-09-01"), as.Date("2016-09-10"), by="days")) %>%
  mutate(DateID=as.integer(format(CalendarDate, "%Y%m%d")),
         Year=year(CalendarDate),
         YearLabel=as.character(Year),
         CalendarQuarter=quarter(CalendarDate),
         Month=month(CalendarDate),
         MonthName=month(CalendarDate, label=TRUE, abbr=FALSE),
         FullMonth=format(CalendarDate, paste0(Year, "-", Month)),
         Day=day(CalendarDate),
         DayOfWeek=wday(CalendarDate, label=TRUE, abbr=FALSE),
         DayOfWeekSort=wday(CalendarDate),
         DateMMDDYYYY=format(CalendarDate, "%m%d%Y")
         )


