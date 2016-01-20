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

# Loads the dimensional database with dummy/demo data

STATE <- "VT"

library(RMySQL)
library(data.table)
library(dplyr)
library(rgdal)
library(sp)
library(readr)
library(tidyr)
library(Hmisc)
library(stringr)
library(xlsx)

source("LoadDateTimeDimensionTables.R")
source("LoadCodeTables.R")

loadStartTime <- Sys.time()

#conn <- dbConnect(MySQL(), host="dw", dbname="ojbc_analytics_demo", username="root")
adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

dbSendQuery(adsConnection, "set foreign_key_checks=0")

# clear out fact tables
#dbSendQuery(conn, "delete from Booking")

# clear out dimension tables
#dbSendQuery(conn, "delete from YesNo")

loadCodeTables("AnalyticsCodeTables.xlsx", adsConnection)
loadDateDimensionTable(adsConnection)
loadTimeDimensionTable(adsConnection)

dbDisconnect(adsConnection)
