This directory (and subdirectories) contain R packages, Mondrian schemas, and other components that together comprise the OJBC Booking / Jail analytics toolset.

Subdirectories contain the following artifacts:

* **db:** [SQL Power Architect](http://www.sqlpower.ca/page/architect) models for the staging and dimensional databases, and `sed` scripts that work around shortcomings in SQL PA to edit the DDL it generates
* **r-analytics:** The `JailBookingDashboard` R package, which contains functions to support creation of widget plots from rolled-up dimensional data (see the function `buildDashboardData()` in the `OJBCJailBookingLoad` package for how data are rolled-up to feed into the plot-making functions `plotBar()` and `plotTimeline()`).  This directory also contains "demostate" versions of a dashboard data package (which contains the rolled-up data) and a dashboard package (which is actually exposed via [opencpu](https://www.opencpu.org/) to provide the actual dashboard pages).
* **r-load:** Contains the `OJBCJailBookingLoad` package, which handles loading of a demostate staging database (useful for demos and testing), and also contains the staging->dimensional load functionality, and the dimensional roll-up functionality mentioned above.
* **JailBookingMondrianSchema.xml:** [Mondrian](http://community.pentaho.com/projects/mondrian/) schema that defines the Booking/Jail ROLAP dimensions and cube.

## How to get a demostate environment working

*Note: there are docker images that make it much easier to run your own demos, rather than building everything from source.  Add info here about docker, with appropriate links into the OJBC docker github repo...*

1. Install mysql, if you don't have it already
2. Install SQL Power Architect
3. Open the staging and dimensional models in the **db** directory, and generate DDL (see SQL PA help content for more info)
4. Run the `sed` commands in the **db** directory to convert the appropriate DATETIME columns to TIMESTAMP (ignore the mssql specific commands, unless you are installing on mssql)
5. In mysql, `create database ojbc_booking_staging_demo` and then execute the staging DDL into this database.  Do the same with `create database ojbc_booking_analytics_demo` for the dimensional DDL.  You now have the two (empty) databases.
6. Install the `devtools` package into your R environment
7. Use `devtools::install_github('ojbc/analytics/booking-jail/r-load/OJBCJailBookingLoad')` to install the load package.  Alternatively, you can pull the source, open the RStudio project, and build there.
8. Run `OJBCJailBookingLoad::loadDemoStaging()` to populate your staging database.  Note the various parameters (documented with roxygen) that control the load.  For example, you can choose the size of the demo jurisdiction's jail population, the length of time into the past to generate bookings, etc.
9. Run `OJBCJailBookingLoad::loadDimensionalDatabase()` to populate the dimensional database with data from the staging database you just loaded.  *Note: the staging and dimensional load functions can take awhile to run...be patient!*
10. Now that you have the dimensional database, you can setup a [Saiku](http://community.meteorite.bi/) instance and load the Mondrian schema into it, to begin exploring the data.  Continue on if you'd also like to get dashboards working.

## How to get the dashboards running locally

1. It is easiest to do the following in RStudio.  Pull the analytics repo, and open the `DemoJailBookingDashboardData` project in RStudio.  Run the BuildDashboardData.R source file.  This will read your local dimensional database and generate a bunch of summary data frames and code table data frames.  It will then build the `DemoJailBookingDashboardData` package and install it locally.  *Note: when you run the R source file, you may find that you have not yet installed some required packages; to fix this, just `install.packages(...)`*
2. Now open the `DemoJailBookingDashboard` project in RStudio.  Make sure you have installed the `opencpu` package from CRAN.  Build and Reload the `DemoJailBookingDashboard` project, and then `library(opencpu)`.  When the `opencpu` package loads, it will show you the URL where the dashboard lives.  To illustrate, if opencpu says it is running at localhost:1234/ocpu, then the dashboards will be at http://localhost:1234/ocpu/library/DemoJailBookingDashboard/www/index.html
