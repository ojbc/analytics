makeQueryParams = function(measure, jurisdiction, agency, targetPopulationOnly, activeOnly, daysAgo) {

  params = new Object();

  params.mondrianBookingLevelMeasure = "";
  params.mondrianChargeLevelMeasure = "";
  params.bookingLevelAxisTitle = "";
  params.chargeLevelAxisTitle = "";
  params.format = null;

  if (measure === "Population Count") {
    params.mondrianBookingLevelMeasure = "Booking Count";
    params.mondrianChargeLevelMeasure = "Charge Count";
    params.bookingLevelAxisTitle = "Bookings";
    params.chargeLevelAxisTitle = "Charges";
  } else if (measure === "Length of Stay") {
    params.mondrianBookingLevelMeasure = "Average Length Of Stay";
    params.mondrianChargeLevelMeasure = "Average Length Of Stay";
    params.bookingLevelAxisTitle = "Avg Length of Stay (Days)";
    params.chargeLevelAxisTitle = "Avg Length of Stay (Days)";
  } else if (measure === "Rebooking Rate") {
    params.mondrianBookingLevelMeasure = "OneYearRebookRate";
    params.mondrianChargeLevelMeasure = "OneYearRebookRate";
    params.bookingLevelAxisTitle = "One-Year Rebooking Rate";
    params.chargeLevelAxisTitle = "One-Year Rebooking Rate";
    params.format = "%";
  }

  params.whereJoin = "(" +
    (activeOnly ? "{[Episode Status].[Episode Status].[Active]}  *  " : "") +
    (daysAgo == null ? "" : "{[Date].[WithinPast" + daysAgo + "Days].[Y]} * ") +
    "{[ArrestAgency].[ArrestAgency].[" + agency + "]}  *  {[Jurisdiction].[JurisdictionType].[" + jurisdiction + "]}  * " +
    (targetPopulationOnly ? "{[Population].[Population].[Target Population]}" : "{[Population].[Population].[All Populations]}") +
    ")"

  return params;

}

createParamsForJurisdictionQuery = function(genericParams) {

  // have to do some extra stuff here, because Jurisdiction is one of our dimension filters.  MDX does not allow a dimension to appear on an axis and in a WHERE

  jurisdictionRegex = /(.+)(\{\[Jurisdiction\]\.\[JurisdictionType\]\.\[[^\]]+\]\})[ ]+\*(.+)/;
  tempWhere = genericParams.whereJoin.replace(jurisdictionRegex, "$1 $3");
  tempRows = "{[Jurisdiction].[JurisdictionType].[JurisdictionCategory].Members}";

  if (!(/All Jurisdictions/.test(genericParams.whereJoin))) {
    tempRows = genericParams.whereJoin.replace(jurisdictionRegex, "$2");
  }

  ret = JSON.parse(JSON.stringify(genericParams));
  ret.whereJoin = tempWhere;
  ret.dimSet = tempRows;

  return ret;

}
