refreshJailDemographicsStaticData = function(measure, jurisdiction, agency, targetPopulationOnly) {

  params = makeQueryParams(measure, jurisdiction, agency, targetPopulationOnly, true, null);

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[Sex].[Sex].[PersonSexTypeDescription].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("PersonSexTypeDescription", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-demographics-static-r1c1").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-static-r1c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[Race].[Race].[PersonRaceTypeDescription].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("PersonRaceTypeDescription", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-demographics-static-r1c2").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-static-r1c2", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[Age].[Age].[AgeRange].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("AgeRange", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-demographics-static-r1c3").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-static-r1c3", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[Language].[Language].[LanguageTypeDescription].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("LanguageTypeDescription", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-demographics-static-r2c1").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-static-r2c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[EducationLevel].[EducationLevel].[EducationLevelTypeDescription].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("EducationLevelTypeDescription", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-demographics-static-r2c2").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-static-r2c2", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[MilitaryServiceStatus].[MilitaryServiceStatus].[MilitaryServiceStatusTypeDescription].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("MilitaryServiceStatusTypeDescription", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-demographics-static-r2c3").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-static-r2c3", s, {"actions" : false, "renderer" : "svg"});
  });

}
