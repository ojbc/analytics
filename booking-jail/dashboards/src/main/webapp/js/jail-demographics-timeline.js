refreshJailDemographicsTimelineData = function(measure, jurisdiction, agency, targetPopulationOnly, daysAgo) {

  params = makeQueryParams(measure, jurisdiction, agency, targetPopulationOnly, false, daysAgo);

  getDataFor("SELECT NON EMPTY CrossJoin({[Sex].[Sex].[PersonSexTypeDescription].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "PersonSexTypeDescription",
    $("#panel-demographics-timeline-r1c1").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-timeline-r1c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[Race].[Race].[PersonRaceTypeDescription].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "PersonRaceTypeDescription",
    $("#panel-demographics-timeline-r1c2").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-timeline-r1c2", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[Age].[Age].[AgeRange].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "AgeRange",
    $("#panel-demographics-timeline-r1c3").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-timeline-r1c3", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[Language].[Language].[LanguageTypeDescription].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "LanguageTypeDescription",
    $("#panel-demographics-timeline-r2c1").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-timeline-r2c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[EducationLevel].[EducationLevel].[EducationLevelTypeDescription].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "EducationLevelTypeDescription",
    $("#panel-demographics-timeline-r2c2").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-timeline-r2c2", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[MilitaryServiceStatus].[MilitaryServiceStatus].[MilitaryServiceStatusTypeDescription].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "MilitaryServiceStatusTypeDescription",
    $("#panel-demographics-timeline-r2c3").width()*.95, 350, v);
    vegaEmbed("#chart-demographics-timeline-r2c3", s, {"actions" : false, "renderer" : "svg"});
  });

}
