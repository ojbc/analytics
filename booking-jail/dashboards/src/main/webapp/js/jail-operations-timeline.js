refreshJailOperationsTimelineData = function(measure, jurisdiction, agency, targetPopulationOnly, daysAgo) {

  params = makeQueryParams(measure, jurisdiction, agency, targetPopulationOnly, false, daysAgo);

  getDataFor("SELECT NON EMPTY CrossJoin({[CaseStatus].[CaseStatusType].[CaseStatusTypeCategory].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "CaseStatusTypeCategory",
    $("#panel-jail-timeline-r1c1").width()*.95, 400, v);
    vegaEmbed("#chart-jail-timeline-r1c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin(Except({[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationTypeCategory].Members}, {[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationTypeCategory].[None]}), {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "BehavioralHealthEvaluationTypeCategory",
    $("#panel-jail-timeline-r1c2").width()*.95, 400, v);
    vegaEmbed("#chart-jail-timeline-r1c2", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[Charge].[ChargeType].[ChargeTypeCategory].Members}, {[Measures].[" +
  params.mondrianChargeLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, "ChargeTypeCategory",
    $("#panel-jail-timeline-r1c3").width()*.95, 400, v);
    vegaEmbed("#chart-jail-timeline-r1c3", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[Bond].[BondType].[BondTypeCategory].Members}, {[Measures].[" +
  params.mondrianChargeLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, "BondTypeCategory",
    $("#panel-jail-timeline-r1c4").width()*.95, 350, v);
    vegaEmbed("#chart-jail-timeline-r1c4", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[WorkReleaseStatus].[WorkReleaseStatus].[WorkReleaseStatusTypeDescription].Members}, {[Measures].[" +
  params.mondrianBookingLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, "WorkReleaseStatusTypeDescription",
    $("#panel-jail-timeline-r2c1").width()*.95, 350, v);
    vegaEmbed("#chart-jail-timeline-r2c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[HoldForAgency].[ArrestAgency].[AgencyCategory].Members}, {[Measures].[" +
  params.mondrianChargeLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, "AgencyCategory",
    $("#panel-jail-timeline-r2c2").width()*.95, 350, v);
    vegaEmbed("#chart-jail-timeline-r2c2", s, {"actions" : false, "renderer" : "svg"});
  });

  jurisdictionParams = createParamsForJurisdictionQuery(params);

  getDataFor("SELECT NON EMPTY CrossJoin(" + jurisdictionParams.dimSet + ", {[Measures].[" +
  params.mondrianChargeLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  jurisdictionParams.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, "JurisdictionCategory",
    $("#panel-jail-timeline-r2c3").width()*.95, 350, v);
    vegaEmbed("#chart-jail-timeline-r2c3", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY CrossJoin({[ChargeDisposition].[ChargeDispositionType].[ChargeDispositionTypeCategory].Members}, {[Measures].[" +
  params.mondrianChargeLevelMeasure + "]}) ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " +
  params.whereJoin,
  function(v) {
    s = makeTimelineVegaSpec(params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, "ChargeDispositionTypeCategory",
    $("#panel-jail-timeline-r2c4").width()*.95, 350, v);
    vegaEmbed("#chart-jail-timeline-r2c4", s, {"actions" : false, "renderer" : "svg"});
  });

}
