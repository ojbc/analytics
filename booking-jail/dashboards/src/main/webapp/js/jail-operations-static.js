refreshJailOperationsStaticData = function(measure, jurisdiction, agency, targetPopulationOnly) {

  params = makeQueryParams(measure, jurisdiction, agency, targetPopulationOnly, true, null);

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[CaseStatus].[CaseStatusType].[CaseStatusTypeCategory].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("CaseStatusTypeCategory", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-jail-static-r1c1").width()*.95, 350, v);
    vegaEmbed("#chart-jail-static-r1c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[Booking Count]} ON COLUMNS, NON EMPTY {[SevereMentalIllness].[SevereMentalIllness].[SevereMentalIllnessTypeDescription].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    denom = 0;
    numer = 0;
    v.values.forEach(function(vv) {denom += vv['Booking Count']});
    v.values.forEach(function(vv) {numer += (vv.SevereMentalIllnessTypeDescription === 'Yes' ? vv['Booking Count'] : 0)});
    pctSMI = numer / denom;
    $("#smi_value").text(pctSMI.toLocaleString("en", {style: "percent"}));
    var s = {
        "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
        "autosize": {
            "type": "fit",
            "contains": "padding"
        },
        "transform": [
            {"calculate": "datum.SevereMentalIllnessTypeDescription == 'Yes' ? 'Yes' : 'No'", "as": "SMI"}
        ],
        "mark": "bar",
          "encoding": {
              "x": {
                  "aggregate": "sum", "field": "Booking Count", "type": "quantitative",
                  "axis": null,
                  "stack":  "normalize"
              },
              "color": {
                  "field": "SMI", "type": "nominal", "legend": null,
                  "scale": {"range": ["#d1e5f0", "#2166ac"]}
              }
        }
    };
    s.data = v;
    s.width = $("#panel-jail-static-r1c2").width()*.95 - $("#smi_label").width() - $("#smi_value").width() - 60;
    s.height = 50;
    vegaEmbed("#chart-jail-static-r1c2_1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure +
    "]} ON COLUMNS, NON EMPTY Except({[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationTypeCategory].Members}, {[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationTypeCategory].[None]}) ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("BehavioralHealthEvaluationTypeCategory", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-jail-static-r1c2").width()*.95, 325, v);
    vegaEmbed("#chart-jail-static-r1c2_2", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianChargeLevelMeasure + "]} ON COLUMNS, NON EMPTY {[Charge].[ChargeType].[ChargeTypeCategory].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("ChargeTypeCategory", params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, $("#panel-jail-static-r1c3").width()*.95, 350, v);
    vegaEmbed("#chart-jail-static-r1c3", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianChargeLevelMeasure + "]} ON COLUMNS, NON EMPTY {[Bond].[BondType].[BondTypeCategory].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("BondTypeCategory", params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, $("#panel-jail-static-r1c4").width()*.95, 350, v);
    vegaEmbed("#chart-jail-static-r1c4", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianBookingLevelMeasure + "]} ON COLUMNS, NON EMPTY {[WorkReleaseStatus].[WorkReleaseStatus].[WorkReleaseStatusTypeDescription].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("WorkReleaseStatusTypeDescription", params.mondrianBookingLevelMeasure, params.bookingLevelAxisTitle, params.format, $("#panel-jail-static-r2c1").width()*.95, 350, v);
    vegaEmbed("#chart-jail-static-r2c1", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[Booking Count]} ON COLUMNS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    JAIL_CAPACITY = 130; // capacity of the jail goes here
    census = v.values[0]["Booking Count"];
    $("#capacity_value").text(census + "/" + JAIL_CAPACITY);
    var s = {
        "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
        "autosize": {
            "type": "fit",
            "contains": "padding"
        },
        "mark": "bar",
          "encoding": {
              "x": {
                  "aggregate": "sum", "field": "Booking Count", "type": "quantitative",
                  "axis": null,
                  "stack":  "normalize"
              },
              "color": {
                  "field": "c", "type": "nominal", "legend": null,
                  "scale": {"range": ["#d1e5f0", "#2166ac"]}
              }
        }
    };
    v.values[0].c = "Census";
    v.values[1] = {"c": "Capacity", "Booking Count": JAIL_CAPACITY - census};
    s.data = v;
    s.width = $("#panel-jail-static-r2c2").width()*.95 - $("#capacity_label").width() - $("#capacity_value").width() - 80;
    s.height = 50;
    vegaEmbed("#chart-jail-static-r2c2_capacity", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianChargeLevelMeasure + "]} ON COLUMNS, " +
    "NON EMPTY Except({[HoldForAgency].[ArrestAgency].[AgencyCategory].Members}, {[HoldForAgency].[ArrestAgency].[AgencyCategory].[Other]}) ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("AgencyCategory", params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, $("#panel-jail-static-r2c2").width()*.45, 280, v);
    vegaEmbed("#chart-jail-static-r2c2_left", s, {"actions" : false, "renderer" : "svg"});
  });

  jurisdictionParams = createParamsForJurisdictionQuery(params);

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianChargeLevelMeasure + "]} ON COLUMNS, NON EMPTY " + jurisdictionParams.dimSet + " ON ROWS FROM [Jail-Booking-Analytics] WHERE " + jurisdictionParams.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("JurisdictionCategory", params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, $("#panel-jail-static-r2c2").width()*.45, 280, v);
    vegaEmbed("#chart-jail-static-r2c2_right", s, {"actions" : false, "renderer" : "svg"});
  });

  getDataFor("SELECT NON EMPTY {[Measures].[" + params.mondrianChargeLevelMeasure + "]} ON COLUMNS, NON EMPTY {[ChargeDisposition].[ChargeDispositionType].[ChargeDispositionTypeCategory].Members} ON ROWS FROM [Jail-Booking-Analytics] WHERE " + params.whereJoin,
  function(v) {
    s = makeStaticVegaSpec("ChargeDispositionTypeCategory", params.mondrianChargeLevelMeasure, params.chargeLevelAxisTitle, params.format, $("#panel-jail-static-r2c3").width()*.95, 350, v);
    vegaEmbed("#chart-jail-static-r2c3", s, {"actions" : false, "renderer" : "svg"});
  });

}
