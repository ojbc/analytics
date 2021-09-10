refreshData = function() {

    measure = $("#measureSelect").val();
    timePeriod = $("#timeSelect").val();
    axisTitle = "";
    daysAgo = "";
    format = null;

    if (measure === "Encounter Count") {
        measure = "Incident Count";
        axisTitle = "# of Incidents";
    } else if (measure === "Reoccurrence Rate") {
        axisTitle = "% of Incidents with Subsequent Incident within 6 Months";
        format = "%";
    } else {
        measure = "Subsequent Booking Rate";
        axisTitle = "% of Incidents with Subsequent Jail Booking within 6 Months";
        format = "%";
    }

    regex30Days = new RegExp('30 Days')
    regex60Days = new RegExp('60 Days')
    regex6Months = new RegExp('Six Months')
    regexYear = new RegExp('Past Year')

    if (regex30Days.test(timePeriod)) {
        daysAgo = 30;
    } else if (regex60Days.test(timePeriod)) {
        daysAgo = 60;
    } else if (regex6Months.test(timePeriod)) {
        daysAgo = 180;
    } else if (regexYear.test(timePeriod)) {
        daysAgo = 365;
    }

    daysFilter = 'WHERE {[Date].[WithinPast' + daysAgo + 'Days].[Y]}';

    getDataFor("SELECT NON EMPTY {[Measures].[" + measure + "]} ON COLUMNS, NON EMPTY {[Engagement Status].[Engagement Status].[Engagement Status].Members} ON ROWS FROM [Crisis Incidents] " +
      daysFilter,
    function(v) {
      s = makeStaticVegaSpec("Engagement Status", measure, axisTitle, format, $("#panel1").width()*.95, 350, v);
      vegaEmbed("#chart1", s, {"actions" : false, "renderer" : "svg"});
    });

    getDataFor("SELECT NON EMPTY {[Measures].[Incident Count]} ON COLUMNS, NON EMPTY {[SevereMentalIllness].[SevereMentalIllness].[SevereMentalIllnessTypeDescription].Members} ON ROWS FROM [Crisis Incidents]" +
    daysFilter,
    function(v) {
      denom = 0;
      numer = 0;
      v.values.forEach(function(vv) {denom += vv['Incident Count']});
      v.values.forEach(function(vv) {numer += (vv.SevereMentalIllnessTypeDescription === 'Yes' ? vv['Incident Count'] : 0)});
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
                    "aggregate": "sum", "field": "Incident Count", "type": "quantitative",
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
      s.width = $("#panel2").width()*.95 - $("#smi_label").width() - $("#smi_value").width() - 60;
      s.height = 50;
      vegaEmbed("#chart2_1", s, {"actions" : false, "renderer" : "svg"});
    });

    getDataFor("SELECT NON EMPTY {[Measures].[" + measure + "]} ON COLUMNS, NON EMPTY Except({[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationTypeCategory].Members}, {[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationType].[BehavioralHealthEvaluationTypeCategory].[None]}) ON ROWS FROM [Crisis Incidents]" +
    daysFilter,
    function(v) {
      s = makeStaticVegaSpec("BehavioralHealthEvaluationTypeCategory", measure, axisTitle, format, $("#panel2").width()*.95, 325, v);
      vegaEmbed("#chart2_2", s, {"actions" : false, "renderer" : "svg"});
    });

    getDataFor("SELECT NON EMPTY {[Measures].[" + measure + "]} ON COLUMNS, NON EMPTY {[CallNature].[Call Nature].[CallNatureTypeDescription].Members} ON ROWS FROM [Crisis Incidents]" +
    daysFilter,
    function(v) {
      s = makeStaticVegaSpec("CallNatureTypeDescription", measure, axisTitle, format, $("#panel3").width()*.95, 350, v);
      vegaEmbed("#chart3", s, {"actions" : false, "renderer" : "svg"});
    });

    getDataFor("SELECT NON EMPTY {[Measures].[" + measure + "]} ON COLUMNS, NON EMPTY {[DispositionLocation].[Disposition Location].[DispositionLocationTypeDescription].Members} ON ROWS FROM [Crisis Incidents]" +
    daysFilter,
    function(v) {
      s = makeStaticVegaSpec("DispositionLocationTypeDescription", measure, axisTitle, format, $("#panel4").width()*.95, 350, v);
      vegaEmbed("#chart4", s, {"actions" : false, "renderer" : "svg"});
    });

    getDataFor("SELECT NON EMPTY {[Measures].[" + measure + "]} ON COLUMNS, NON EMPTY {[Date].[WeekStartingDate].[WeekStartingDate].Members} ON ROWS FROM [Crisis Incidents]" +
    daysFilter,
    function(v) {
      var s = {
        "$schema": "https://vega.github.io/schema/vega-lite/v2.0.4.json",
        "autosize": {
            "type": "fit",
            "contains": "padding"
        },
        "mark": "line",
        "encoding": {
            "x": {"field": "WeekStartingDate", "type": "temporal", "axis": {"format": "%b %-d", "title" : ""}},
            "y": {"field": measure, "type": "quantitative", "axis": {"title" : axisTitle}}
        }
      };
      if (format != null) {
        s.encoding.y.axis.format = format;
      }
      s.data = v;
      s.width = $("#panel5").width()*.95;
      s.height = 350;
      vegaEmbed("#chart5", s, {"actions" : false, "renderer" : "svg"});
    });

}
