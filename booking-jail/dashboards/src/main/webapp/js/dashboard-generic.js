makeStaticVegaSpec = function(yAxisField, xAxisField, xAxisTitle, xAxisFormat, width, height, data) {
  s = {
      "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
      "autosize": {
          "type": "fit",
          "contains": "padding"
      },
      "mark": "bar",
      "encoding": {
          "y": {"field": yAxisField, "type": "nominal", "axis" : {"title" : ""}},
          "x": {"field": xAxisField, "type": "quantitative", "axis" : {"title" : xAxisTitle}}
      }
  };
  if (xAxisFormat != null) {
    s.encoding.x.axis.format = xAxisFormat;
  }
  s.data = data;
  s.width = width;
  s.height = height;
  return s;
}

makeTimelineVegaSpec = function(yAxisField, yAxisTitle, yAxisFormat, colorField, width, height, data) {
  s = {
    "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
    "autosize": {
      "type": "fit",
      "contains": "padding"
    },
    "mark": "line",
    "encoding": {
      "color": {"field": colorField, "type": "nominal", "legend" : {"title" : ""}},
      "x": {"field": "WeekStartingDate", "type": "temporal", "axis": {"format": "%b %-d", "title" : ""}},
      "y": {"field": yAxisField, "type": "quantitative", "axis" : {"title" : yAxisTitle}}
    }
  };
  if (yAxisFormat != null) {
    s.encoding.y.axis.format = yAxisFormat;
  }
  s.data = data;
  s.width = width;
  s.height = height;
  return s;
}
