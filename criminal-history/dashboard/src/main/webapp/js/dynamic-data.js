CONNECTION_NAME = "cch";

chartQueryMap = {
	"chart1" : "WITH SET [~COLUMNS] AS {[Charge Severity].[Charge Severity].[ChargeSeverityTypeDescription].Members} " +
		"SET [~ROWS] AS Except({[Arresting Agency].[ArrestingAgencyByCounty].[County].Members}, {[Arresting Agency].[ArrestingAgencyByCounty].[None]}) " + 
		"SELECT NON EMPTY CrossJoin([~COLUMNS], {[Measures].[Disposition Rate]}) ON COLUMNS, NON EMPTY [~ROWS] ON ROWS FROM [Criminal Histories]",
	"chart2" : "WITH SET [~ROWS] AS TopCount({[Court].[Court by Level].[CourtTypeDescription].Members}, 10, [Measures].[Avg Days to Disposition]) " +
		"SELECT NON EMPTY {[Measures].[Avg Days to Disposition]} ON COLUMNS, NON EMPTY [~ROWS] ON ROWS FROM [Criminal Histories]",
	"chart3" : "WITH SET [~COLUMNS] AS {[Charge Severity].[Charge Severity].[ChargeSeverityTypeDescription].Members} " +
		"SET [~ROWS] AS {[ArrestDate].[Month of Year].[Month of Year].Members} " +
		"SELECT NON EMPTY CrossJoin([~COLUMNS], {[Measures].[Arrests]}) ON COLUMNS, " +
		"NON EMPTY [~ROWS] ON ROWS FROM [Criminal Histories]",
	"chart4" : "WITH SET [~COLUMNS] AS {[Charge Severity].[Charge Severity].[ChargeSeverityTypeDescription].Members} " +
		"SET [~ArrestDate_DateYMD_Year] AS {[ArrestDate].[DateYMD].[2015]} " +
		"SET [~ArrestDate_DateYMD_Month] AS Exists({[ArrestDate].[DateYMD].[Month].Members}, [~ArrestDate_DateYMD_Year]) " +
		"SET [~ROWS] AS {[~ArrestDate_DateYMD_Month]} SELECT NON EMPTY CrossJoin([~COLUMNS], {[Measures].[Avg Days to Disposition]}) ON COLUMNS, " +
		"NON EMPTY [~ROWS] ON ROWS FROM [Criminal Histories]",
	"chart5" : "WITH SET [~COLUMNS] AS {[Charge Severity].[Charge Severity].[ChargeSeverityTypeDescription].Members} " +
		"SET [~ArrestDate_DateYMD_Year] AS {[ArrestDate].[DateYMD].[2015]} " +
		"SET [~ArrestDate_DateYMD_Month] AS Exists({[ArrestDate].[DateYMD].[Month].Members}, [~ArrestDate_DateYMD_Year]) " +
		"SET [~ROWS] AS {[~ArrestDate_DateYMD_Month]} SELECT NON EMPTY CrossJoin([~COLUMNS], {[Measures].[Disposition Rate]}) ON COLUMNS, " +
		"NON EMPTY [~ROWS] ON ROWS FROM [Criminal Histories]",
	"chart6" : "WITH SET [~ROWS] AS Descendants([Arresting Agency].[ArrestingAgencyByCounty].[Statewide], , LEAVES) " +
		"SELECT NON EMPTY {[Measures].[Arrests], [Measures].[Avg Days to Disposition]} ON COLUMNS, NON EMPTY [~ROWS] ON ROWS FROM [Criminal Histories]"
};

getDataFor = function(divName, callbackFunction) {

	query = chartQueryMap[divName]

	localCallback = function(response) {
		callbackFunction(response.responseJSON);
	}

	if (query != null) {

		request = new Object();
		request.tidy = new Object();
		request.connectionName = CONNECTION_NAME;
		request.query = query;
		request.tidy.enabled = true;
		request.tidy.simplifyNames = true;

		$.ajax({
		    "url"        : "/mondrian-rest/query",
		    "dataType"   : "json",
		    "contentType": "application/json",
		    "data"       : JSON.stringify(request),
		    "type"       : "POST",
		    "complete"   : localCallback
		});

	} else {
		callbackFunction({ "values" : [] });
	}

}