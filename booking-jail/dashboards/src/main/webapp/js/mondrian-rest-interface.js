CONNECTION_NAME = "JailBooking";

getDataFor = function(query, callbackFunction) {

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
