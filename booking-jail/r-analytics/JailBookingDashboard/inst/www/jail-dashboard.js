var localTesting = false;

var chartNameRFunctionLookup = new Array();
chartNameRFunctionLookup["operations_static_r1c1"] = "plotStaticCaseStatus";
chartNameRFunctionLookup["operations_static_r1c2"] = "plotStaticIllnessDisorder";
chartNameRFunctionLookup["operations_static_r1c2_SMI"] = "plotStaticSMI";
chartNameRFunctionLookup["operations_static_r1c3"] = "plotStaticChargeType";
chartNameRFunctionLookup["operations_static_r2c1_left"] = "plotStaticOriginatingAgency";
chartNameRFunctionLookup["operations_static_r2c1_right"] = "plotStaticBedType";
chartNameRFunctionLookup["operations_static_r2c1_util"] = "plotStaticJailUtilization";
chartNameRFunctionLookup["operations_static_r2c2"] = "plotStaticPretrialStatus";

chartNameRFunctionLookup["demographics_static_r1c1"] = "plotStaticGender";
chartNameRFunctionLookup["demographics_static_r1c2"] = "plotStaticRace";
chartNameRFunctionLookup["demographics_static_r1c3"] = "plotStaticAge";
chartNameRFunctionLookup["demographics_static_r1c4"] = "plotStaticLanguage";
chartNameRFunctionLookup["demographics_static_r2c1"] = "plotStaticIncome";
chartNameRFunctionLookup["demographics_static_r2c2"] = "plotStaticEducation";
chartNameRFunctionLookup["demographics_static_r2c3"] = "plotStaticHousingStatus";

var RFunctionChartNameLookup = new Array();
for (var chartName in chartNameRFunctionLookup) {
	RFunctionChartNameLookup[chartNameRFunctionLookup[chartName]] = chartName;
}

var dashboardDrawAllFunctionLookup = new Array();
dashboardDrawAllFunctionLookup["jail-static"] = "allStaticOperationalDashboardPlots"
dashboardDrawAllFunctionLookup["demographics-static"] = "allStaticDemographicDashboardPlots"
dashboardDrawAllFunctionLookup["jail-timeline"] = "allTimelineOperationalDashboardPlots"
dashboardDrawAllFunctionLookup["demographics-timeline"] = "allTimelineDemographicDashboardPlots"

var ocpuCallStack = new Array();

drawImages = function(opencpuSession) {
	opencpuSession.getObject(function(rReturnValue) {
		for (i=0; i < rReturnValue.length; i++) {
			rFunction = rReturnValue[i];
			panelLabel = RFunctionChartNameLookup[rFunction];
			image = $("#img_" + panelLabel);
			image.hide();
			image.attr("src", opencpuSession.getLoc() + "files/" + rFunction + ".svg");
			image.fadeIn(1000);
		}
	});
}

drawPanel = function(panelLabel, args) {

	fn = chartNameRFunctionLookup[panelLabel];

	//console.log("Drawing panel, panelLabel=" + panelLabel + ", args=" + JSON.stringify(args) + ", function=" + fn)

	if (!localTesting) {
		//console.log("Stack length = " + ocpuCallStack.length);
		if (!ocpuCallStack.length) {
			//console.log("Showing wait pane");
			showWaitPane();
		}
		ocpuCallStack.push(panelLabel);
		ocpu.call(fn, args, drawImages)
		.fail(function(req) {
			alert("Error: " + req.responseText);
		})
		.always(function(req) {
			ocpuCallStack.pop();
			//console.log("Call complete, stack length=" + ocpuCallStack.length);
			if (!ocpuCallStack.length) {
				hideWaitPane();
			}
		});
	} else {
		console.log("Skipping call to opencpu, disabled in code.")
	}
}

drawAllPanels = function(args) {

	var selectedDashboardLabel = $("#dashboard-select").val();
	//console.log("Active dashboard is " + selectedDashboardLabel);
	var functionName = dashboardDrawAllFunctionLookup[selectedDashboardLabel];
	//console.log("Calling R Function " + functionName);

	if (!localTesting) {

		showWaitPane();

		ocpu.call(functionName, args, drawImages)
		.fail(function(req) {
			alert("Error: " + req.responseText);
		})
		.always(function(req) {
			if (!ocpuCallStack.length) {
				hideWaitPane();
			}
		});


	} else {
		console.log("Skipping call to opencpu, disabled in code.")
	}

}

showWaitPane = function() {

	var waitPaneDiv =  $("#wait-pane");

	waitPaneDiv.height($(window).height());
	waitPaneDiv.width($(window).width());

	waitPaneDiv.show();

}

hideWaitPane = function() {
	$("#wait-pane").hide();
}

collectQueryArgs = function() {
	ret = {
		jurisdiction : $("#jurisdiction-select").val(),
		originatingAgency : $("#agency-select").val(),
		targetPopulationOnly : ($('#pop-button-group button.btn.active').attr('id') == "target-pop-button")
	};
	console.log("Query controls updated, values=" + JSON.stringify(ret));
	return ret;
}

$(document).ready(function () {

	var sliderStops = ['30','60','90','180','365','730'];

	$("#slider").slider({ticks_labels: sliderStops});

	$('#slider').on('slideStop', function(e) {
		var t = e.value;
		var v = sliderStops[t-1];
		//console.log(v);
	});
	
	$('select.dashselect').on('change', function () {
		$(':selected', this).tab('show');
		if (( this.value == 'jail-timeline') || ( this.value == 'demo-timeline')) {
			$(".timeline-slider").show();
			$("#slider").slider('refresh');
		}
		else {
			$(".timeline-slider").hide();
		}
	});
	
	$("#info-icon-operations-static-r1c1").click(function() {
		$(".modal-title").text("About Case Status")
		$(".modal-body").html("<p>This is the description of the Case Status measure.</p><p>Here we will define " +
			"what it means, any data anomalies, guide to interpretation, etc.  We can do any kind of html formatting " + 
			"in here, like <b>bold</b> and <i>italics</i></p>")
		$("#infoModal").modal();
	});

	$("#pop-button-group .btn").on("click", function() {
		$("#pop-button-group .btn").button("toggle");
		drawAllPanels(collectQueryArgs());
	});

	$('.query-control').on('changed.bs.select', function (e) {
		drawAllPanels(collectQueryArgs());
	});

	$('#dashboard-select').on('changed.bs.select', function (e) {
		drawAllPanels(collectQueryArgs());
	});

	$("#img_operations_static_r1c2_SMI").on("load", function() {
		$("div .row1-panel-control-spacer").each(function() {
			$(this).height($("#smi-widget-div").height());
		}); 
	})

	$("#img_operations_static_r2c1_util").on("load", function() {
		$("div .row2-panel-control-spacer").each(function() {
			$(this).height($("#jail-util-widget-div").height());
		}); 
	})

	drawAllPanels(collectQueryArgs());

})	  	
