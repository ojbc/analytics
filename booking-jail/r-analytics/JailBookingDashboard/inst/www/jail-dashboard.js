var localTesting = false;

var sliderStops = ['30','60','90','180','365','730'];

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

chartNameRFunctionLookup["operations_timeline_r1c1"] = "plotTimelineCaseStatus";
chartNameRFunctionLookup["operations_timeline_r1c2"] = "plotTimelineIllnessDisorder";
chartNameRFunctionLookup["operations_timeline_r1c3"] = "plotTimelineChargeType";
chartNameRFunctionLookup["operations_timeline_r2c1"] = "plotTimelineOriginatingAgency";
chartNameRFunctionLookup["operations_timeline_r2c2"] = "plotTimelineBedType";
chartNameRFunctionLookup["operations_timeline_r2c3"] = "plotTimelinePretrialStatus";

chartNameRFunctionLookup["demographics_timeline_r1c1"] = "plotTimelineGender";
chartNameRFunctionLookup["demographics_timeline_r1c2"] = "plotTimelineRace";
chartNameRFunctionLookup["demographics_timeline_r1c3"] = "plotTimelineAge";
chartNameRFunctionLookup["demographics_timeline_r1c4"] = "plotTimelineLanguage";
chartNameRFunctionLookup["demographics_timeline_r2c1"] = "plotTimelineIncome";
chartNameRFunctionLookup["demographics_timeline_r2c2"] = "plotTimelineEducation";
chartNameRFunctionLookup["demographics_timeline_r2c3"] = "plotTimelineHousingStatus";

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
	ret = new Object();
	ret.jurisdiction = $("#jurisdiction-select").val();
	ret.originatingAgency = $("#agency-select").val();
	ret.targetPopulationOnly = ($('#pop-button-group button.btn.active').attr('id') == "target-pop-button");
	var selectedDashboardLabel = $("#dashboard-select").val();
	if (selectedDashboardLabel.includes("timeline")) {
		stop = $("#slider").val();
		ret.periodFilterDays = Number.parseInt(sliderStops[stop-1]);
	}
	console.log("Query controls updated, values=" + JSON.stringify(ret));
	return ret;
}

$(document).ready(function () {

	$("#slider").slider({ticks_labels: sliderStops});

	$('#slider').on('slideStop', function(e) {
		var t = e.value;
		var v = sliderStops[t-1];
		drawAllPanels(collectQueryArgs());
	});
	
	$(".glyphicon-info-sign").click(function() {
		var iconSpan = $(this);
		var helpFile = iconSpan.data("help-file");
		var title = $(".panel-title", iconSpan.parent()).text();
		$(".modal-title").text("About " + title);
		if (helpFile) {
			$(".modal-body").load(helpFile);
		} else {
			$(".modal-body").html("<p>Help content to be determined...</p>");
		}
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
		$(':selected', this).tab('show');
		if (this.value.includes("timeline")) {
			$(".timeline-slider").show();
			v = $("#slider").val();
			$("#slider").slider('refresh');
			$("#slider").slider('setValue', Number.parseInt(v));
		}
		else {
			$(".timeline-slider").hide();
		}
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
