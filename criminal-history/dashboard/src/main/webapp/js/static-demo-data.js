getDataFor = function(divName, callbackFunction) {
    callbackFunction(staticDataMap[divName]);
}

getRandomValue = function (min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

staticDataMap = {

  "chart1" : {
    "values": [
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 1",
            "Disposition Rate": 0.854
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 2",
            "Disposition Rate": 0.9026
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 3",
            "Disposition Rate": 0.8376
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 4",
            "Disposition Rate": 0.8682
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 5",
            "Disposition Rate": 0.8916
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 6",
            "Disposition Rate": 0.8236
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 7",
            "Disposition Rate": 0.8812
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 8",
            "Disposition Rate": 0.9036
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 9",
            "Disposition Rate": 0.879
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 10",
            "Disposition Rate": 0.9207
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 11",
            "Disposition Rate": 0.8586
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 12",
            "Disposition Rate": 0.9009
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 13",
            "Disposition Rate": 0.8697
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 14",
            "Disposition Rate": 0.8506
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 15",
            "Disposition Rate": 0.8275
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "County": "County 16",
            "Disposition Rate": 0.914
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 1",
            "Disposition Rate": 0.8353
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 2",
            "Disposition Rate": 0.791
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 3",
            "Disposition Rate": 0.7918
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 4",
            "Disposition Rate": 0.8438
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 5",
            "Disposition Rate": 0.8482
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 6",
            "Disposition Rate": 0.7838
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 7",
            "Disposition Rate": 0.8663
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 8",
            "Disposition Rate": 0.8732
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 9",
            "Disposition Rate": 0.8308
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 10",
            "Disposition Rate": 0.8939
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 11",
            "Disposition Rate": 0.8685
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 12",
            "Disposition Rate": 0.8339
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 13",
            "Disposition Rate": 0.83
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 14",
            "Disposition Rate": 0.8081
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 15",
            "Disposition Rate": 0.7813
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "County": "County 16",
            "Disposition Rate": 0.827
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 1",
            "Disposition Rate": 0.8872
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 2",
            "Disposition Rate": 0.9473
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 3",
            "Disposition Rate": 0.9326
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 4",
            "Disposition Rate": 0.9865
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 5",
            "Disposition Rate": 0.9549
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 6",
            "Disposition Rate": 0.9213
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 7",
            "Disposition Rate": 0.9716
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 8",
            "Disposition Rate": 0.9606
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 9",
            "Disposition Rate": 0.9117
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 10",
            "Disposition Rate": 0.981
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 11",
            "Disposition Rate": 0.9684
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 12",
            "Disposition Rate": 0.9632
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 13",
            "Disposition Rate": 0.9506
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 14",
            "Disposition Rate": 0.9353
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 15",
            "Disposition Rate": 0.9187
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "County": "County 16",
            "Disposition Rate": 0.9777
        }
    ]
},

  "chart2" : {
    "values": [
        {
            "Avg Days to Disposition": 289.5455,
            "CourtTypeDescription": "County 4 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 256.1088,
            "CourtTypeDescription": "County 9 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 243.1607,
            "CourtTypeDescription": "County 1 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 242.2743,
            "CourtTypeDescription": "County 14 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 239.4804,
            "CourtTypeDescription": "County 2 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 234.431,
            "CourtTypeDescription": "County 16 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 234.0925,
            "CourtTypeDescription": "County 5 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 233.7951,
            "CourtTypeDescription": "County 12 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 230.1873,
            "CourtTypeDescription": "County 2 Superior Court",
            "CourtTypeCategory": "Superior Court"
        },
        {
            "Avg Days to Disposition": 215.8251,
            "CourtTypeDescription": "County 7 Superior Court",
            "CourtTypeCategory": "Superior Court"
        }
    ]
},

  "chart3" : {
    "values": [
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "January",
            "Arrests": 1566
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "February",
            "Arrests": 1390
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "March",
            "Arrests": 1582
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "April",
            "Arrests": 1475
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "May",
            "Arrests": 1526
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "June",
            "Arrests": 1505
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "July",
            "Arrests": 1401
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "August",
            "Arrests": 1450
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "September",
            "Arrests": 1277
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "October",
            "Arrests": 1223
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "November",
            "Arrests": 1204
        },
        {
            "ChargeSeverityTypeDescription": "Felony",
            "Month of Year": "December",
            "Arrests": 1312
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "January",
            "Arrests": 11405
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "February",
            "Arrests": 10503
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "March",
            "Arrests": 12193
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "April",
            "Arrests": 12120
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "May",
            "Arrests": 12920
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "June",
            "Arrests": 12115
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "July",
            "Arrests": 12483
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "August",
            "Arrests": 11982
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "September",
            "Arrests": 10152
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "October",
            "Arrests": 9612
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "November",
            "Arrests": 8809
        },
        {
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Month of Year": "December",
            "Arrests": 9032
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "January",
            "Arrests": 1447
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "February",
            "Arrests": 1313
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "March",
            "Arrests": 1501
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "April",
            "Arrests": 1484
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "May",
            "Arrests": 1497
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "June",
            "Arrests": 1332
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "July",
            "Arrests": 1473
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "August",
            "Arrests": 1512
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "September",
            "Arrests": 1368
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "October",
            "Arrests": 1329
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "November",
            "Arrests": 1171
        },
        {
            "ChargeSeverityTypeDescription": "Mixed",
            "Month of Year": "December",
            "Arrests": 1224
        }
    ]
  },

  "chart4" : {
    "values": [
        {
            "Quarter": "1",
            "Month": "January",
            "Year": "2015",
            "Avg Days to Disposition": 235.3392,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "1",
            "Month": "February",
            "Year": "2015",
            "Avg Days to Disposition": 227.6344,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "1",
            "Month": "March",
            "Year": "2015",
            "Avg Days to Disposition": 212.8861,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "2",
            "Month": "April",
            "Year": "2015",
            "Avg Days to Disposition": 232.801,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "2",
            "Month": "May",
            "Year": "2015",
            "Avg Days to Disposition": 197.6514,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "2",
            "Month": "June",
            "Year": "2015",
            "Avg Days to Disposition": 225.7964,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "3",
            "Month": "July",
            "Year": "2015",
            "Avg Days to Disposition": 217.4651,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "3",
            "Month": "August",
            "Year": "2015",
            "Avg Days to Disposition": 207.907,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "3",
            "Month": "September",
            "Year": "2015",
            "Avg Days to Disposition": 219.64,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "4",
            "Month": "October",
            "Year": "2015",
            "Avg Days to Disposition": 202.5026,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "4",
            "Month": "November",
            "Year": "2015",
            "Avg Days to Disposition": 200.148,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "4",
            "Month": "December",
            "Year": "2015",
            "Avg Days to Disposition": 201.7202,
            "ChargeSeverityTypeDescription": "Felony"
        },
        {
            "Quarter": "1",
            "Month": "January",
            "Year": "2015",
            "Avg Days to Disposition": 118.3661,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "1",
            "Month": "February",
            "Year": "2015",
            "Avg Days to Disposition": 119.4304,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "1",
            "Month": "March",
            "Year": "2015",
            "Avg Days to Disposition": 118.4621,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "2",
            "Month": "April",
            "Year": "2015",
            "Avg Days to Disposition": 123.0397,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "2",
            "Month": "May",
            "Year": "2015",
            "Avg Days to Disposition": 129.0951,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "2",
            "Month": "June",
            "Year": "2015",
            "Avg Days to Disposition": 134.6931,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "3",
            "Month": "July",
            "Year": "2015",
            "Avg Days to Disposition": 140.0057,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "3",
            "Month": "August",
            "Year": "2015",
            "Avg Days to Disposition": 141.7318,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "3",
            "Month": "September",
            "Year": "2015",
            "Avg Days to Disposition": 146.4501,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "4",
            "Month": "October",
            "Year": "2015",
            "Avg Days to Disposition": 138.8188,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "4",
            "Month": "November",
            "Year": "2015",
            "Avg Days to Disposition": 141.4093,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "4",
            "Month": "December",
            "Year": "2015",
            "Avg Days to Disposition": 131.9069,
            "ChargeSeverityTypeDescription": "Misdemeanor"
        },
        {
            "Quarter": "1",
            "Month": "January",
            "Year": "2015",
            "Avg Days to Disposition": 205.1584,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "1",
            "Month": "February",
            "Year": "2015",
            "Avg Days to Disposition": 223.3481,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "1",
            "Month": "March",
            "Year": "2015",
            "Avg Days to Disposition": 214.1757,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "2",
            "Month": "April",
            "Year": "2015",
            "Avg Days to Disposition": 202.872,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "2",
            "Month": "May",
            "Year": "2015",
            "Avg Days to Disposition": 189.4259,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "2",
            "Month": "June",
            "Year": "2015",
            "Avg Days to Disposition": 197.3598,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "3",
            "Month": "July",
            "Year": "2015",
            "Avg Days to Disposition": 211.8436,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "3",
            "Month": "August",
            "Year": "2015",
            "Avg Days to Disposition": 202.2029,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "3",
            "Month": "September",
            "Year": "2015",
            "Avg Days to Disposition": 201.3992,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "4",
            "Month": "October",
            "Year": "2015",
            "Avg Days to Disposition": 210.9956,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "4",
            "Month": "November",
            "Year": "2015",
            "Avg Days to Disposition": 181.8227,
            "ChargeSeverityTypeDescription": "Mixed"
        },
        {
            "Quarter": "4",
            "Month": "December",
            "Year": "2015",
            "Avg Days to Disposition": 159.5272,
            "ChargeSeverityTypeDescription": "Mixed"
        }
    ]
},

"chart5" : {
    "values": [
        {
            "Quarter": "1",
            "Month": "January",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 0.7467
        },
        {
            "Quarter": "1",
            "Month": "February",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 0.7848
        },
        {
            "Quarter": "1",
            "Month": "March",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 0.7953
        },
        {
            "Quarter": "2",
            "Month": "April",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 0.7731
        },
        {
            "Quarter": "2",
            "Month": "May",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 0.773
        },
        {
            "Quarter": "2",
            "Month": "June",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 0.8777
        },
        {
            "Quarter": "3",
            "Month": "July",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 1
        },
        {
            "Quarter": "3",
            "Month": "August",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 1
        },
        {
            "Quarter": "3",
            "Month": "September",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 1
        },
        {
            "Quarter": "4",
            "Month": "October",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 1
        },
        {
            "Quarter": "4",
            "Month": "November",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 1
        },
        {
            "Quarter": "4",
            "Month": "December",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Felony",
            "Disposition Rate": 1
        },
        {
            "Quarter": "1",
            "Month": "January",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.8169
        },
        {
            "Quarter": "1",
            "Month": "February",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.8278
        },
        {
            "Quarter": "1",
            "Month": "March",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.827
        },
        {
            "Quarter": "2",
            "Month": "April",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.8131
        },
        {
            "Quarter": "2",
            "Month": "May",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.7899
        },
        {
            "Quarter": "2",
            "Month": "June",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.8306
        },
        {
            "Quarter": "3",
            "Month": "July",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.7932
        },
        {
            "Quarter": "3",
            "Month": "August",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.7915
        },
        {
            "Quarter": "3",
            "Month": "September",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.7892
        },
        {
            "Quarter": "4",
            "Month": "October",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.7895
        },
        {
            "Quarter": "4",
            "Month": "November",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.7879
        },
        {
            "Quarter": "4",
            "Month": "December",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Misdemeanor",
            "Disposition Rate": 0.79
        },
        {
            "Quarter": "1",
            "Month": "January",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 0.9266
        },
        {
            "Quarter": "1",
            "Month": "February",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 0.8916
        },
        {
            "Quarter": "1",
            "Month": "March",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 0.8985
        },
        {
            "Quarter": "2",
            "Month": "April",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 0.9259
        },
        {
            "Quarter": "2",
            "Month": "May",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 0.9153
        },
        {
            "Quarter": "2",
            "Month": "June",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 0.9427
        },
        {
            "Quarter": "3",
            "Month": "July",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 1
        },
        {
            "Quarter": "3",
            "Month": "August",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 1
        },
        {
            "Quarter": "3",
            "Month": "September",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 1
        },
        {
            "Quarter": "4",
            "Month": "October",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 1
        },
        {
            "Quarter": "4",
            "Month": "November",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 1
        },
        {
            "Quarter": "4",
            "Month": "December",
            "Year": "2015",
            "ChargeSeverityTypeDescription": "Mixed",
            "Disposition Rate": 1
        }
    ]
  },
  "chart6" : {
    "values": [
        {
            "AgencyTypeDescription": "Agency 1",
            "Avg Days to Disposition": 278.4688,
            "County": "County 1",
            "Arrests": 43
        },
        {
            "AgencyTypeDescription": "Agency 2",
            "Avg Days to Disposition": 82.5556,
            "County": "County 1",
            "Arrests": 23
        },
        {
            "AgencyTypeDescription": "Agency 3",
            "Avg Days to Disposition": 154.4423,
            "County": "County 1",
            "Arrests": 1187
        },
        {
            "AgencyTypeDescription": "Agency 4",
            "Avg Days to Disposition": 47,
            "County": "County 2",
            "Arrests": 3
        },
        {
            "AgencyTypeDescription": "Agency 5",
            "Avg Days to Disposition": 138.4831,
            "County": "County 2",
            "Arrests": 676
        },
        {
            "AgencyTypeDescription": "Agency 6",
            "Avg Days to Disposition": 149.3333,
            "County": "County 2",
            "Arrests": 128
        },
        {
            "AgencyTypeDescription": "Agency 5",
            "Avg Days to Disposition": 121.1598,
            "County": "County 2",
            "Arrests": 1090
        },
        {
            "AgencyTypeDescription": "Agency 8",
            "Avg Days to Disposition": 134.543,
            "County": "County 2",
            "Arrests": 312
        },
        {
            "AgencyTypeDescription": "Agency 9",
            "Avg Days to Disposition": 105.7269,
            "County": "County 2",
            "Arrests": 331
        },
        {
            "AgencyTypeDescription": "Agency 10",
            "Avg Days to Disposition": 156.5271,
            "County": "County 3",
            "Arrests": 561
        },
        {
            "AgencyTypeDescription": "Agency 11",
            "Avg Days to Disposition": 119.2465,
            "County": "County 3",
            "Arrests": 2135
        },
        {
            "AgencyTypeDescription": "Agency 12",
            "Avg Days to Disposition": 146.5039,
            "County": "County 3",
            "Arrests": 328
        },
        {
            "AgencyTypeDescription": "Agency 13",
            "Avg Days to Disposition": 77,
            "County": "County 3",
            "Arrests": 2
        },
        {
            "AgencyTypeDescription": "Agency 14",
            "Avg Days to Disposition": 288.5283,
            "County": "County 3",
            "Arrests": 66
        },
        {
            "AgencyTypeDescription": "Agency 15",
            "Avg Days to Disposition": 154.0028,
            "County": "County 3",
            "Arrests": 2803
        },
        {
            "AgencyTypeDescription": "Agency 16",
            "Avg Days to Disposition": 157.1062,
            "County": "County 3",
            "Arrests": 315
        },
        {
            "AgencyTypeDescription": "Agency 17",
            "Avg Days to Disposition": 188.0118,
            "County": "County 3",
            "Arrests": 768
        },
        {
            "AgencyTypeDescription": "Agency 18",
            "Avg Days to Disposition": 125.1537,
            "County": "County 3",
            "Arrests": 735
        },
        {
            "AgencyTypeDescription": "Agency 19",
            "Avg Days to Disposition": 98.5,
            "County": "County 3",
            "Arrests": 6
        },
        {
            "AgencyTypeDescription": "Agency 20",
            "Avg Days to Disposition": 148.9876,
            "County": "County 3",
            "Arrests": 1533
        },
        {
            "AgencyTypeDescription": "Agency 21",
            "Avg Days to Disposition": 229.6786,
            "County": "County 4",
            "Arrests": 107
        },
        {
            "AgencyTypeDescription": "Agency 22",
            "Avg Days to Disposition": 156.1784,
            "County": "County 4",
            "Arrests": 1568
        },
        {
            "AgencyTypeDescription": "Agency 23",
            "Avg Days to Disposition": 141.4351,
            "County": "County 5",
            "Arrests": 1530
        },
        {
            "AgencyTypeDescription": "Agency 24",
            "Avg Days to Disposition": 114.8784,
            "County": "County 5",
            "Arrests": 104
        },
        {
            "AgencyTypeDescription": "Agency 25",
            "Avg Days to Disposition": 192,
            "County": "County 5",
            "Arrests": 18
        },
        {
            "AgencyTypeDescription": "Agency 26",
            "Avg Days to Disposition": 274.5,
            "County": "County 5",
            "Arrests": 7
        },
        {
            "AgencyTypeDescription": "Agency 27",
            "Avg Days to Disposition": 147.5871,
            "County": "County 5",
            "Arrests": 1130
        },
        {
            "AgencyTypeDescription": "Agency 28",
            "Avg Days to Disposition": 137.4604,
            "County": "County 6",
            "Arrests": 4518
        },
        {
            "AgencyTypeDescription": "Agency 29",
            "Avg Days to Disposition": 95.6917,
            "County": "County 6",
            "Arrests": 200
        },
        {
            "AgencyTypeDescription": "Agency 30",
            "Avg Days to Disposition": 142.6738,
            "County": "County 6",
            "Arrests": 613
        },
        {
            "AgencyTypeDescription": "Agency 31",
            "Avg Days to Disposition": 129.5056,
            "County": "County 6",
            "Arrests": 130
        },
        {
            "AgencyTypeDescription": "Agency 32",
            "Avg Days to Disposition": 53.1587,
            "County": "County 6",
            "Arrests": 69
        },
        {
            "AgencyTypeDescription": "Agency 33",
            "Avg Days to Disposition": 156.3289,
            "County": "County 6",
            "Arrests": 1491
        }
    ]
  }
};



