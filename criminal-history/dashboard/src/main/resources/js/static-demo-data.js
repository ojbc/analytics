getRandomValue = function (min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

staticDataMap = {
  "chart1" : {
    "values": [
      {"a": "Alpha","b": getRandomValue(0, 100)},
      {"a": "Bravo","b": getRandomValue(0, 100)},
      {"a": "Charlie","b": getRandomValue(0, 100)},
      {"a": "Delta","b": getRandomValue(0, 100)},
      {"a": "Echo","b": getRandomValue(0, 100)},
      {"a": "Foxtrot","b": getRandomValue(0, 100)},
      {"a": "Golf","b": getRandomValue(0, 100)},
      {"a": "Hotel","b": getRandomValue(0, 100)},
      {"a": "India","b": getRandomValue(0, 100)}
    ]
  },
  "chart2" : {
    "values": [
      {"Character" : "Homer", "Age" : getRandomValue(20, 50)},
      {"Character" : "Marge", "Age" : getRandomValue(20, 50)},
      {"Character" : "Bart",  "Age" : getRandomValue(5, 18)},
      {"Character" : "Lisa",  "Age" : getRandomValue(5, 18)}
    ]
  }
};