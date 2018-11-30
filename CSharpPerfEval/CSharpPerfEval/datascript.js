var currencies = ["MXN", "USD", "EUD", "GBP", "THB"]
for (var i = 10000; i < 110000; ++i) {
  console.log(`${i}|${Math.round(Math.random()*1000)} ${currencies[Math.round(Math.random()*(currencies.length - 1))]}|Test Name`)
}