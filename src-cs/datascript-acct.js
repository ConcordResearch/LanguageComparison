
var currencies = ["MXN", "USD", "EUD", "GBP", "THB"]
var numberOfAccounts = 1200000
for (var i = 0; i < numberOfAccounts ; ++i) {
  console.log(`${i+10000}|${Math.round(Math.random()*1000)} ${currencies[Math.round(Math.random()*(currencies.length - 1))]}|Test Name`)
}