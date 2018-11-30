var currencies = ["MXN", "USD", "EUD", "GBP", "THB"]
var trans = ["Bill", "Payment"]

for (var i = 0; i < 1000000; ++i) { //accounts

  var acctNum = Math.round(Math.random() * 110000) + 5000

  var tran = trans[Math.round(Math.random() * (trans.length - 1))]
  var notes = tran === "Bill" ? "Dues" : "Online Payment";


  console.log(`${acctNum}|${Math.round(Math.random()*1000)} ${currencies[Math.round(Math.random() * (currencies.length-1))]}|${tran}|${notes}`)

//AccountNumber="12345", Amount= 200d, Bucket="Dues", Currency= Currency.EUD },
}
