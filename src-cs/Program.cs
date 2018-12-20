using System;
using System.Collections.Generic;
using System.Transactions;

namespace CSharpPerfEval
{
    class Program
    {
        static void Main(string[] args)
        {
            //USD,
            //MXN,
            //EUD,
            //THB,
            //GBP
            var testExchangeData = new List<ExchangeRate>()
            {
                new ExchangeRate(Currency.USD, Currency.MXN, 1.5),
                new ExchangeRate(Currency.USD, Currency.EUD, 2.5),
                new ExchangeRate(Currency.USD, Currency.THB, 3.5),
                new ExchangeRate(Currency.USD, Currency.GBP, 4.5),

                
                new ExchangeRate(Currency.MXN, Currency.EUD, 2.5),
                new ExchangeRate(Currency.MXN, Currency.THB, 3.5),
                new ExchangeRate(Currency.MXN, Currency.GBP, 4.5),

                new ExchangeRate(Currency.EUD, Currency.THB, 3.5),
                new ExchangeRate(Currency.EUD, Currency.GBP, 4.5),

                new ExchangeRate(Currency.THB, Currency.GBP, 4.5)

            };

            var converter = new CurrencyConverter(testExchangeData);

            var reader = new FileReader();

            var accounts = new AccountParser().ParseFile(reader.ReadFile("../accounts-100k.txt"));
            Console.WriteLine("AccountParser complete");
            var transactions = new TransactionParser().ParseFile(reader.ReadFile("../transactions-100k.txt"));
            Console.WriteLine("AccountParser complete");




            //var accounts = new List<Account>()
            //{
            //    new Account() { AccountNumber = "12345", BalanceAmount = 100d, BalanceCurrency = Currency.EUD, Name = "Joe Smith" },
            //    new Account() { AccountNumber = "12346", BalanceAmount = 200d, BalanceCurrency = Currency.MXN, Name = "Joe Smyth" },
            //    new Account() { AccountNumber = "12347", BalanceAmount = 300d, BalanceCurrency = Currency.GBP, Name = "Joe Herrera" },
            //    new Account() { AccountNumber = "12348", BalanceAmount = 400d, BalanceCurrency = Currency.USD, Name = "Joe Paramo" },
            //    new Account() { AccountNumber = "12349", BalanceAmount = 500d, BalanceCurrency = Currency.EUD, Name = "Joe Gutierrez" },
            //    new Account() { AccountNumber = "12350", BalanceAmount = 600d, BalanceCurrency = Currency.GBP, Name = "Joe Dominquez" },
            //    new Account() { AccountNumber = "12351", BalanceAmount = 700d, BalanceCurrency = Currency.THB, Name = "Joe Perez" }
            //};

            //var transactions = new List<Transaction>() {
            //    new Bill(){ AccountNumber="12345", Amount= 200d, Bucket="Dues", Currency= Currency.EUD },
            //    new Bill(){ AccountNumber="12346", Amount= 100d, Bucket="Dues", Currency= Currency.GBP },

            //    new Payment(){ AccountNumber="12345", Amount= 100d, Source="Online Payment", Currency= Currency.EUD},
            //    new Payment(){ AccountNumber="12345", Amount= 100d, Source="Online Payment", Currency= Currency.EUD },
            //    new Payment(){ AccountNumber="12346", Amount= 15d, Source="Online Payment", Currency= Currency.MXN },
            //};
            var processor = new Processor(converter, accounts, transactions);

            //accounts.ForEach(a => Console.WriteLine($"Account: {a.AccountNumber}, Balance: {a.BalanceAmount}, {a.BalanceCurrency.ToString()}"));

            processor.Process();

                        foreach (var account in accounts)
            {
                Console.WriteLine(account);
            }

            //accounts.ForEach(a => Console.WriteLine($"Account: {a.AccountNumber}, Balance: {a.BalanceAmount}, {a.BalanceCurrency.ToString()}"));



            //foreach (var test in testExchangeData)
            //{
            //    var fromTo = converter.Convert(test.From, test.To, 1);
            //    var toFrom = converter.Convert(test.To, test.From, 1);
            //    Console.WriteLine($"From: {test.From} To: {test.To} Result: {fromTo}");
            //    Console.WriteLine($"From: {test.To} To: {test.From} Result: {toFrom}");
            //    Console.WriteLine("------------------");
            //}

            //Console.ReadLine();

        }
    }

    //Import file into Data Structures

    public enum Currency
    {
        USD,
        MXN,
        EUD,
        THB,
        GBP
    }

    public class Account
    {
        public string AccountNumber { get; set; }
        public string Name { get; set; }
        public double BalanceAmount { get; set; }
        public Currency BalanceCurrency { get; set; }

        public override string ToString()
        {
            return $"AccountNumber: {AccountNumber}, Name: {Name}, Balance: {BalanceAmount}, Currency: {BalanceCurrency.ToString()}";
        }
    }

    public abstract class Transaction
    {
        public string AccountNumber { get; set; }
        public double Amount { get; set; }
        public Currency Currency { get; set; }
        
    }

    public class Bill : Transaction
    {
        public string Bucket { get; set; }
    }

    public class Payment : Transaction
    {
        public string Source { get; set; }
    }


    public class CurrencyConverter
    {
        public readonly  Dictionary<Currency, Dictionary<Currency,ExchangeRate>> ExchangeRates;

        public CurrencyConverter(List<ExchangeRate> rates)
        {
                                           // from              // to      // rate
            ExchangeRates = new Dictionary<Currency, Dictionary<Currency, ExchangeRate>>();

            var dict = new Dictionary<Currency, ExchangeRate>();

            foreach (var rate in rates)
            {
                if (!ExchangeRates.ContainsKey(rate.From))
                {
                    ExchangeRates.Add(rate.From, new Dictionary<Currency, ExchangeRate>());
                }

                if (!ExchangeRates.ContainsKey(rate.To))
                {
                    ExchangeRates.Add(rate.To, new Dictionary<Currency, ExchangeRate>());
                }
            }

            foreach (var rate in rates)
            {
                ExchangeRates[rate.From][rate.To] = rate;
                ExchangeRates[rate.To][rate.From] = new ExchangeRate(rate.From, rate.To, 1 / rate.Rate);
            }
        }

        public double Convert(Currency from, Currency to, double amount)
        {
            return ExchangeRates[from][to].Rate * amount;
        }
    }
    
    public class ExchangeRate
    {
        public ExchangeRate(Currency from, Currency to, double rate)
        {
            From = from;
            To = to;
            Rate = rate;
        }

        public Currency From { get; }
        public Currency To { get; }
        public double Rate { get; }
    }

    public class Processor
    {
        CurrencyConverter _converter;
        List<Account> accounts;
        List<Transaction> transactions;
        

        public Processor(CurrencyConverter converter, List<Account> accounts, List<Transaction> transactions)
        {
            _converter = converter;
            this.accounts = accounts;
            this.transactions = transactions;
        }

        public void Process()
        {
            var transactionsForAccount = new Dictionary<string, List<Transaction>>();

            foreach (var transaction in transactions)
            {
                if (!transactionsForAccount.ContainsKey(transaction.AccountNumber))
                {
                    transactionsForAccount[transaction.AccountNumber] = new List<Transaction>();
                }

                transactionsForAccount[transaction.AccountNumber].Add(transaction);
            }

            foreach(var kvp in transactionsForAccount)
            {
                var account =  accounts.Find(acc => acc.AccountNumber == kvp.Key);
                if (account != null)
                {
                    foreach (var trans in kvp.Value)
                    {
                        ApplyTransactionToAccount(account, trans);
                    }
                }
            }
        }

        
        private void ApplyTransactionToAccount(Account acct, Transaction trans)
        {
            var amount = trans.Amount;
            if (trans.Currency != acct.BalanceCurrency)
            {
                amount = _converter.Convert(trans.Currency, acct.BalanceCurrency, trans.Amount);
            }

            switch (trans)
            {
                case Bill b:
                    acct.BalanceAmount += amount;
                    break;
                case Payment p:
                    acct.BalanceAmount -= amount;
                    break;
                default:
                    throw new Exception();
            }
        } 
    }
    public class TransactionParser
    {
        public List<Transaction> ParseFile(string content)
        {
            var transactions = new List<Transaction>();
            foreach (var line in content.Split("\r\n"))
            {
                var columns = line.Split("|");
                Transaction transaction = null;
                if (TryParse(columns, ref transaction))
                {
                    transactions.Add(transaction);
                }
                else
                {
                    Console.WriteLine($"Failed to import transaction: {line}");
                }
            }

            return transactions;
        }

        // console.log(`${acctNum}|${Math.round(Math.random()*1000)} ${currencies[Math.round(Math.random() * (currencies.length-1))]}|${tran}|${notes}`)
        public bool TryParse(string[] columns, ref Transaction transaction)
        {
            if (columns.Length != 4)
                return false;

            int account;
            if (!int.TryParse(columns[0], out account))
            {
                return false;
            }

            var moneyParts = columns[1].Split(" ");
            double currencyAmount;
            Currency currencyType;
            if (moneyParts.Length != 2
                || !double.TryParse(moneyParts[0], out currencyAmount)
                || !Enum.TryParse(moneyParts[1], out currencyType))
            {
                return false;
            }

            
            if (columns[2] == "Bill")
            {
                transaction = new Bill()
                {
                    AccountNumber = account.ToString(),
                    Amount = currencyAmount,
                    Currency = currencyType,
                    Bucket = columns[3]
                };
                return true;
            }
            if (columns[2] == "Payment")
            {
                transaction = new Payment()
                {
                    AccountNumber = account.ToString(),
                    Amount = currencyAmount,
                    Currency = currencyType,
                    Source = columns[3]
                };
                return true;
            }

            return false;
        }
    }

   
    public class AccountParser
    {
        public List<Account> ParseFile(string content)
        {
            var accounts = new List<Account>();
            foreach (var line in content.Split("\r\n"))
            {
                var columns = line.Split("|");
                Account account = null;
                if (TryParse(columns, ref account))
                {
                    accounts.Add(account);
                }
                else
                {
                    Console.WriteLine($"Failed to import account: {line}");
                }
            }

            return accounts;
        }
        public bool TryParse(string[] columns, ref Account account) {
            if(columns.Length != 3)
                return false;

            int result;
            if (!int.TryParse(columns[0], out result))
            {
                return false;
            }

            var moneyParts = columns[1].Split(" ");
            double currencyAmount;
            Currency currencyType;
            if (moneyParts.Length != 2 
                || !double.TryParse(moneyParts[0], out currencyAmount)
                || !Enum.TryParse(moneyParts[1], out currencyType))
            {
                return false;
            }

            account = new Account()
            {
                AccountNumber = result.ToString(),
                BalanceAmount = currencyAmount,
                BalanceCurrency = currencyType,
                Name = columns[2]
            };

            return true;
        } 
    }

    public class FileReader
    {
        public string ReadFile(string path)
        {
            return System.IO.File.ReadAllText(path);
        }
    }
}
