using System;
using System.Collections.Generic;
using System.IO;

namespace CSharpConcPerfEval
{
    class Program
    {
        static void Main(string[] args)
        {
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

            CurrencyConverter.Init(testExchangeData);

            var reader = new FileReader();

            var accounts = AccountParser.ParseFile(reader.ReadFile("../accounts1.2m.txt"));
            var transactions = TransactionParser.ParseFile(reader.ReadFile("../transactions10m.txt"));

            var processor = new Processor(accounts, transactions);

            processor.Process();

            using (var file = new StreamWriter("output.txt"))
            {
                foreach (var account in accounts)
                {
                    file.WriteLine(account.Value);
                }
            }

            Console.WriteLine("Done");
        }
    }

    public enum Currency
    {
        USD,
        MXN,
        EUD,
        THB,
        GBP
    }

    public struct Account
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

    public interface Transaction
    {
        string AccountNumber { get; set; }
        double Amount { get; set; }
        Currency Currency { get; set; }
    }

    public struct Bill : Transaction
    {
        public string AccountNumber { get; set; }
        public double Amount { get; set; }
        public Currency Currency { get; set; }

        public string Bucket { get; set; }
    }

    public struct Payment : Transaction
    {
        public string AccountNumber { get; set; }
        public double Amount { get; set; }
        public Currency Currency { get; set; }
        public string Source { get; set; }
    }


    public static class CurrencyConverter
    {
        public static readonly Dictionary<Currency[], double> dictExchangeRates = new Dictionary<Currency[], double>;

        public static void Init(List<ExchangeRate> exchangeRates)
        {
            foreach(ExchangeRate er in exchangeRates)
            {
                Currency[] currencyPair = new Currency[2] { er.From, er.To };
                Currency[] currencyPairRev = new Currency[2] { er.To, er.From };
                if (!dictExchangeRates.ContainsKey(currencyPair))
                {
                    dictExchangeRates.Add(currencyPair, er.Rate);
                }

                if (!dictExchangeRates.ContainsKey(currencyPairRev))
                {
                    dictExchangeRates.Add(currencyPairRev, 1 / er.Rate);
                }
            }
        }

        public static double Convert(Currency from, Currency to, double amount)
        {
            return dictExchangeRates[new Currency[2] { from, to }] * amount;
        }
    }

    public struct ExchangeRate
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
        Dictionary<String, Account> accounts;
        List<Transaction> transactions;

        public Processor(Dictionary<String, Account> accounts, List<Transaction> transactions)
        {
            this.accounts = accounts;
            this.transactions = transactions;
        }

        public void Process()
        {
            foreach (var transaction in transactions)
            {
                Account account;
                if (accounts.TryGetValue(transaction.AccountNumber, out account))
                {
                    ApplyTransactionToAccount(account, transaction);
                }
            }
        }

        private void ApplyTransactionToAccount(Account acct, Transaction trans)
        {
            var amount = trans.Amount;
            if (trans.Currency != acct.BalanceCurrency)
            {
                amount = CurrencyConverter.Convert(trans.Currency, acct.BalanceCurrency, trans.Amount);
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

    public static class TransactionParser
    {
        public static List<Transaction> ParseFile(string content)
        {
            var transactions = new List<Transaction>();
            foreach (var line in content.Split("\n"))
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

        public static bool TryParse(string[] columns, ref Transaction transaction)
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

    public static class AccountParser
    {
        public static Dictionary<String, Account> ParseFile(string content)
        {
            var accounts = new Dictionary<String, Account>();
            foreach (var line in content.Split("\n"))
            {
                var columns = line.Split("|");
                Account account = new Account();
                if (TryParse(columns, ref account))
                {
                    accounts.Add(account.AccountNumber, account);
                }
                else
                {
                    Console.WriteLine($"Failed to import account: {line}");
                }
            }

            return accounts;
        }

        public static bool TryParse(string[] columns, ref Account account)
        {
            if (columns.Length != 3)
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
