use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::str::FromStr;

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Copy, Clone, Debug)]
enum Currency {
    USD,
    MXN,
    EUD,
    THB,
    GBP,
}
impl FromStr for Currency {
    type Err = ();

    fn from_str(s: &str) -> Result<Currency, ()> {
        match s {
            "USD" => Ok(Currency::USD),
            "MXN" => Ok(Currency::MXN),
            "EUD" => Ok(Currency::EUD),
            "THB" => Ok(Currency::THB),
            "GBP" => Ok(Currency::GBP),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
struct Money {
    amount: f32,
    currency: Currency,
}

#[derive(Debug, Clone)]
struct Account {
    account_number: String,
    name: String,
    balance: Money,
}

#[derive(Debug)]
enum Transaction {
    Bill {
        account_number: String,
        amount: Money,
        bucket: String,
    },
    Payment {
        account_number: String,
        amount: Money,
        source: String,
    },
}

fn main() -> Result<(), std::io::Error> {
    // the ? is quasi doing what <- does in a do block
    let accounts_text = fs::read_to_string("../accounts1.2m.txt")?;
    let transactions_text = fs::read_to_string("../transactions10m.txt")?;

    let conversion_rates = build_conversion_rates(&[
        (Currency::USD, Currency::MXN, 1.5),
        (Currency::USD, Currency::EUD, 2.5),
        (Currency::USD, Currency::THB, 3.5),
        (Currency::USD, Currency::GBP, 4.5),
        (Currency::MXN, Currency::EUD, 2.5),
        (Currency::MXN, Currency::THB, 3.5),
        (Currency::MXN, Currency::GBP, 4.5),
        (Currency::EUD, Currency::THB, 3.5),
        (Currency::EUD, Currency::GBP, 4.5),
        (Currency::THB, Currency::GBP, 4.5),
    ]);

    let mut account_lookup = parse_accounts(&accounts_text);

    let transactions_lookup = parse_transactions(&transactions_text);
    process_transactions(&mut account_lookup, &conversion_rates, &transactions_lookup);

    let file = File::create("output.txt")?;
    let mut buffer = BufWriter::new(file);

    account_lookup.iter().for_each(|(_, v)| {
        buffer
            .write_all(format!("{:?}", v).as_bytes())
            .expect("Failed to write to output file");
    });

    println!("Done");

    Ok(())
}

// borrow the array as a slice and return the HashMap for looking up rates
fn build_conversion_rates(
    base_rates: &[(Currency, Currency, f32)],
) -> HashMap<Currency, HashMap<Currency, f32>> {
    let mut map = HashMap::new();

    base_rates
        .iter()
        .for_each(|(first_currency, second_currency, rate)| {
            let mappings_for_first_currency =
                map.entry(first_currency.clone()).or_insert(HashMap::new());
            mappings_for_first_currency.insert(second_currency.clone(), rate.clone());

            mappings_for_first_currency.insert(first_currency.clone(), 1.0);

            let mappings_for_second_currency =
                map.entry(second_currency.clone()).or_insert(HashMap::new());
            mappings_for_second_currency.insert(first_currency.clone(), 1.0 / rate.clone());

            mappings_for_second_currency.insert(second_currency.clone(), 1.0);
        });
    map
}

// higher order functions, especially ones involving generics are a bit hard to read
// also the need to declare functions as FnOnce/FnMut/etc. is additional friction for
// using HOF
fn process_lines<T, F>(text: &String, str_to_t: F) -> (Vec<Option<T>>, Vec<Option<T>>)
where
    F: FnMut(&str) -> Option<T>,
{
    text.split("\n").map(str_to_t).partition(|e| e.is_some())
}

fn parse_transactions(text: &String) -> Vec<Transaction> {
    let (valid, _invalid) = process_lines(text, |line| {
        let parts: Vec<&str> = line.split("|").collect();
        if parts.len() == 4 {
            build_transaction(parts[0], parts[1], parts[2], parts[3])
        } else {
            None
        }
    });
    valid.into_iter().map(|e| e.unwrap()).collect()
}

fn build_transaction(
    account_number: &str,
    balance: &str,
    transaction_type: &str,
    notes: &str,
) -> Option<Transaction> {
    let parsed_account_number = u32::from_str(account_number).ok()?;
    let parsed_balance = str_to_money(balance)?;

    match transaction_type {
        "Bill" => Some(Transaction::Bill {
            account_number: parsed_account_number.to_string(),
            amount: parsed_balance,
            bucket: notes.to_string(),
        }),
        "Payment" => Some(Transaction::Payment {
            account_number: parsed_account_number.to_string(),
            amount: parsed_balance,
            source: notes.to_string(),
        }),
        _ => None,
    }
}

fn parse_accounts(text: &String) -> HashMap<String, Account> {
    let (valid, _error) = process_lines(text, |line| {
        let parts: Vec<&str> = line.split("|").collect();
        if parts.len() == 3 {
            build_account(parts[0], parts[1], parts[2])
        } else {
            None
        }
    });

    let mut result = HashMap::new();

    // this is sligly tricky, we have to use into_iter here so that all the downstream
    // iterator functions own the objects in the iterator. Without this we can't insert the
    // T into the map, only a &T inside the for_each
    valid
        .into_iter()
        .map(|e| match e {
            Some(val) => val,
            None => unreachable!(), // the partition above guarantees all of these values will be Some
        })
        .for_each(|account| {
            result.insert(account.account_number.clone(), account);
        });

    result
}

fn build_account(account_number: &str, balance: &str, name: &str) -> Option<Account> {
    let parsed_account_number = u32::from_str(account_number).ok()?;
    let parsed_balance = str_to_money(balance)?;

    Some(Account {
        account_number: parsed_account_number.to_string(),
        name: name.to_string(),
        balance: parsed_balance,
    })
}

fn process_transactions(
    accounts: &mut HashMap<String, Account>,
    conversion_rates: &HashMap<Currency, HashMap<Currency, f32>>,
    transactions: &Vec<Transaction>,
) {
    // for each transaction
    // get account from map
    // apply transaction change to account
    // update account map
    transactions.iter().for_each(|transaction| {
        let account_number = match transaction {
            Transaction::Bill { account_number, .. } => account_number,
            Transaction::Payment { account_number, .. } => account_number,
        };

        match accounts.get(account_number) {
            Some(account) => match apply_transaction(account, transaction, conversion_rates) {
                Some(updated_account) => {
                    accounts.insert(account_number.to_string(), updated_account);
                    ()
                }
                None => println!(
                    "Failed to apply transaction {:?} to {}",
                    transaction, account_number
                ),
            },
            None => (),
        }
    });
}

fn apply_transaction(
    account: &Account,
    transaction: &Transaction,
    currency_conversion: &HashMap<Currency, HashMap<Currency, f32>>,
) -> Option<Account> {
    let (transaction_currency, transaction_amount) = match transaction {
        Transaction::Bill { amount, .. } => (amount.currency, amount.amount),
        Transaction::Payment { amount, .. } => (amount.currency, -amount.amount),
    };

    let conversion_rates = currency_conversion.get(&transaction_currency)?;
    let target_rate = conversion_rates.get(&account.balance.currency)?;

    let new_balance = apply_money(&account.balance, transaction_amount * target_rate);

    Some(Account {
        balance: new_balance,
        ..account.clone()
    })
}

// 100 USD
fn str_to_money(money_as_text: &str) -> Option<Money> {
    let parts: Vec<&str> = money_as_text.split(" ").collect();
    if parts.len() == 2 {
        let amount = f32::from_str(parts[0]).ok()?;
        let currency = Currency::from_str(parts[1]).ok()?;

        Some(Money { amount, currency })
    } else {
        None
    }
}

fn apply_money(Money { amount, currency }: &Money, amount_to_apply: f32) -> Money {
    Money {
        amount: amount + amount_to_apply,
        currency: *currency,
    }
}
