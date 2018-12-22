defmodule TestElixir do
  # import Number.Delimit

  def apply_transactions() do
    {time, result} = :timer.tc(fn -> do_apply_transactions() end)
    IO.puts("It took #{time / 1_000_000}s")
    # IO.inspect("There are: #{number_to_delimited(Enum.count(result))} accounts")
    result
  end

  defp do_apply_transactions() do
    File.rm("/tmp/output.txt")
    accounts = read_accounts()
    transactions = read_transactions()

    # IO.inspect("There are: #{number_to_delimited(Enum.count(transactions))} transactions")

    transactions
    |> Enum.map(fn trx ->
      case Map.fetch(accounts, trx.account_number) do
        {:ok, account} ->
          cond do
            trx = %Bill{} ->
              x = Currency.add(account.balance, trx.amount)
              new_account = %Account{account | balance: x}
              Map.put(account, account.number, new_account)

            trx = %Payment{} ->
              x = Currency.subtract(account.balance, trx.amount)
              new_account = %Account{account | balance: x}
              Map.put(account, account.number, new_account)
          end

        :error ->
          []
          # IO.inspect("Transaction does not contain valid account #{inspect(trx)}")
      end
    end)

    # case File.open("/tmp/output.txt", [:write, :utf8]) do
    case :file.open("/tmp/output.txt", [:write, :raw]) do
      {:ok, file} ->
        accounts
        |> Enum.each(&:file.write(file, [inspect(&1), "\n"]))

        File.close(file)

      x ->
        IO.puts("Eror opeing file #{inspect(x)}")
    end

    # accounts
  end

  defp read_transactions(transactions \\ "./transactions.txt") do
    load_file(transactions)
    |> Stream.map(&into_transactions/1)
    |> Enum.into([])
  end

  defp read_accounts(accounts \\ "./accounts.txt") do
    load_file(accounts)
    |> Stream.map(&into_accounts/1)
    |> Enum.into(
      %{},
      fn {:ok, x} -> {x.number, x} end
    )
  end

  defp load_file(filename) when is_binary(filename) do
    File.stream!(filename)
    |> Stream.map(&String.replace(&1, "\n", ""))
  end

  defp into_accounts(raw_line_string) when is_binary(raw_line_string) do
    raw_line_string
    |> parse_line_into_fields()
    |> validate_it_has_correct_number_of_fields(3)
    |> validate_account_number(0)
    |> validate_currency(1)
    |> populate_account()
  end

  defp into_transactions(raw_line_string) when is_binary(raw_line_string) do
    raw_line_string
    |> parse_line_into_fields()
    |> validate_it_has_correct_number_of_fields(4)
    |> validate_account_number(0)
    |> validate_currency(1)
    |> validate_transaction_type(2)
    |> populate_transaction()
  end

  defp parse_line_into_fields(line) when is_binary(line) do
    line |> String.split("|")
  end

  defp validate_it_has_correct_number_of_fields([], _) do
    []
  end

  defp validate_it_has_correct_number_of_fields(data, no_fields)
       when is_list(data) and is_number(no_fields) do
    cond do
      Enum.count(data) == no_fields ->
        data

      true ->
        log_bad_data("#{Enum.at(data, 0)} expected #{no_fields} fields", data)
    end
  end

  defp validate_account_number([], _) do
    []
  end

  defp validate_account_number(data, position) when is_list(data) and is_number(position) do
    test =
      Enum.at(data, position)
      |> String.trim()
      |> Integer.parse()

    case test do
      {_number, ""} -> data
      {number, _} -> log_bad_data(" bad account number '#{number}'", data)
      :error -> log_bad_data(" bad account number ", data)
    end
  end

  defp validate_currency([], _) do
    []
  end

  defp validate_currency(data, position) when is_list(data) and is_number(position) do
    fields = Enum.at(data, position) |> String.trim()

    case split_currency(fields) do
      {:ok, _} -> data
      {:error, error} -> log_bad_data(error, data)
    end
  end

  defp split_currency(currency_field) do
    fields = currency_field |> String.trim() |> String.split(" ")

    cond do
      length(fields) == 2 ->
        parsed_amount = Enum.at(fields, 0) |> Integer.parse()
        symbol = Enum.at(fields, 1)

        cond do
          String.length(symbol) == 3 ->
            case parsed_amount do
              {amount, _} ->
                case symbol do
                  "USD" -> {:ok, %USD{amount: amount}}
                  "MXN" -> {:ok, %MXN{amount: amount}}
                  "EUD" -> {:ok, %EUD{amount: amount}}
                  "GBP" -> {:ok, %GBP{amount: amount}}
                  "THB" -> {:ok, %THB{amount: amount}}
                  _ -> {:error, "#{symbol} not a recognized currency symbol abbreviation"}
                end

              _ ->
                {:error, "cannot determine the  currency  amount"}
            end

          true ->
            {:error, "#{symbol} not a currency symbol abbreviation"}
        end

      true ->
        {:error, "no amount and currency in this field #{currency_field}"}
    end
  end

  defp populate_transaction([]) do
    []
  end

  defp populate_transaction(data) when is_list(data) do
    # 12728|107 GBP|Bill|Dues
    # 85850|589 MXN|Payment|Online Payment
    account_number = Enum.at(data, 0)
    currency_field = Enum.at(data, 1)

    currency =
      case split_currency(currency_field) do
        {:ok, valid} -> valid
        {:error, error} -> log_bad_data(error, data)
      end

    source_concept = Enum.at(data, 3)
    trans_field = Enum.at(data, 2) |> String.upcase()

    case trans_field do
      "BILL" ->
        %Bill{account_number: account_number, amount: currency, concept: source_concept}

      "PAYMENT" ->
        %Payment{account_number: account_number, amount: currency, source: source_concept}
    end
  end

  defp validate_transaction_type(data, position) when is_list(data) and is_number(position) do
    field =
      data
      |> Enum.at(position)
      |> String.trim()
      |> String.upcase()

    case field do
      "BILL" ->
        data

      "PAYMENT" ->
        data

      _ ->
        log_bad_data("cannot determine the type of transaction", data)
    end
  end

  defp populate_account([]) do
    []
  end

  defp populate_account(data) when is_list(data) do
    account_number = Enum.at(data, 0)

    currency_field = Enum.at(data, 1)

    currency =
      case split_currency(currency_field) do
        {:ok, c} -> c
        {:error, error} -> log_bad_data(error, data)
      end

    name = Enum.at(data, 2)

    {:ok,
     %Account{
       number: account_number,
       name: name,
       balance: currency
     }}
  end

  defp log_bad_data(message, data) do
    IO.inspect("bad account. Error: #{message} for #{data}")
    []
  end
end
