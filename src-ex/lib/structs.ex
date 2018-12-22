defmodule USD do
  defstruct type: :USD, amount: 0.0
end

defmodule MXN do
  defstruct type: :MXN, amount: 0.0
end

defmodule EUD do
  defstruct type: :EUD, amount: 0.0
end

defmodule GBP do
  defstruct type: :GBP, amount: 0.0
end

defmodule THB do
  defstruct type: :THB, amount: 0.0
end

defmodule Account do
  defstruct number: 0, balance: %USD{}, name: ""
end

# 12728|107 GBP|Bill|Dues
# 85850|589 MXN|Payment|Online Payment
defmodule Bill do
  defstruct account_number: 0, amount: %USD{}, concept: "Dues"
end

defmodule Payment do
  defstruct account_number: 0, amount: %USD{}, source: "Online"
end

defmodule ExchangeRates do
  defstruct rates: %{
              USD: %{USD: 1.0, MXN: 2.0, EUD: 3.0, GBP: 4.0, THB: 5.0},
              MXN: %{USD: 0.2, MXN: 1.0, EUD: 42.0, GPB: 45.0, THB: 23.0},
              EUD: %{USD: 1.0, MXN: 2.0, EUD: 1.0, GBP: 4.0, THB: 5.0},
              GBP: %{USD: 0.7, MXN: 0.6, EUD: 0.5, GBP: 1.0, THB: 0.3},
              THB: %{USD: 0.9, MXN: 0.8, EUD: 0.7, GBP: 0.6, THB: 1.0}
            }
end

defimpl String.Chars, for: [ExchangeRates, Account, USD, MXN, EUD, GBP, THB, Bill, Payment] do
  def to_string(data) do
    IO.inspect(data)
  end
end

defprotocol Currency do
  def add(
        currency_one \\ %USD{},
        into_currency_two \\ %USD{},
        currency_xrates \\ %ExchangeRates{}
      )

  def subtract(
        from_currency_one \\ %USD{},
        currency_two \\ %USD{},
        currency_xrates \\ %ExchangeRates{}
      )
end

defimpl Currency, for: [USD, MXN, EUD, GBP, THB] do
  def add(
        %_{type: type_one, amount: amount_one},
        %_{type: type_two, amount: amount_two},
        all_rates = %ExchangeRates{}
      ) do
    implementaion_logic(type_one, amount_one, type_two, amount_two, all_rates, :add)
  end

  @spec subtract(
          USD.t(),
          %{__struct__: atom(), amount: any(), type: any()},
          atom() | %{rates: map()}
        ) :: {:error, <<_::64, _::_*8>>} | {:ok, number()}
  def subtract(
        %USD{type: type_one, amount: amount_one},
        %_{type: type_two, amount: amount_two},
        all_rates
      ) do
    implementaion_logic(type_one, amount_one, type_two, amount_two, all_rates, :subtract)
  end

  defp implementaion_logic(type_one, amount_one, type_two, amount_two, all_rates, operation) do
    case Map.fetch(all_rates.rates, type_one) do
      {:ok, this_types_exchange_rates} ->
        case Map.fetch(this_types_exchange_rates, type_two) do
          {:ok, rate} ->
            case operation do
              :add ->
                {:ok, amount_one + amount_two * rate}

              :subtract ->
                {:ok, amount_one - amount_two * rate}
            end

          :error ->
            {:error, "key #{type_one} does not contain a rate for #{type_two}"}
        end

      :error ->
        {:error, "{type_one} is not defined in our exchange rates table #{all_rates}"}
    end
  end
end
