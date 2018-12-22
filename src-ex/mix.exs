defmodule TestElixir.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_elixir,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {TestElixir.Application, []}
    ]
  end

  defp deps do
    [
      {:number, "~> 1.0.0"},
      {:distillery, "~> 2.0"}
    ]
  end
end
