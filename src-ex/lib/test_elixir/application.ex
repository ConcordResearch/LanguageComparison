defmodule TestElixir.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    children = [
      {Runner, []}
    ]

    opts = [strategy: :one_for_one, name: TestElixir.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
