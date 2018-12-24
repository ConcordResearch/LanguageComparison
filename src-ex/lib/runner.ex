defmodule Runner do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl true
  def init([]) do
    schedule_run()
    {:ok, :run}
  end

  def run(pid) do
    GenServer.cast(pid, :run)
  end

  def show(pid) do
    GenServer.call(pid, :show)
  end

  @impl true
  def handle_cast(:run, _state) do
    TestElixir.apply_transactions()
    {:noreply, []}
  end

  @impl true
  def handle_call(:show, _who, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_info(:run, state) do
    cond do
      :run = state ->
        IO.puts("\n\n((GenServer  go go go run())")
        IO.puts("Where are here '#{Path.expand(".")}'")
        GenServer.cast(self(), :run)
        IO.puts("\n\n")

      true ->
        []
    end

    {:noreply, []}
  end

  defp schedule_run() do
    Process.send_after(self(), :run, 1000)
  end
end
