defmodule Runner do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl true
  def init(state) do
    {:ok, state}
  end

  def run(pid) do
    GenServer.cast(pid, :run)
  end

  def show(pid) do
    GenServer.call(pid, :show)
  end

  @impl true
  def handle_call(:show, _who, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast(:run, _state) do
    new_state = TestElixir.apply_transactions()
    {:noreply, new_state}
  end

  @impl true
  def handle_info(x, state) do
    IO.inspect("handle_info got this: #{x}")
    {:noreply, state}
  end
end
