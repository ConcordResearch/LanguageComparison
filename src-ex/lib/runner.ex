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

  def show_state(pid) do
    GenServer.call(pid, :show_state)
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
  def handle_info(:work, state) do
    # Do the desired work here
    # Reschedule once more
    schedule_work()
    {:noreply, state}
  end

  defp schedule_work() do
    Process.send_after(self(), :work, 2 * 60 * 60 * 1000)
  end
end
