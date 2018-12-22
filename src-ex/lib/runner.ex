defmodule Runner do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl true
  def init(state) do
    schedule_notifications()
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
  def handle_info(:update, state) do
    IO.write([".."])

    if(Enum.count(state) == 0) do
      schedule_notifications()
    end

    {:noreply, state}
  end

  defp schedule_notifications(interval \\ 10 * 1000) do
    Process.send_after(self(), :update, interval)
  end
end
