defmodule Producer do
  use GenStage

  def start_link(initial \\ 0) do
    GenStage.start_link(Producer, initial, name: Producer)
  end

  def init(arg) do
    {:producer, :initial_state}
  end

  def handle_demand(demand, state) do
    {:noreply, ["test", " ", "genstage"], state}
  end
end