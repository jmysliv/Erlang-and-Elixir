defmodule Consumer do
  use GenStage

  def start_link do
    GenStage.start_link(Consumer, :state_doesnt_matter)
  end

  def init(arg) do
    {:consumer, :initial_state, subscribe_to: [{ProducerConsumer, max_demand: 3}]}
  end

  def handle_events(data, from, state) do
    print(data)
    {:noreply, [], state}
  end

  def print([]) do :ok end
  def print([head | tail]) do
    IO.inspect(head)
    print(tail)
  end
end