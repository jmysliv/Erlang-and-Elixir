defmodule Consumer do
  use GenStage

  def start_link do
    GenStage.start_link(Consumer, :state_doesnt_matter)
  end

  def init(arg) do
    {:consumer, :initial_state, subscribe_to: [ProducerConsumer]}
  end

  def handle_events(data, from, state) do
    IO.puts("#{data}")
    {:noreply, [], state}
  end
end