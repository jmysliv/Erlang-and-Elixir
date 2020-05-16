defmodule ProducerConsumer do
  use GenStage

  def start_link do
    GenStage.start_link(ProducerConsumer, :state_doesnt_matter, name: ProducerConsumer)
  end

  def init(arg) do
    {:producer_consumer, :initial_state, subscribe_to: [Producer]}
  end

  def handle_events(data, from, state) do
    {:noreply, data, state}
  end
end