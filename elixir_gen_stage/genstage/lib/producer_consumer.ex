defmodule ProducerConsumer do
  use GenStage

  def start_link do
    GenStage.start_link(ProducerConsumer, :some_state, name: ProducerConsumer)
  end

  def init(initial) do
    {:producer_consumer, initial, subscribe_to: [{Producer, max_demand: 3}]}
  end

  def handle_events(data, from, state) do
    {:noreply,analyse(data), state}
  end

  def analyse([]) do [] end
  def analyse([head | tail]) do
    [Map.put(head, "result", sentiment_analysis(head["en"]))] ++ analyse(tail)
  end

  def sentiment_analysis(text) do
    body = Poison.encode!(%{"text" => text})
    {:ok, response} = Tesla.post("https://sentim-api.herokuapp.com/api/v1/", body, headers: [{"content-type", "application/json"}])
    Poison.decode!(response.body) |>  Map.get("result")
  end

end