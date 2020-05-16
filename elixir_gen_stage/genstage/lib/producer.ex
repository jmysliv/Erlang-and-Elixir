defmodule Producer do
  use GenStage

  def start_link(initial \\ 0) do
    GenStage.start_link(Producer, initial, name: Producer)
  end

  def init(arg) do
    {:producer, :initial_state}
  end

  def handle_demand(demand, state) do
    :timer.sleep(3)
    quotes = ProgrammingQuotes.get_quotes(demand)
    {:noreply, quotes, state}
  end
end

defmodule ProgrammingQuotes do

  def get_quote() do
    {:ok, response} = Tesla.get("https://programming-quotes-api.herokuapp.com/quotes/random")
    Poison.decode!(response.body) |>  Map.delete("_id") |> Map.delete("id")
  end

  def get_quotes(0) do [] end
  def get_quotes(number) do
    [get_quote()] ++ get_quotes(number-1)
  end
end