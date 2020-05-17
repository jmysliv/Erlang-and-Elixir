defmodule Producer do
  use GenStage

  def start_link(initial) do
    GenStage.start_link(Producer, initial, name: Producer)
  end

  def init(max_number) do
    {:producer, max_number}
  end

  def handle_demand(demand, state)  when demand>0 and state>=demand do
    :timer.sleep(3)
    {:noreply, ProgrammingQuotes.get_quotes(demand), state - demand}
  end
  def handle_demand(_, state) do {:noreply, [], state} end

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