defmodule Consumer do
  use GenStage

  def start_link(threshold) do
    GenStage.start_link(Consumer, threshold)
  end

  def init(threshold) do
    {:ok, file} = File.open("result.txt", [:write, :utf8])
    {:consumer, %{:threshold => threshold, :file => file}, subscribe_to: [{ProducerConsumer, max_demand: 3}]}
  end

  def handle_events(data, _, state) do
    Enum.filter(data, &(&1["result"]["polarity"] > state.threshold)) |> Enum.map(&(Poison.encode!(&1) |> String.replace("\"", ""))) |> Enum.each(&(save_to_file(state.file, &1)))
    {:noreply, [], state}
  end

  def save_to_file(file, result) do
    IO.write(file, inspect(result))
    IO.write(file, "\n")
  end
end