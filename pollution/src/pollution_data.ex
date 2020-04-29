defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV() do
    list = File.read!("pollution.csv") |> String.split("\n")
#    for test
    [head | _] = list
    parseLine(head)
  end

  def parseLine(line) do
    [date, time, latitude, longitude, value] = String.split(line, ",")
    date = String.split(date, "-") |> Enum.reverse |> Enum.map(&(Integer.parse(&1))) |> Enum.map(& (elem(&1, 0))) |> :erlang.list_to_tuple()
    {h, m} = String.split(time, ":") |> Enum.map(&(Integer.parse(&1))) |> Enum.map(& (elem(&1, 0))) |> :erlang.list_to_tuple()
    time = {h, m, 0}
    dateTime = {date, time}
    location = {Float.parse(latitude) |> elem(0), Float.parse(longitude) |> elem(0)}
    value = Integer.parse(value) |> elem(0)
    %{:datetime => dateTime, :location  => location, :pollutionLevel => value}
  end

end
