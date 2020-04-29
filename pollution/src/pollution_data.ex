defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV() do
    list = File.read!("pollution.csv") |> String.split("\n") |> Enum.map(& (parseLine(&1)))
    IO.puts("#{length(list)}")
    list
  end

  def parseLine(line) do
    [date, time, latitude, longitude, value] = String.split(line, ",")
    date = String.split(date, "-") |> Enum.reverse |> Enum.map(&(Integer.parse(&1) |> elem(0)))  |> :erlang.list_to_tuple()
    {h, m} = String.split(time, ":") |> Enum.map(&(Integer.parse(&1) |> elem(0))) |> :erlang.list_to_tuple()
    time = {h, m, 0}
    dateTime = {date, time}
    location = {Float.parse(latitude) |> elem(0), Float.parse(longitude) |> elem(0)}
    value = Integer.parse(value) |> elem(0)
    %{:datetime => dateTime, :location  => location, :pollutionLevel => value}
  end

  def identifyStations(list) do
    stations = Enum.uniq_by(list, & (&1.location))
    IO.puts("#{length(stations)}")
    stations
  end

  def addStations([station | rest]) do
    name = "station_#{station.location |> elem(0)}_#{station.location |> elem(1)}"
    :pollution_gen_server.addStation(name, station.location)
    addStations(rest)
  end
  def addStations([]) do :ok end

  def addMeasurements([measure | rest]) do
    name = "station_#{measure.location |> elem(0)}_#{measure.location |> elem(1)}"
    :pollution_gen_server.addValue(name, measure.datetime, "PM10", measure.pollutionLevel)
    addMeasurements(rest)
  end
  def addMeasurements([]) do :ok end

  def analizeData() do
    {time, value} = (&:pollution_gen_server.getStationMean/2) |> :timer.tc(["station_20.06_49.986", "PM10"])
    IO.puts("Czas: #{time}; warosc: #{value}")
    {time, value} = (&:pollution_gen_server.getDailyMean/2) |> :timer.tc([{{2017, 5, 3}, {0, 0, 0}}, "PM10"])
    IO.puts("Czas: #{time}; warosc: #{value}")
  end

  def parseAndLoadData do
    :pollution_supervisor.start_link()
    loadStation = fn -> importLinesFromCSV() |> identifyStations() |> addStations() end
    loadMeasurements = fn -> importLinesFromCSV() |> addMeasurements() end

    IO.puts("#{ loadStation |> :timer.tc([]) |> elem(0)}")
    IO.puts("#{ loadMeasurements |> :timer.tc([]) |> elem(0)}")
    analizeData()
  end

end

defmodule PollutionDataStream do
  @moduledoc false

  def importLinesFromCSV() do
    list = File.stream!("pollution.csv")  |> Stream.map(& (parseLine(&1))) |> Enum.to_list
    IO.puts("#{length(list)}")
    list
  end

  def parseLine(line) do
    [date, time, latitude, longitude, value] = String.split(line, ",")
    date = String.split(date, "-") |> Enum.reverse |> Stream.map(&(Integer.parse(&1) |> elem(0))) |> Enum.reduce({}, fn(element, tuple) -> Tuple.append(tuple, element) end)
    {h, m} = String.split(time, ":") |> Stream.map(&(Integer.parse(&1) |> elem(0))) |> Enum.reduce({}, fn(element, tuple) -> Tuple.append(tuple, element) end)
    time = {h, m, 0}
    dateTime = {date, time}
    location = {Float.parse(latitude) |> elem(0), Float.parse(longitude) |> elem(0)}
    value = Integer.parse(value) |> elem(0)
    %{:datetime => dateTime, :location  => location, :pollutionLevel => value}
  end

  def identifyStations(list) do
    stations = Enum.uniq_by(list, & (&1.location))
    IO.puts("#{length(stations)}")
    stations
  end

  def addStations([station | rest]) do
    name = "station_#{station.location |> elem(0)}_#{station.location |> elem(1)}"
    :pollution_gen_server.addStation(name, station.location)
    addStations(rest)
  end
  def addStations([]) do :ok end

  def addMeasurements([measure | rest]) do
    name = "station_#{measure.location |> elem(0)}_#{measure.location |> elem(1)}"
    :pollution_gen_server.addValue(name, measure.datetime, "PM10", measure.pollutionLevel)
    addMeasurements(rest)
  end
  def addMeasurements([]) do :ok end

  def analizeData() do
    {time, value} = (&:pollution_gen_server.getStationMean/2) |> :timer.tc(["station_20.06_49.986", "PM10"])
    IO.puts("Czas: #{time}; warosc: #{value}")
    {time, value} = (&:pollution_gen_server.getDailyMean/2) |> :timer.tc([{{2017, 5, 3}, {0, 0, 0}}, "PM10"])
    IO.puts("Czas: #{time}; warosc: #{value}")
  end

  def parseAndLoadData do
    :pollution_supervisor.start_link()
    loadStation = fn -> importLinesFromCSV() |> identifyStations() |> addStations() end
    loadMeasurements = fn -> importLinesFromCSV() |> addMeasurements() end

    IO.puts("#{ loadStation |> :timer.tc([]) |> elem(0)}")
    IO.puts("#{ loadMeasurements |> :timer.tc([]) |> elem(0)}")
    analizeData()
  end

end