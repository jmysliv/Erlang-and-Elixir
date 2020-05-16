defmodule App do

  def hello do
    :world
  end

  def run do
    Producer.start_link(:initial)
    ProducerConsumer.start_link()
    Consumer.start_link()
  end

end
