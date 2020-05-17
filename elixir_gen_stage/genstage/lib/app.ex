defmodule App do

  def run do
    Producer.start_link(100)
    ProducerConsumer.start_link()
    Consumer.start_link(0.1)
  end

end
