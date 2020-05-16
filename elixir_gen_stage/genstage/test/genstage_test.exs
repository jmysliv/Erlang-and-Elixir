defmodule GenstageTest do
  use ExUnit.Case
  doctest Genstage

  test "greets the world" do
    assert Genstage.hello() == :world
  end
end
