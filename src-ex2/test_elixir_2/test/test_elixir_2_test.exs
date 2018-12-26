defmodule TestElixir2Test do
  use ExUnit.Case
  doctest TestElixir2

  test "greets the world" do
    assert TestElixir2.hello() == :world
  end
end
