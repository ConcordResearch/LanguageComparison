defmodule TestElixirTest do
  use ExUnit.Case
  doctest TestElixir

  test "greets the world" do
    assert TestElixir.hello() == :world
  end
end
