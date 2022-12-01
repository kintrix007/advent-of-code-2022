defmodule Calories do
  def part1() do
    {:ok, content} = File.read("input")
    dwarfs = String.split(content, "\n\n", trim: true)
    |> Enum.map(fn x ->
      String.split(x, "\n", trim: true)
      |> Enum.map(fn str -> Integer.parse str end)
      |> Enum.map(fn {x, _} -> x end)
    end)
    |> Enum.map(fn x -> Enum.sum(x) end)

    Enum.max dwarfs
  end
end
