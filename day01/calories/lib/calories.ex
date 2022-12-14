defmodule Calories do
  #? First time writing Elixir
  #? I would assume this is messier than it needs to be

  def parse(file) do
    {:ok, content} = File.read(file)

    dwarfs = String.split(content, "\n\n", trim: true)
    |> Stream.map(fn x ->
      String.split(x, "\n", trim: true)
      |> Stream.map(&Integer.parse/1)
      |> Stream.map(fn {x, _} -> x end)
    end)

    dwarfs
  end

  def part1() do
    parse("input")
    |> Stream.map(&Enum.sum/1)
    |> Enum.max
  end

  def part2() do
    parse("input")
    |> Stream.map(&Enum.sum/1)
    |> Enum.sort_by(&-/1) # Sort by negated elements. e.g. descending order
    |> Stream.take(3)
    |> Enum.sum
  end
end
