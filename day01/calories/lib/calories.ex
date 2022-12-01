defmodule Calories do
  #? First time writing Elixir
  #? I would assume this is messier than it needs to be

  def part1() do
    {:ok, content} = File.read("input")

    dwarfs = String.split(content, "\n\n", trim: true)
    |> Enum.map(fn x ->
      String.split(x, "\n", trim: true)
      |> Enum.map(&Integer.parse/1)
      |> Enum.map(fn {x, _} -> x end)
    end)
    |> Enum.map(&Enum.sum/1)

    Enum.max dwarfs
  end

  def part2() do
    {:ok, content} = File.read("input")

    dwarfs = String.split(content, "\n\n", trim: true)
    |> Enum.map(fn x ->
      String.split(x, "\n", trim: true)
      |> Enum.map(&Integer.parse/1)
      |> Enum.map(fn {x, _} -> x end)
    end)
    |> Enum.map(&Enum.sum/1)

    dwarfs
    |> Enum.sort_by(&-/1) # Sort by negated elements. e.g. descending order
    |> Enum.take(3)
    |> Enum.sum
  end
end
