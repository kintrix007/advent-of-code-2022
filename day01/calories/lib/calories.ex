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
    |> Enum.sort_by(fn x -> -x end)

    [d1 | [ d2 | [ d3 | _ ]]] = dwarfs
    Enum.sum [ d1, d2, d3 ]
  end
end
