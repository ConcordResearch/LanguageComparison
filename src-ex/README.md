# TestElixir
This version of the test uses a single process for all the work.

For a concurrent test see [TestElixir2](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-ex2)
### Notes
```shell
> clear; mix deps.get; mix clean; mix format ; iex -S mix
```

```elixir
iex(1)> {:ok, pid} = GenServer.start_link(Runner, [])
{:ok, #PID<0.304.0>}
iex(2)> GenServer.cast(pid, :run)
:ok
"There are: 100,000.00 accounts"
"There are: 100,000.00 transactions"
iex(3)> GenServer.call(pid, :show) |> Enum.count()
100000
iex(4)>
```