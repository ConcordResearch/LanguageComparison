clear

echo ""
echo "##### C# ##############"
echo ""

date
time docker run -it --rm lang-comp-cs /bin/bash -c "dotnet run"

echo ""
echo "##### RUST ############"
echo ""

date
time docker run -it --rm lang-comp-rs /bin/bash -c "cargo run --release"

echo ""
echo "##### ELIXIR ############"
echo ""

date
time docker run -it --rm lang-comp-ex /bin/bash -c "./_build/prod/rel/test_elixir/bin/test_elixir foreground"

echo ""
echo "##### PURESCRIPT ############"
echo ""

date
time docker run -it --rm lang-comp-ps /bin/bash -c "node --max-old-space-size=8192 -e \"require('./output/Main').main()\""

echo ""
echo "##### HASKELL ############"
echo ""

date
time docker run -it --rm lang-comp-hs /bin/bash -c "stack run Main"

 