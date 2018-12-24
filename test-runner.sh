echo ""
echo "##### C# ##############"
echo ""

cd src-cs
rm output.txt
dotnet build
time dotnet run
cd ..

echo ""
echo "##### RUST ############"
echo ""

cd src-rs
rm output.txt
cargo build --release
time cargo run --release
cd ..

echo ""
echo "##### HASKELL #########"
echo ""

cd src-hs
rm output.txt
stack build
time stack run Main
cd ..

echo ""
echo "##### PURESCRIPT ######"
echo ""

cd src-ps
rm output.txt
psc-package build
pulp build
time node -e "require('output/Main').main()"
cd ..


echo ""
echo "##### ELIXIR ##########"
echo ""

rm elixir-output.txt
cd src-ex
rm output.txt
rm -rf test_elixir
mix deps.get
mix clean
MIX_ENV=prod mix release
mv _build/prod/rel/test_elixir .
cd ..
./src-ex/test_elixir/bin/test_elixir foreground
