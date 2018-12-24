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

cd src-ex
rm output.txt
mix deps.get
mix clean
time mix run
cd ..