clear

echo "##### C# ##############"
echo ""

cd src-cs
rm output.txt
dotnet build &&\
time dotnet run
cd ..

echo ""
echo "##### RUST ############"
echo ""

cd src-rs
rm output.txt
cargo build --release &&\
time cargo run --release
cd ..

echo ""
echo "##### HASKELL #########"
echo ""

cd src-hs
rm output.txt
stack build --fast &&\
time stack run Main
cd ..

# echo ""
# echo "##### PURESCRIPT ######"
# echo ""

# cd src-ps
# rm output.txt
# psc-package build &&\
# pulp build &&\
# time node -e "require('output/Main').main()"
# cd ..


echo ""
echo "##### ELIXIR ##########"
echo ""

cd src-ex
rm _build/prod/rel/elixir-output.txt
rm -rf test_elixir
mix deps.get &&\
mix clean &&\
MIX_ENV=prod mix release &&\
time _build/prod/rel/test_elixir/bin/test_elixir.bat foreground
cd ..

