clear

echo ""
echo "##### C# Conc##############"
echo ""

cd src-csc
rm output.txt
dotnet build &&\
time dotnet run
cd ..

echo ""
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


echo ""
echo "##### PURESCRIPT ######"
echo ""
cd src-ps
rm output.txt
psc-package build &&\
pulp build -- --json-errors 2> errors.txt &&\
time node --max-old-space-size=8192 -e "require('./output/Main').main()"
cd ..

echo ""
echo "##### ELIXIR ##########"
echo ""

### The following lines are in this order to make sure we can run repeatedly
cd src-ex                                                               ####
rm -rf _build                                                           ####
mix deps.get                                                            ####
mix clean                                                               ####
MIX_ENV=prod mix release                                                ####
time ./_build/prod/rel/test_elixir/bin/test_elixir foreground           ####
cd ..                                                                   ####
###  On Windows Uncomment line below instead                            ####
#./src-ex/test_elixir/bin/test_elixir.bat foreground                    ####
############################################################################
