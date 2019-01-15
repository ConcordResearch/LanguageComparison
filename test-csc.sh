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
