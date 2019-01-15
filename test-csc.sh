clear

echo ""
echo "##### C# Conc##############"
echo ""

cd src-csc
rm output.txt
dotnet build &&\
time dotnet run
cd ..