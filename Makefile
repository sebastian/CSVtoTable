publish:
	dotnet publish -c Release -o output/linux-x64 --self-contained -r linux-x64
	dotnet publish -c Release -o output/osx-x64 --self-contained -r osx-x64
	dotnet publish -c Release -o output/win-x64 --self-contained -r win-x64
