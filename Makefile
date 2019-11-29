publish:
	dotnet publish -c Release -o output/linux --self-contained -r linux-x64
	dotnet publish -c Release -o output/osx --self-contained -r osx-x64
