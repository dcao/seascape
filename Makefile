nix-build-exe:
	hpack
	nix build -f . seascape.components.exes.seascape-app
