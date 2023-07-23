{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/23.05";
  outputs = { self, nixpkgs }:
    let
      ghcVersion = "ghc927";
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hpkgs = pkgs.haskell.packages.${ghcVersion};
      lifx-lan = hpkgs.callCabal2nix "lifx-lan" ./. { };
      tools = with hpkgs; [ cabal-install ];
    in
    {
      packages.${system}.default = lifx-lan;
      devShells.${system}.default = hpkgs.shellFor {
        packages = p: [ lifx-lan ];
        nativeBuildInputs = tools;
      };
    };
}
