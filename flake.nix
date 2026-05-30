{
  inputs = {
    nixpkgs.url = "https://github.com/nixos/nixpkgs/archive/3afd19146cac33ed242fc0fc87481c67c758a59e.tar.gz";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-stable,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        haskellPackages = pkgs.haskellPackages;

        buildInputs = with pkgs; [
          # Haskell toolchain
          ghc
          cabal-install
          haskell-language-server
          haskellPackages.cabal-fmt
          haskellPackages.hlint
          ormolu

          # PostgreSQL client library (required by postgresql-simple in tests)
          postgresql

          # System libraries
          zlib
          pkg-config
        ];

        packageName = "postgres-interface";
      in

      with pkgs;
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self rec {
          # Dependency overrides go here
        };
        packages.default = self.packages.${system}.${packageName};
        devShells.default = mkShell {
          inherit buildInputs;
          shellHook = ''
            export LORCAN_FLAKE_NAME="postgres-interface"
          '';
        };
      }
    );
}
