{
  description = "Data structures, pure, functional";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , flake-utils
    }:
    let utils = flake-utils.lib;
    in
    utils.eachDefaultSystem (system:
    let
      supportedGHCVersion = "8104";
      compilerVersion = "ghc${supportedGHCVersion}";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          lamport-spectacle = hfinal.callCabal2nix "lamport-spectacle" ./. { };
        };
      };
    in
    rec {
      packages = utils.flattenTree
        { lamport-spectacle = hsPkgs.lamport-spectacle; };

      # nix flake check
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
          };
        };
      };

      # nix develop
      devShell = hsPkgs.shellFor {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        withHoogle = true;
        packages = p: [
          p.lamport-spectacle
        ];
        buildInputs = with pkgs; [
          hsPkgs.haskell-language-server
          haskellPackages.cabal-install
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
        ] ++ (builtins.attrValues (import ./scripts.nix { s = pkgs.writeShellScriptBin; }));
      };

      # nix build
      defaultPackage = packages.lamport-spectacle;
    });
}
