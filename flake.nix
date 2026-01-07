{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
          ];
        };
      in
      {
        packages.emoji-favicon = pkgs.haskellPackages.emoji-favicon;
        packages.default = self.packages.${system}.emoji-favicon;

        checks = {
          inherit (pkgs.haskellPackages) emoji-favicon;

          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              cabal-fmt.enable = true;
              deadnix.enable = true;
              hlint.enable = true;
              markdownlint.enable = true;
              nixpkgs-fmt.enable = true;
              statix.enable = true;
              stylish-haskell.enable = true;
            };
          };
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.emoji-favicon ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-fmt
            cabal-install
            hlint
          ];
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };

      }) // {
      overlays.default = _: prev: {
        haskell = prev.haskell // {
          # override for all compilers
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (_: hprev: {

            emoji-favicon =
              let
                haskellSourceFilter = prev.lib.sourceFilesBySuffices ./. [
                  ".cabal"
                  ".hs"
                  ".txt"
                  "LICENSE"
                ];
                emoji-favicon = hprev.callCabal2nix "emoji-favicon" haskellSourceFilter { };
              in
              prev.lib.trivial.pipe emoji-favicon
                (with prev.haskell.lib; [
                  dontHaddock
                  enableStaticLibraries
                  disableLibraryProfiling
                ]);
          });
        };
      };
    };
}
