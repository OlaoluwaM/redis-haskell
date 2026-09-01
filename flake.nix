{
  description = "Development environment for redis haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # NOTE: What does this do?
        pkgs = nixpkgs.legacyPackages.${system};
        # 1. MUST match the GHC of your stack.yaml resolver
        # (e.g. LTS 23 -> ghc984, LTS 24 -> ghc9103; check stackage.org)

        # 2. Wrap stack so it uses Nix's GHC instead of downloading its own
        hpkgs = pkgs.haskell.packages.ghc984;
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          # Uses --no-nix to have the flake handle things and turn off stack's native nix support lest they conflict. --system-ghc to use the system ghc instead of stack's own ghc. --no-install-ghc to avoid stack trying to install its own ghc.
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "--no-nix --system-ghc --no-install-ghc"
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          # packages = buildInputs in this case
          packages = [
            hpkgs.ghc
            hpkgs.haskell-language-server
            hpkgs.hoogle
            hpkgs.implicit-hie
            hpkgs.retrie
            pkgs.fourmolu
            pkgs.hlint
            stack-wrapped
            pkgs.zlib
          ];
        };
      }
    );
}
