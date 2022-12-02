{
  description = "AoC 2022";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          hPkgs = pkgs.haskell.packages.ghc92;
      in
      with pkgs.haskell.lib;
      rec {
        defaultPackage = (hPkgs.developPackage
          {
            root = ./.;
            overrides = self: super:
              {
                PyF = super.PyF_0_11_1_0;
                besout = unmarkBroken (doJailbreak super.besout);
              };
          });
        devShell = defaultPackage.env.overrideAttrs (old: {
          buildInputs = [ pkgs.cabal-install hPkgs.haskell-language-server ];
        });
      });
}
