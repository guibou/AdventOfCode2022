{
  description = "AoC 2022";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          hPkgs = pkgs.haskellPackages;
      in
      with pkgs.haskell.lib;
      rec {
        defaultPackage = (hPkgs.developPackage
          {
            root = ./.;
            overrides = self: super:
              {
                besout = unmarkBroken (doJailbreak super.besout);
              };
          });
        devShell = defaultPackage.env.overrideAttrs (old: {
          buildInputs = [ pkgs.cabal-install hPkgs.haskell-language-server ];
        });
      });
}
