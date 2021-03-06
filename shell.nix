{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "DocsDuck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ conduit HTTP httpClient httpConduit tagsoup cassava vector ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    description = "A simple Haskell program for importing moodle grades into DocsDB";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
