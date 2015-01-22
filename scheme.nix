{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc763
}:
let
  hsEnv = haskellPackages.ghcWithPackages (hsPkgs : (with hsPkgs ; [
    hlint
    ghcMod
    hdevtools
    parsec
    derive
    lens
    haskeline
  ]));
in
haskellPackages.cabal.mkDerivation (self: {
  pname = "scheme";
  version = "0.1.0.0";
  src = /home/croh/db/Dropbox/dev/hs/basicScheme;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    hsEnv ];
  extraCmds = ''
    $(grep export ${hsEnv.outPath}/bin/ghc)
  '';
  meta = {
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
