with import <nixpkgs> { };

let
  ghc = haskellPackages.ghcWithHoogle (p: with p; [
    aeson
    containers
    data-fix
    megaparsec
    optics
    optics-extra
    ordered-containers
    safe-exceptions
    text
    vty
    vty-unix
  ]);

in mkShell {
  buildInputs = [
    cabal-install
    ghc
  ];
}
