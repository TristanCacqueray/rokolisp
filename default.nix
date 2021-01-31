{ nixpkgs ? import <nixpkgs> { } }:
let
  name = "rokolisp";
  drv = nixpkgs.haskellPackages.callCabal2nix name ./. { };
  # cabal2nix doesn't add benchmark build depends
  drvWithBench = nixpkgs.haskell.lib.addBuildDepends drv
    (with nixpkgs.haskellPackages; [ gauge ]);
  shellDrv = nixpkgs.haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [ drvWithBench ];
    buildInputs = with nixpkgs.haskellPackages; [
      haskell-language-server
      hlint
      cabal-install
    ];

  };
in if nixpkgs.lib.inNixShell then shellDrv else drv
