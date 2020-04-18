with (
  import (builtins.fetchTarball {
    url    = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
    sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
  })
  {}
);

let
  srcFilter = builtins.filterSource (path: type: baseNameOf path != ".git" &&
  baseNameOf path != ".hdevtools.sock" && path != "result" && path != "result-2");

  # Dependencies
  PLParser = pkgs.haskell.packages.ghcjs.callCabal2nix "PLParser" (srcFilter ../../Parser) {
    inherit PLLabel PLPrinter;
  };
  PLPrinter = pkgs.haskell.packages.ghcjs.callCabal2nix "PLPrinter" (srcFilter ../../Printer) {
  };
  PLGrammar = pkgs.haskell.packages.ghcjs.callCabal2nix "PLGrammar" (srcFilter ../../Grammar) {
    inherit PLLabel Reversible DSL-Compose;
  };
  PLLabel = pkgs.haskell.packages.ghcjs.callCabal2nix "PLLabel" (srcFilter ../../Label) {
    inherit PLPrinter;
  };

  DSL-ComposeSrc = pkgs.fetchFromGitHub {
    owner  = "syallop";
    repo   = "DSL-Compose";
    rev    = "5a95ae855f3291aa3193a7424892035226b10d2f";
    sha256 = "0315xn9g9f6sv69wwa9vdw3lfyv2gzw3bv8qwlv4qb6f76wyhrhr";
  };
  DSL-Compose = pkgs.haskell.packages.ghcjs.callCabal2nix "DSL-Compose" DSL-ComposeSrc {
  };

  ReversibleSrc = pkgs.fetchFromGitHub {
    owner = "syallop";
    repo  = "reversible";
    rev   = "5185a10559f0567ba927ea67a0b90f446d87dbe3";
    sha256 = "1f3ppz7d2gi34hnwgisvimfixcci2ldiwkxxisd54r6bg9dhi2fz";
  };
  Reversible = pkgs.haskell.packages.ghcjs.callCabal2nix "Reversible" ReversibleSrc {
    inherit DSL-Compose;
  };

  PL = pkgs.haskell.packages.ghcjs.callCabal2nix "PL" (srcFilter ../../Core) {
    inherit PLGrammar PLParser PLPrinter Reversible;
  };

  PLEditor = pkgs.haskell.packages.ghcjs.callCabal2nix "PLEditor" (srcFilter ../../Editor) {
  };

  PLLispy = pkgs.haskell.packages.ghcjs.callCabal2nix "PLLispy" (srcFilter ../../Lispy) {
    inherit PL PLGrammar PLLabel PLParser PLPrinter Reversible;
  };


  PLRepl = pkgs.haskell.packages.ghcjs.callCabal2nix "PLRepl" (srcFilter ../Lib) {
    inherit PL PLEditor PLGrammar PLLabel PLLispy PLParser PLPrinter Reversible;
  };
in
{
  PLReplJs = pkgs.haskell.packages.ghcjs.callCabal2nix "PLReplJs" (srcFilter ./.) {
    inherit PL PLRepl;
  };
}
