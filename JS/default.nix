with (
  import (builtins.fetchTarball {
    url    = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
    sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
  })
  {}
);

let
  # Filter to exclude things which don't effect the reproducability of the build - developer dot files, build artifacts, etc.
  srcFilter = builtins.filterSource (path: type: baseNameOf path != ".git" && baseNameOf path != ".hdevtools.sock" && path != "result" && path != "result-2");

  # Dependencies
  #
  # Most are expected to live at the same level as the root PLRepl project. I.E.
  # /Repl
  # /Core
  # etc.
  # This is currently two levels up from here: ../../
  #
  # This structure should be maintained by the PL megarepo.

  # Parser combinators.
  PLParser = pkgs.haskell.packages.ghcjs.callCabal2nix "PLParser" (srcFilter ../../Parser) {
    inherit PLLabel PLPrinter;
  };

  # Pretty printing.
  PLPrinter = pkgs.haskell.packages.ghcjs.callCabal2nix "PLPrinter" (srcFilter ../../Printer) {
  };

  # Grammars that can be used as parsers/ printers.
  PLGrammar = pkgs.haskell.packages.ghcjs.callCabal2nix "PLGrammar" (srcFilter ../../Grammar) {
    inherit PLLabel Reversible DSL-Compose;
  };

  # Nano-dependency of Parser that should probably be merged or extended.
  PLLabel = pkgs.haskell.packages.ghcjs.callCabal2nix "PLLabel" (srcFilter ../../Label) {
    inherit PLPrinter;
  };

  # Compose DSLs and their interpreters.
  DSL-ComposeSrc = pkgs.fetchFromGitHub {
    owner  = "syallop";
    repo   = "DSL-Compose";
    rev    = "5a95ae855f3291aa3193a7424892035226b10d2f";
    sha256 = "0315xn9g9f6sv69wwa9vdw3lfyv2gzw3bv8qwlv4qb6f76wyhrhr";
  };
  DSL-Compose = pkgs.haskell.packages.ghcjs.callCabal2nix "DSL-Compose" DSL-ComposeSrc {
  };

  # Functions which can be ran forwards as well as backwards.
  ReversibleSrc = pkgs.fetchFromGitHub {
    owner = "syallop";
    repo  = "reversible";
    rev   = "5185a10559f0567ba927ea67a0b90f446d87dbe3";
    sha256 = "1f3ppz7d2gi34hnwgisvimfixcci2ldiwkxxisd54r6bg9dhi2fz";
  };
  Reversible = pkgs.haskell.packages.ghcjs.callCabal2nix "Reversible" ReversibleSrc {
    inherit DSL-Compose;
  };

  # TODO: Switch to a pinned version/ Hackage when the version we need is released.
  filepath-bytestringSrc = builtins.fetchGit {
    url = "git://git.joeyh.name/haskell-filepath-bytestring.git";
    ref = "master";
  };
  filepath-bytestring = pkgs.haskell.packages.ghcjs.callCabal2nix "filepath-bytestring" filepath-bytestringSrc {
  };

  # Hash primitive and composite data as well as hashes themselves into human
  # readable strings which describe their hashing algorithm.
  PLHash = pkgs.haskell.packages.ghcjs.callCabal2nix "PLHash" (srcFilter ../../Hash) {
    inherit PLPrinter;
  };

  # Store interface
  PLStore = pkgs.haskell.packages.ghcjs.callCabal2nix "PLStore" (srcFilter ../../Store/Store) {
    inherit PLHash PLGrammar PLPrinter Reversible filepath-bytestring;
  };
  # Memory backed store
  PLStore-Memory = pkgs.haskell.packages.ghcjs.callCabal2nix "PLStore-Memory" (srcFilter ../../Store/Memory) {
    inherit PLStore PLGrammar PLPrinter Reversible filepath-bytestring;
  };
  # File system backed store
  PLStore-File = pkgs.haskell.packages.ghcjs.callCabal2nix "PLStore-File" (srcFilter ../../Store/File) {
    inherit PLStore PLGrammar PLPrinter PLHash Reversible filepath-bytestring;
  };
  # Layered stores
  PLStore-Nested= pkgs.haskell.packages.ghcjs.callCabal2nix "PLStore-Nested" (srcFilter ../../Store/Nested) {
    inherit PLStore PLGrammar PLPrinter Reversible filepath-bytestring;
  };
  # DHT backed store
  PLStore-DHT = pkgs.haskell.packages.ghcjs.callCabal2nix "PLStore-DHT" (srcFilter ../../Store/DHT) {
    inherit PLStore PLGrammar PLPrinter Reversible;
  };
  # A store where hash(value) is used for keys
  PLStore-Hash= pkgs.haskell.packages.ghcjs.callCabal2nix "PLStore-Hash" (srcFilter ../../Store/Hash) {
    inherit PLStore PLHash PLGrammar PLPrinter Reversible filepath-bytestring;
  };

  # The core data structures and functions of the language
  PL = pkgs.haskell.packages.ghcjs.callCabal2nix "PL" (srcFilter ../../Core) {
    inherit PLGrammar PLPrinter PLHash PLStore PLStore-Hash Reversible filepath-bytestring;
  };

  # Text manipulation. Might not be useful in the JS-world.
  PLEditor = pkgs.haskell.packages.ghcjs.callCabal2nix "PLEditor" (srcFilter ../../Editor) {
  };

  # Lispy Grammar for parsing/ printing.
  PLLispy = pkgs.haskell.packages.ghcjs.callCabal2nix "PLLispy" (srcFilter ../../Lispy) {
    inherit PL PLGrammar PLLabel PLParser PLPrinter PLHash PLStore-Hash Reversible;
  };

  # Core Repl functions, agnostic of the frontend.
  PLRepl = pkgs.haskell.packages.ghcjs.callCabal2nix "PLRepl" (srcFilter ../Lib) {
    inherit PL PLEditor PLGrammar PLLabel PLLispy PLParser PLPrinter PLHash PLStore PLStore-Hash Reversible;
  };
in
{
  # JS front-end to the REPL.
  PLReplJS = pkgs.haskell.packages.ghcjs.callCabal2nix "PLReplJS" (srcFilter ./.) {
    inherit PL PLRepl PLLispy PLEditor PLPrinter PLParser PLGrammar PLHash PLStore PLStore-Hash PLStore-Memory PLStore-File PLStore-Nested Reversible filepath-bytestring;
  };
}
