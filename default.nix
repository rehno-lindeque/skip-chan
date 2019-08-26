{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "skip-chan";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/rehno-lindeque/skip-chan";
  description = "Skip channel from Concurrent Haskell paper";
  license = stdenv.lib.licenses.bsd3;
}
