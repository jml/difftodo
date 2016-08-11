{ mkDerivation, base, bytestring, diff-parse, highlighting-kate
, optparse-applicative, protolude, stdenv, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "difftodo";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base diff-parse highlighting-kate protolude text
  ];
  executableHaskellDepends = [
    base optparse-applicative protolude text
  ];
  testHaskellDepends = [
    base bytestring highlighting-kate protolude tasty tasty-hunit text
  ];
  homepage = "https://github.com/jml/difftodo#readme";
  description = "Generate todo lists from source code";
  license = stdenv.lib.licenses.asl20;
}
