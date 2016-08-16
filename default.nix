{ mkDerivation, base, bytestring, diff-parse, highlighter2
, optparse-applicative, pretty-show, process, protolude, stdenv
, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "difftodo";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring diff-parse highlighter2 protolude text
  ];
  executableHaskellDepends = [
    base bytestring optparse-applicative process protolude text
  ];
  testHaskellDepends = [
    base bytestring highlighter2 pretty-show protolude tasty
    tasty-hunit text
  ];
  homepage = "https://github.com/jml/difftodo#readme";
  description = "Generate todo lists from source code";
  license = stdenv.lib.licenses.asl20;
}
