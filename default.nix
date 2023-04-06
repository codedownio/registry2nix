{ mkDerivation, base, containers, filepath, lib
, optparse-applicative, sandwich, string-interpolate, text, time
, tomland, unordered-containers, validation-selective
}:
mkDerivation {
  pname = "registry2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers filepath optparse-applicative sandwich
    string-interpolate text time tomland unordered-containers
    validation-selective
  ];
  executableHaskellDepends = [
    base containers filepath optparse-applicative sandwich
    string-interpolate text time tomland unordered-containers
    validation-selective
  ];
  testHaskellDepends = [
    base containers filepath optparse-applicative sandwich
    string-interpolate text time tomland unordered-containers
    validation-selective
  ];
  homepage = "https://github.com/codedownio/registry2nix#readme";
  license = lib.licenses.bsd3;
  mainProgram = "registry2nix-exe";
}
