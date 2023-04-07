{ mkDerivation, aeson, base, bytestring, containers, exceptions
, filepath, lib, monad-logger, mtl, optparse-applicative, process
, sandwich, string-interpolate, text, time, tomland, unliftio
, unliftio-core, unordered-containers, validation-selective, yaml
}:
mkDerivation {
  pname = "registry2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers exceptions filepath monad-logger
    mtl optparse-applicative process sandwich string-interpolate text
    time tomland unliftio unliftio-core unordered-containers
    validation-selective yaml
  ];
  executableHaskellDepends = [
    aeson base bytestring containers exceptions filepath monad-logger
    mtl optparse-applicative process sandwich string-interpolate text
    time tomland unliftio unliftio-core unordered-containers
    validation-selective yaml
  ];
  testHaskellDepends = [
    aeson base bytestring containers exceptions filepath monad-logger
    mtl optparse-applicative process sandwich string-interpolate text
    time tomland unliftio unliftio-core unordered-containers
    validation-selective yaml
  ];
  homepage = "https://github.com/codedownio/registry2nix#readme";
  license = lib.licenses.bsd3;
  mainProgram = "registry2nix-exe";
}
