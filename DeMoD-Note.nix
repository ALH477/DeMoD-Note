{ mkDerivation, aeson, aeson-pretty, array, async, base, binary
, brick, bytestring, containers, deepseq, directory
, explicit-exception, fftw, filepath, hosc, hspec, jack, katip, lib
, libjack2, mtl, network, optparse-applicative, process, QuickCheck
, quickcheck-instances, random, scotty, stm, text, time, tomland
, transformers, unix, vector, vector-fftw, vty, vty-crossplatform
}:
mkDerivation {
  pname = "DeMoD-Note";
  version = "1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty array async base binary brick bytestring
    containers deepseq directory explicit-exception filepath hosc jack
    katip mtl network optparse-applicative process random scotty stm
    text time tomland transformers unix vector vector-fftw vty
    vty-crossplatform
  ];
  libraryPkgconfigDepends = [ fftw libjack2 ];
  executableHaskellDepends = [
    async base brick directory filepath katip optparse-applicative
    process stm text time vty vty-crossplatform
  ];
  testHaskellDepends = [
    base hosc hspec process QuickCheck quickcheck-instances stm vector
  ];
  homepage = "https://github.com/ALH477/DeMoD-Note";
  description = "Deterministic Monophonic Note Detector";
  license = lib.licensesSpdx."MIT";
}
