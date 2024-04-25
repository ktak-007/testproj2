{ mkDerivation, base, lib }:
mkDerivation {
  pname = "testproj2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "testproj2";
}
