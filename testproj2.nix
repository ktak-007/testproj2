{ mkDerivation, base, bytestring, conduit, lib, parsec, rainbow
, text
}:
mkDerivation {
  pname = "testproj2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring conduit parsec rainbow text
  ];
  license = lib.licenses.mit;
  mainProgram = "testproj2";
}
