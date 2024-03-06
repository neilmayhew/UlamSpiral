{ mkDerivation, base, colour, diagrams-lib, diagrams-svg, lib
, SVGFonts
}:
mkDerivation {
  pname = "UlamSpiral";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base colour diagrams-lib diagrams-svg SVGFonts
  ];
  description = "Draw an Ulam Spiral using the diagrams package";
  license = "unknown";
  mainProgram = "UlamSpiral";
}
