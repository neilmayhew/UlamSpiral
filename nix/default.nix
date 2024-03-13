{ mkDerivation, arithmoi, base, colour, diagrams-lib, diagrams-svg
, lib, optparse-applicative, SVGFonts, terminal-size, vector
}:
mkDerivation {
  pname = "UlamSpiral";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    arithmoi base colour diagrams-lib diagrams-svg optparse-applicative
    SVGFonts terminal-size vector
  ];
  description = "Draw an Ulam Spiral using the diagrams package";
  license = "unknown";
  mainProgram = "UlamSpiral";
}
