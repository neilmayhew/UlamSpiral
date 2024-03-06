{
  description = "Draw an Ulam Spiral using the Haskell diagrams package";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "ulam-spiral";
      overlay = nix/overlay.nix;
      shell = nix/shell.nix;
    };
}
