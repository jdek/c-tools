{
  inputs.flakelight.url = "github:nix-community/flakelight";
  outputs = { flakelight, ... }:
    flakelight ./. {
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      devShell.packages = pkgs: with pkgs; [
        chez
        racket-minimal
        guile
        chicken
        gambit
        sbcl
      ];
      devShell.env = {
        CHEZSCHEMELIBDIRS = "./lib::.cache/lib";
      };
    };
}

