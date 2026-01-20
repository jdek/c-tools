{
  inputs.flakelight.url = "github:nix-community/flakelight";
  outputs = { flakelight, ... }:
    flakelight ./. {
      devShell.packages = pkgs: with pkgs; [ akkuPackages.scheme-langserver chez ];
      devShell.env = {
        CHEZSCHEMELIBDIRS = "./lib";
      };
    };
}

