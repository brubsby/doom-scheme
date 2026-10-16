{
  description = "DOOM in Chez Scheme";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});
    in {
      devShells = forAll (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [ chez SDL2 gcc gnumake python3 ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.SDL2 ];
        };
      });
    };
}
