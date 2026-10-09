{
  description = "Mash object shell - development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs:
        let
          # JDK 17: newer JDKs work but warn about native access from jnr/jffi
          jdk = pkgs.jdk17;
        in
        {
          default = pkgs.mkShell {
            packages = [
              jdk
              (pkgs.sbt.override { jre = jdk; })
              pkgs.git
            ];

            JAVA_HOME = jdk.home;
          };
        });
    };
}
