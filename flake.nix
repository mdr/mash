{
  description = "Mash object shell - development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  inputs.rust-overlay = {
    url = "github:oxalica/rust-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, rust-overlay }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {
        inherit system;
        overlays = [ rust-overlay.overlays.default ];
      }));
    in
    {
      devShells = forAllSystems (pkgs:
        let
          # JDK 17: newer JDKs work but warn about native access from jnr/jffi
          jdk = pkgs.jdk17;
          # Rust toolchain for the port (rust/), including the wasm target for the browser build
          rust = pkgs.rust-bin.stable.latest.default.override {
            extensions = [ "rust-src" "rust-analyzer" ];
            targets = [ "wasm32-unknown-unknown" ];
          };
        in
        {
          default = pkgs.mkShell {
            packages = [
              jdk
              (pkgs.sbt.override { jre = jdk; })
              pkgs.git
              rust
              # The browser build (rust/crates/mash-web): the CLI must match the wasm-bindgen crate's version
              pkgs.wasm-bindgen-cli_0_2_126
              pkgs.binaryen
              pkgs.nodejs
            ];

            JAVA_HOME = jdk.home;
          };
        });
    };
}
