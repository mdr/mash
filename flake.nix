{
  description = "Mash object shell: the Rust port's `mash` (nix run github:mdr/mash), and a development environment";

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
      # The Rust port's shell, so that `nix run github:mdr/mash` runs it
      packages = forAllSystems (pkgs:
        let
          # The workspace needs a newer Rust than nixpkgs has, so build with rust-overlay's
          toolchain = pkgs.rust-bin.stable.latest.minimal;
          rustPlatform = pkgs.makeRustPlatform { cargo = toolchain; rustc = toolchain; };
          workspace = (builtins.fromTOML (builtins.readFile ./rust/Cargo.toml)).workspace.package;
          mash = rustPlatform.buildRustPackage {
            pname = "mash";
            version = workspace.version;
            src = ./rust;
            cargoLock.lockFile = ./rust/Cargo.lock;
            cargoBuildFlags = [ "-p" "mash-cli" ];
            # The tests need a home directory, git and the network, which the build sandbox doesn't have
            doCheck = false;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            # Mash's git functions run `git`: fall back to Nix's when there's none on the PATH
            postInstall = ''
              wrapProgram $out/bin/mash --suffix PATH : ${pkgs.lib.makeBinPath [ pkgs.git ]}
            '';
            meta = {
              description = "An object shell";
              homepage = "https://mash-shell.org";
              license = pkgs.lib.licenses.mit;
              mainProgram = "mash";
            };
          };
        in
        {
          inherit mash;
          default = mash;
        });

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
