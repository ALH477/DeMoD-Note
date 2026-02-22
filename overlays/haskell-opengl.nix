# Custom Haskell overlay for OpenGL-dependent packages
# Uses haskell-nix to build packages from Hackage with proper system dependencies

{ system }:
let
  # Recent stable LTS (LTS 24.x - February 2025)
  # This pins the Hackage index state for reproducibility
  indexState = "2025-02-01T00:00:00Z";
in
{ nixpkgs ? import <nixpkgs> {} }:
let
  # Import haskell-nix
  haskellNix = (import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}) {
    inherit nixpkgs;
  };
in
{
  # The overlay function - takes pkgs and returns modified haskellPackages
  overlay = pkgs: hpkgs:
    let
      # Get the compiler from hpkgs
      compiler = hpkgs.config.ghc.package;
      compiler-nix-name = "ghc${compiler.version}";
    in
    hpkgs.extend (self: super: {
      # GLFW-b - OpenGL windowing
      # Requires: glfw3
      GLFW-b = haskellNix.hackage-package {
        name = "GLFW-b";
        version = "3.3.9.1";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
      }.components.library;

      # gl - OpenGL bindings
      gl = haskellNix.hackage-package {
        name = "gl";
        version = "0.14.0.2";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
      }.components.library;

      # dear-imgui - Dear ImGui bindings
      dear-imgui = haskellNix.hackage-package {
        name = "dear-imgui";
        version = "2.3.1";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
        # Enable OpenGL3 and GLFW backends
        flags = {
          opengl3 = true;
          glfw = true;
        };
      }.components.library;

      # dear-imgui-glfw - GLFW backend for dear-imgui
      dear-imgui-glfw = haskellNix.hackage-package {
        name = "dear-imgui-glfw";
        version = "2.3.1";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
      }.components.library;

      # dear-imgui-opengl3 - OpenGL3 backend for dear-imgui
      dear-imgui-opengl3 = haskellNix.hackage-package {
        name = "dear-imgui-opengl3";
        version = "2.3.1";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
      }.components.library;

      # nanovg - NanoVG bindings (OpenGL 2D rendering)
      # Requires: freetype2, fontconfig
      nanovg = haskellNix.hackage-package {
        name = "nanovg";
        version = "0.8.1.0";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
        # Enable stb_truetype for font rendering
        flags = {
          stb_truetype = true;
        };
      }.components.library;

      # zmidi-core - MIDI file parsing
      zmidi-core = haskellNix.hackage-package {
        name = "zmidi-core";
        version = "0.8.3.0.0";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
      }.components.library;

      # svg-tree - SVG parsing
      svg-tree = haskellNix.hackage-package {
        name = "svg-tree";
        version = "0.6.5.1";
        index-state = indexState;
        compiler-nix-name = compiler-nix-name;
      }.components.library;
    });

  # System dependencies needed for OpenGL packages
  buildInputs = with pkgs; [
    glfw
    libGL
    libGLU
    freetype
    fontconfig
    glm
    libX11
    libXrandr
  ];

  # Description for documentation
  description = ''
    Haskell overlay for OpenGL-dependent packages:
    - GLFW-b, gl, dear-imgui, nanovg, zmidi-core, svg-tree
    
    Uses haskell-nix with Hackage index state: ${indexState}
  '';
}
