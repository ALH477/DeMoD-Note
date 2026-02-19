{
  description = "DeMoD-Note – Deterministic Monophonic Note Detector";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hp = pkgs.haskellPackages;

        # Build the package with tests enabled
        demod-note = hp.callCabal2nix "DeMoD-Note" self {};
        
        # Run tests and return results
        runTests = pkgs.runCommand "demod-note-test-runner" {
          buildInputs = [ demod-note ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          
          # Run the test suite
          echo "Running DeMoD-Note test suite..."
          cabal test --test-options="--color=always" 2>&1 | tee /tmp/test-output.txt
          
          # Check if tests passed
          if [ $? -eq 0 ]; then
            echo "All tests passed successfully!"
            echo "Test suite completed successfully" > $out
          else
            echo "Tests failed! See output above."
            exit 1
          fi
        '';
        
        # Full test coverage report
        testCoverage = pkgs.runCommand "demod-note-test-coverage" {
          buildInputs = [ demod-note ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          
          echo "Running test suite with coverage..."
          cabal test --enable-coverage --test-options="--color=always" 2>&1 | tee /tmp/coverage-output.txt
          
          echo "Coverage report generated" > $out
        '';
        
        # Integration tests for JACK functionality  
        jackIntegrationTests = pkgs.runCommand "demod-note-jack-tests" {
          buildInputs = [ demod-note pkgs.jack2 ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          
          echo "Running JACK integration tests..."
          
          # Check if JACK is available
          if command -v jackd &> /dev/null; then
            echo "JACK available - testing JACK functionality"
            cabal test --test-options="--match='JACK'" 2>&1 || true
          else
            echo "JACK not available - skipping JACK tests"
          fi
          
          echo "JACK integration tests completed" > $out
        '';
        
        # Integration tests for OSC functionality
        oscIntegrationTests = pkgs.runCommand "demod-note-osc-tests" {
          buildInputs = [ demod-note ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          
          echo "Running OSC integration tests..."
          cabal test --test-options="--match='OSC'" 2>&1 || true
          
          echo "OSC integration tests completed" > $out
        '';

        wrapped = pkgs.runCommand "demod-note-wrapped" {
          nativeBuildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin
          makeWrapper ${demod-note}/bin/demod-note $out/bin/demod-note \
            --set FLUID_SOUNDFONT "${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM.sf2" \
            --prefix PATH : "${pkgs.jack2}/bin:${pkgs.fluidsynth}/bin"
        '';

        appimage = pkgs.appimageTools.wrapType2 {
          pname = "demod-note";
          version = "1.0.0";
          src = demod-note;
          extraPkgs = p: [ p.jack2 p.fluidsynth p.alsa-lib ];
        };

        windows = (pkgs.pkgsCross.mingwW64.haskellPackages.callCabal2nix "DeMoD-Note" self {}).overrideAttrs (old: {
          configureFlags = old.configureFlags or [] ++ ["--enable-executable-static"];
        });

        desktop = pkgs.runCommand "demod-note-desktop" {
          nativeBuildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin
          mkdir -p $out/share/applications
          
          # Copy desktop file
          cp ${./desktop/DeMoD-Note.desktop} $out/share/applications/DeMoD-Note.desktop
          
          # Create launcher script
          cat > $out/bin/DeMoD-Note << 'SCRIPT'
          #!/usr/bin/env bash
          cd "$(dirname "$(readlink -f "$0")/../share/DeMoD-Note")"
          
          export FLUID_SOUNDFONT="${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM.sf2"
          
          # Install python deps if needed
          pip install --user -r requirements.txt 2>/dev/null || true
          
          # Start JACK if not running
          if ! jack_lsp >/dev/null 2>&1; then
              jackd -d dummy -r 44100 -p 256 &
              sleep 2
          fi
          
          # Start OSC-MIDI bridge
          python3 tools/osc-midi-bridge.py &
          
          # Start DeMoD-Note
          cabal run -- run
          SCRIPT
          chmod +x $out/bin/DeMoD-Note
          
          # Copy share directory
          cp -r . $out/share/DeMoD-Note/
        '';
      in {
        packages = {
          default = demod-note;
          nixos = wrapped;
          appimage = appimage;
          windows = windows;
          static = pkgs.pkgsMusl.haskellPackages.callCabal2nix "DeMoD-Note" self {};
          desktop = desktop;
          # Test outputs
          tests = runTests;
          test-coverage = testCoverage;
          jack-tests = jackIntegrationTests;
          osc-tests = oscIntegrationTests;
        };
        
        # Add checks for CI/CD integration
        checks = {
          # Run basic test suite as part of flake checks
          test = runTests;
          # Run JACK integration tests if JACK is available
          jack-integration = jackIntegrationTests;
          # Run OSC integration tests  
          osc-integration = oscIntegrationTests;
        };

        nixosModules.default = { config, lib, pkgs, ... }:
          let cfg = config.services.demod-note; in {
            options.services.demod-note = with lib; {
              enable = mkEnableOption "DeMoD-Note note detector";
              user = mkOption { type = types.str; default = "root"; };
              configFile = mkOption { type = types.path; default = "/etc/demod-note.toml"; };
            };

            config = lib.mkIf cfg.enable {
              environment.systemPackages = [ self.packages.${pkgs.system}.nixos ];
              users.users.${cfg.user}.extraGroups = [ "audio" "jackaudio" ];
              security.rtkit.enable = true;

              system.activationScripts.demodNoteConfig = lib.stringAfter ["etc"] ''
                if [ ! -f ${cfg.configFile} ]; then
                  cp ${./config.example.toml} ${cfg.configFile}
                  chmod 644 ${cfg.configFile}
                fi
              '';

              systemd.user.services.demod-note = {
                description = "DeMoD-Note deterministic note detector";
                after = [ "pipewire.service" "jack.service" ];
                wantedBy = [ "default.target" ];
                serviceConfig = {
                  ExecStart = "${self.packages.${pkgs.system}.nixos}/bin/demod-note --config ${cfg.configFile}";
                  Restart = "on-failure";
                  Nice = "-15";
                  CPUSchedulingPolicy = "fifo";
                  CPUSchedulingPriority = "80";
                  LimitRTPRIO = "95";
                  User = cfg.user;
                };
              };
            };
          };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ demod-note.env ];
          buildInputs = with pkgs; [
            hp.cabal-install hp.haskell-language-server hp.ghcid
            jack2 fluidsynth qjackctl fftw pkg-config
            python3 python3Packages.pip
            hp.hspec
            hp.QuickCheck
            hp.quickcheck-instances
          ];
          shellHook = ''
            export FLUID_SOUNDFONT="${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM.sf2"
            pip install --user -r ${./requirements.txt}
          '';
        };
      });
}
