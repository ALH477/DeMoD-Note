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

        demod-note = hp.callCabal2nix "DeMoD-Note" self {};

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
      in {
        packages = {
          default = demod-note;
          nixos = wrapped;
          appimage = appimage;
          windows = windows;
          static = pkgs.pkgsMusl.haskellPackages.callCabal2nix "DeMoD-Note" self {};
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
          ];
          shellHook = ''
            export FLUID_SOUNDFONT="${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM.sf2"
          '';
        };
      });
}
