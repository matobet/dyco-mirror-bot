{ pkgs ? import <nixpkgs> {}, dyco-mirror-bot ? import ./default.nix {} }:

let
  dyco-mirror-bot-exe = dyco-mirror-bot.projectCross.aarch64-multiplatform.hsPkgs.dyco-mirror-bot.components.exes.dyco-mirror-bot-exe;

  pkgsCross = pkgs.pkgsCross.aarch64-multiplatform;

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
  #!${pkgsCross.stdenv.shell}
  $@
  '';
in
pkgsCross.dockerTools.buildImage {
  name = "dyco-mirror-bot";
  tag = "latest";
  contents = [ dyco-mirror-bot-exe pkgsCross.iana-etc pkgsCross.cacert ];
  config = {
    Cmd = [ "${dyco-mirror-bot-exe}/bin/dyco-mirror-bot-exe" ];
    # Entrypoint = [ entrypoint ];
  };
}
