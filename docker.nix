{ pkgs ? import <nixpkgs> {}, dyco-mirror-bot ? import ./default.nix {} }:

let
  dyco-mirror-bot-exe = dyco-mirror-bot.hsPkgs.dyco-mirror-bot.components.exes.dyco-mirror-bot-exe;

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
  #!${pkgs.stdenv.shell}
  $@
  '';
in
pkgs.dockerTools.buildImage {
  name = "dyco-mirror-bot";
  tag = "latest";
  contents = [ dyco-mirror-bot-exe pkgs.iana-etc pkgs.cacert ];
  config = {
    Cmd = [ "${dyco-mirror-bot-exe}/bin/dyco-mirror-bot-exe" ];
    Entrypoint = [ entrypoint ];
  };
}
